//! Types and traits for generating ASCII commands.

use crate::ascii::{checksum::LrcWriter, id, Target};
use std::borrow::Cow;
use std::io;

mod private {
    use super::*;
    pub trait Sealed {}

    impl Sealed for str {}
    impl Sealed for String {}
    impl Sealed for [u8] {}
    impl<const N: usize> Sealed for [u8; N] {}
    impl Sealed for Vec<u8> {}
    impl<T, D> Sealed for (T, D)
    where
        T: Into<Target> + Copy,
        D: AsRef<[u8]>,
    {
    }
    impl<D: AsRef<[u8]>> Sealed for (u8, u8, D) {}
    impl<T> Sealed for &T where T: Sealed + ?Sized {}
    impl<T> Sealed for &mut T where T: Sealed + ?Sized {}
    impl<T> Sealed for Box<T> where T: Sealed + ?Sized {}
}

/// A trait that is implemented by all ASCII commands.
///
/// Multiple types implement `Command` including:
///
/// * `str`,
/// * `String`,
/// * `[u8; N]`,
/// * `[u8]`,
/// * `Vec<u8>`,
/// * and any `&T`, `&mut T`, or `Box<T>` where `T` implements `Command`.
///
/// When used as a command the bytes in these types will be interpreted as the
/// data for a command and targeted to all devices and axes in the chain. For
/// instance, the following are examples of how the above types are converted
/// into ASCII commands:
///
/// * `"home"` → `"/home\n"`
/// * `format!("move abs {}", 12345)` → `"/move abs 12345\n"`
///
/// To target specific devices and/or axes, prepend any type convertible to a
/// [`Target`] to the data as part of a tuple:
///
/// * `(1, "home")` → `"/1 home\n"`
/// * `((1, 2), format!("move abs {}", 12345))` → `"/1 2 move abs 12345\n"`
///
/// For convenience, tuples of the form `(u8, u8, data)` are also supported:
///
/// * `(1, 2, "home")` → `"/1 2 home\n"`
///
/// Note, whether message IDs and/or checksums are generated for each command
/// is defined by the [`Port`](crate::ascii::Port). By default they are both
/// enabled.
pub trait Command: private::Sealed {
    /// The type returned when calling `as_ref`, below.
    type Ref: Command + ?Sized;

    /// Get a reference to the command.
    ///
    /// This is roughly equivalent to `AsRef` except that it is not generic over
    /// the return type, which allows this trait to also not be generic.
    /// Including it as part of the trait directly rather than a supertrait
    /// avoids requiring call sites to add a somewhat complicated `AsRef` bound.
    fn as_ref(&self) -> &Self::Ref;
    /// Get the command's target.
    fn target(&self) -> Target;
    /// Get the command's data.
    fn data(&self) -> Cow<[u8]>;
}

impl Command for str {
    type Ref = str;

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn target(&self) -> Target {
        Target::for_all()
    }
    fn data(&self) -> Cow<[u8]> {
        self.as_bytes().into()
    }
}

impl Command for [u8] {
    type Ref = [u8];

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn target(&self) -> Target {
        Target::for_all()
    }
    fn data(&self) -> Cow<[u8]> {
        self.into()
    }
}

impl<const N: usize> Command for [u8; N] {
    type Ref = [u8; N];

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn target(&self) -> Target {
        Target::for_all()
    }
    fn data(&self) -> Cow<[u8]> {
        self.as_slice().into()
    }
}

impl Command for String {
    type Ref = [u8];

    fn as_ref(&self) -> &Self::Ref {
        self.as_bytes()
    }
    fn target(&self) -> Target {
        Target::for_all()
    }
    fn data(&self) -> Cow<[u8]> {
        self.as_bytes().into()
    }
}

impl Command for Vec<u8> {
    type Ref = [u8];

    fn as_ref(&self) -> &Self::Ref {
        self.as_slice()
    }
    fn target(&self) -> Target {
        Target::for_all()
    }
    fn data(&self) -> Cow<[u8]> {
        self.as_slice().into()
    }
}

impl<T, D> Command for (T, D)
where
    T: Into<Target> + Copy,
    D: AsRef<[u8]>,
{
    type Ref = (T, D);

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn target(&self) -> Target {
        self.0.into()
    }
    fn data(&self) -> Cow<[u8]> {
        self.1.as_ref().into()
    }
}

impl<D> Command for (u8, u8, D)
where
    D: AsRef<[u8]>,
{
    type Ref = (u8, u8, D);

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn target(&self) -> Target {
        Target::for_device(self.0).with_axis(self.1)
    }
    fn data(&self) -> Cow<[u8]> {
        self.2.as_ref().into()
    }
}

impl<T> Command for &T
where
    T: Command + ?Sized,
{
    type Ref = T;

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn target(&self) -> Target {
        (**self).target()
    }
    fn data(&self) -> Cow<[u8]> {
        (**self).data()
    }
}

impl<T> Command for &mut T
where
    T: Command + ?Sized,
{
    type Ref = T;

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn target(&self) -> Target {
        (**self).target()
    }
    fn data(&self) -> Cow<[u8]> {
        (**self).data()
    }
}

impl<T> Command for Box<T>
where
    T: Command + ?Sized,
{
    type Ref = T;

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn target(&self) -> Target {
        (**self).target()
    }
    fn data(&self) -> Cow<[u8]> {
        (**self).data()
    }
}

/// An instance of an ASCII command that can be sent over the serial port.
pub(crate) struct CommandInstance<'a> {
    /// The targeted device/axis
    pub target: Target,
    /// The message ID
    pub id: Option<u8>,
    /// The command data
    pub data: Cow<'a, [u8]>,
    /// Whether to generate a checksum
    pub checksum: bool,
}

impl<'a> CommandInstance<'a> {
    /// Create a instance of this command, which can be sent over the serial port.
    pub fn new<C, G>(
        command: &'a C,
        mut generator: G,
        generate_id: bool,
        generate_checksum: bool,
    ) -> CommandInstance<'a>
    where
        C: Command,
        G: id::Generator,
    {
        CommandInstance {
            target: command.target(),
            id: if generate_id {
                Some(generator.next_id())
            } else {
                None
            },
            data: command.data(),
            checksum: generate_checksum,
        }
    }

    /// Write the command packet into the specified writer.
    pub fn write_into<W: io::Write + ?Sized>(&self, writer: &mut W) -> io::Result<()> {
        use std::io::Write as _;

        write!(writer, "/")?;
        let writer = &mut LrcWriter::new(writer);

        // Only output the address, axis or message ID if it is necessary.
        let wrote = match self.id {
            Some(id) => {
                write!(
                    writer,
                    "{} {} {}",
                    self.target.device(),
                    self.target.axis(),
                    id
                )?;
                true
            }
            None => {
                if self.target.axis() != 0 {
                    write!(writer, "{} {}", self.target.device(), self.target.axis())?;
                    true
                } else if self.target.device() != 0 {
                    write!(writer, "{}", self.target.device())?;
                    true
                } else {
                    false
                }
            }
        };

        if !self.data.is_empty() {
            // If necessary add a delimiting whitespace
            if wrote {
                write!(writer, " ")?;
            }
            writer.write_all(self.data.as_ref())?;
        }

        if self.checksum {
            let checksum = writer.finish_hash();
            write!(writer, ":{:02X}", checksum)?;
        }
        writeln!(writer)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct ConstId {}
    impl id::Generator for ConstId {
        fn next_id(&mut self) -> u8 {
            5
        }
    }

    #[test]
    fn test_target() {
        assert_eq!(Target::default(), Target::for_all());
        assert_eq!(Target::for_device(1), Target(1, 0));
        assert_eq!(Target::for_device(1).with_axis(1), Target(1, 1));
        assert_eq!(Target::new(5, 9).with_all_axes(), Target(5, 0));
    }

    #[test]
    fn test_write_command() {
        let mut generator = ConstId {};

        let mut buf = Vec::new();

        CommandInstance::new(&"get maxspeed", &mut generator, false, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/get maxspeed:49\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        CommandInstance::new(&(2, "get maxspeed"), &mut generator, true, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/2 0 5 get maxspeed:52\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        CommandInstance::new(&"", &mut generator, false, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/:00\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        CommandInstance::new(&(1, ""), &mut generator, false, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/1:CF\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        CommandInstance::new(&(1, 2, "tools echo bob"), &mut generator, false, false)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/1 2 tools echo bob\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );
    }

    #[test]
    fn test_command_checksum() {
        let mut buf = vec![];
        CommandInstance::new(&(1, "tools echo"), ConstId {}, false, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/1 tools echo:BF\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );
    }
}
