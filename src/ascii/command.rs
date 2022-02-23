//! Types and traits for generating ASCII commands.

use crate::ascii::{checksum::Lrc, id, Target};
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
    fn get_target(&self) -> Target;
    /// Get the command's data.
    fn get_data(&self) -> &[u8];
}

impl Command for str {
    type Ref = str;

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn get_target(&self) -> Target {
        Target::all()
    }
    fn get_data(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl Command for [u8] {
    type Ref = [u8];

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn get_target(&self) -> Target {
        Target::all()
    }
    fn get_data(&self) -> &[u8] {
        self
    }
}

impl<const N: usize> Command for [u8; N] {
    type Ref = [u8; N];

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn get_target(&self) -> Target {
        Target::all()
    }
    fn get_data(&self) -> &[u8] {
        self
    }
}

impl Command for String {
    type Ref = [u8];

    fn as_ref(&self) -> &Self::Ref {
        self.as_bytes()
    }
    fn get_target(&self) -> Target {
        Target::all()
    }
    fn get_data(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl Command for Vec<u8> {
    type Ref = [u8];

    fn as_ref(&self) -> &Self::Ref {
        self.as_slice()
    }
    fn get_target(&self) -> Target {
        Target::all()
    }
    fn get_data(&self) -> &[u8] {
        self.as_slice()
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
    fn get_target(&self) -> Target {
        self.0.into()
    }
    fn get_data(&self) -> &[u8] {
        self.1.as_ref()
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
    fn get_target(&self) -> Target {
        Target::device(self.0).axis(self.1)
    }
    fn get_data(&self) -> &[u8] {
        self.2.as_ref()
    }
}

impl<T> Command for &T
where
    T: Command + ?Sized,
{
    type Ref = T;

    fn as_ref(&self) -> &Self::Ref {
        *self
    }
    fn get_target(&self) -> Target {
        (**self).get_target()
    }
    fn get_data(&self) -> &[u8] {
        (**self).get_data()
    }
}

impl<T> Command for &mut T
where
    T: Command + ?Sized,
{
    type Ref = T;

    fn as_ref(&self) -> &Self::Ref {
        *self
    }
    fn get_target(&self) -> Target {
        (**self).get_target()
    }
    fn get_data(&self) -> &[u8] {
        (**self).get_data()
    }
}

impl<T> Command for Box<T>
where
    T: Command + ?Sized,
{
    type Ref = T;

    fn as_ref(&self) -> &Self::Ref {
        &*self
    }
    fn get_target(&self) -> Target {
        (**self).get_target()
    }
    fn get_data(&self) -> &[u8] {
        (**self).get_data()
    }
}

/// An instance of an ASCII command that can be sent over the serial port.
pub(crate) struct CommandInstance<'a> {
    /// The targeted device/axis
    pub target: Target,
    /// The message ID
    pub id: Option<u8>,
    /// The command data
    pub data: &'a [u8],
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
            target: command.get_target(),
            id: if generate_id {
                Some(generator.next_id())
            } else {
                None
            },
            data: command.get_data(),
            checksum: generate_checksum,
        }
    }

    /// Write the contents of the command packet into writer.
    ///
    /// This is useful for calculating the checksum for the command and for
    /// writing the full packet.
    fn write_contents_into<W: io::Write + ?Sized>(&self, writer: &mut W) -> io::Result<()> {
        // Only output the address, axis or message ID if it is necessary.
        let wrote = match self.id {
            Some(id) => {
                write!(
                    writer,
                    "{} {} {}",
                    self.target.get_device(),
                    self.target.get_axis(),
                    id
                )?;
                true
            }
            None => {
                if self.target.get_axis() != 0 {
                    write!(
                        writer,
                        "{} {}",
                        self.target.get_device(),
                        self.target.get_axis()
                    )?;
                    true
                } else if self.target.get_device() != 0 {
                    write!(writer, "{}", self.target.get_device())?;
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
            writer.write_all(self.data)?;
        }
        Ok(())
    }

    /// Get the checksum of the command if it were sent now.
    ///
    /// Ignores whether the checksum is actually enabled via `checksum`.
    fn checksum(&self) -> u32 {
        let mut buf = Vec::with_capacity(80);
        self.write_contents_into(&mut buf).unwrap();
        Lrc::hash(&buf[..])
    }

    /// Write the command packet into the specified writer.
    pub fn write_into<W: io::Write + ?Sized>(&self, writer: &mut W) -> io::Result<()> {
        write!(writer, "/")?;
        self.write_contents_into(writer)?;

        if self.checksum {
            write!(writer, ":{:02X}", self.checksum())?;
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
        assert_eq!(Target::default(), Target::all());
        assert_eq!(Target::device(1), Target(1, 0));
        assert_eq!(Target::device(1).axis(1), Target(1, 1));
        assert_eq!(Target::new(5, 9).all_axes(), Target(5, 0));
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
