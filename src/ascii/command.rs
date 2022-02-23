//! Types and traits for generating ASCII commands.

use crate::ascii::{checksum::Lrc, id, Target};
use std::io;

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
pub trait Command {
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

impl Command for CommandBuilder {
    type Ref = CommandBuilder;

    fn as_ref(&self) -> &Self::Ref {
        self
    }
    fn get_target(&self) -> Target {
        self.target
    }
    fn get_data(&self) -> &[u8] {
        self.data.as_slice()
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

/// An instance of a [`CommandBuilder`] that can be sent over the serial port.
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

/// A builder for a Zaber ASCII command.
///
/// Create a command using either the [`empty`](CommandBuilder::empty) or
/// [`new`](CommandBuilder::new) methods. By default the command will target all
/// devices and only include message IDs or checksums if the port it is sent
/// on is configured to do so. To target a specific device and/or axis, use the
/// [`target`](CommandBuilder::target) method.
///
/// ## Example
///
/// Build a command that will send `/1 2 home`. The message ID and checksum will
/// be generated if the port the command is sent on is configured to do so (the
/// default):
///
/// ```
/// # use zproto::ascii::CommandBuilder;
/// let cmd = CommandBuilder::new("home").target((1, 2));
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct CommandBuilder {
    /// The targeted device or axis.
    target: Target,
    /// The command data.
    data: Vec<u8>,
}

impl CommandBuilder {
    /// Create the empty command (i.e. `/`).
    pub const fn empty() -> CommandBuilder {
        CommandBuilder {
            target: Target::all(),
            data: Vec::new(),
        }
    }

    /// Create a command with the given data.
    ///
    /// The command header (address, axis, and message ID) should not be included.
    /// By default the command is addressed to all devices and axes, and uses the [`Port`](crate::ascii::Port)'s preference for ID and checksum.
    pub fn new<T: AsRef<[u8]>>(data: T) -> CommandBuilder {
        CommandBuilder {
            target: Target::all(),
            data: data.as_ref().to_vec(),
        }
    }

    /// Set the device address and, optionally, axis number to send the command to.
    pub fn target<T: Into<Target>>(&mut self, target: T) -> &mut Self {
        self.target = target.into();
        self
    }
}

impl AsRef<CommandBuilder> for CommandBuilder {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl Default for CommandBuilder {
    /// Create the default command, which is the empty (`/`) command.
    fn default() -> CommandBuilder {
        CommandBuilder::empty()
    }
}

/// A trait to convert a type into the body of an ASCII [`CommandBuilder`].
///
/// ```rust
/// # use zproto::ascii::CommandBuilder;
/// use zproto::ascii::IntoCommand as _;
///
/// let command: CommandBuilder = "get maxspeed".target(1);
/// ```
pub trait IntoCommand {
    /// Create a command targeting `target` with this type as the command's data
    fn target<T: Into<Target>>(&self, target: T) -> CommandBuilder;
    /// Create a command targeting all devices with this type as the command's data
    fn target_all(&self) -> CommandBuilder;
}

impl IntoCommand for [u8] {
    fn target<T: Into<Target>>(&self, target: T) -> CommandBuilder {
        let mut cmd = CommandBuilder::new(self);
        cmd.target(target);
        cmd
    }
    fn target_all(&self) -> CommandBuilder {
        CommandBuilder::new(self)
    }
}

impl<T> IntoCommand for T
where
    T: AsRef<[u8]>,
{
    fn target<S: Into<Target>>(&self, target: S) -> CommandBuilder {
        self.as_ref().target(target)
    }
    fn target_all(&self) -> CommandBuilder {
        self.as_ref().target_all()
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
    fn test_into_command_data() {
        let mut expected = CommandBuilder::new("get maxspeed");
        expected.target(1);
        assert_eq!(
            "get maxspeed".target((1, 2)),
            *CommandBuilder::new("get maxspeed").target((1, 2))
        );
        assert_eq!("get maxspeed".target(1), expected);
        assert_eq!("get maxspeed".to_string().target(1), expected);
        assert_eq!(b"get maxspeed".target(1), expected);
        assert_eq!(b"get maxspeed".to_vec().target(1), expected);
    }

    #[test]
    fn test_write_command() {
        let mut generator = ConstId {};

        let mut buf = Vec::new();

        CommandInstance::new(&"get maxspeed".target_all(), &mut generator, false, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/get maxspeed:49\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        CommandInstance::new(&"get maxspeed".target(2), &mut generator, true, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/2 0 5 get maxspeed:52\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        CommandInstance::new(&"".target_all(), &mut generator, false, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/:00\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        CommandInstance::new(&"".target(1), &mut generator, false, true)
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
