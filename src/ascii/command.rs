//! Types and traits for generating ASCII commands.

use crate::ascii::{checksum::Lrc, id, Target};
use std::io;

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
        Lrc::hash(buf.as_ref())
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

    /// Get the target of the command.
    pub(crate) fn get_target(&self) -> Target {
        self.target
    }

    /// Create a instance of this command, which can be sent over the serial port.
    pub(crate) fn instance<G: id::Generator>(
        &self,
        mut generator: G,
        generate_id: bool,
        generate_checksum: bool,
    ) -> CommandInstance<'_> {
        CommandInstance {
            target: self.target,
            id: if generate_id {
                Some(generator.next_id())
            } else {
                None
            },
            data: &self.data,
            checksum: generate_checksum,
        }
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

        let cmd = "get maxspeed".target_all();
        cmd.instance(&mut generator, false, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/get maxspeed:49\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        "get maxspeed"
            .target(2)
            .instance(&mut generator, true, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/2 0 5 get maxspeed:52\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        "".target_all()
            .instance(&mut generator, false, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/:00\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        "".target(1)
            .instance(&mut generator, false, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/1:CF\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );
    }

    #[test]
    fn test_command_checksum() {
        let mut buf = vec![];
        "tools echo"
            .target(1)
            .instance(ConstId {}, false, true)
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
