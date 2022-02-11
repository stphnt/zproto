//! Types and traits for generating ASCII commands.

use crate::ascii::{checksum::Lrc, id, Target};
use std::io;

/// A concrete instance of a message ID (e.g., what would be sent over the serial port)
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub(crate) enum IdInstance {
    /// The specified message ID
    Some(u8),
    /// No message ID
    None,
    /// The special marker (`--`) indicating no reply should be sent to the command.
    NoReply,
}

impl std::convert::From<IdInstance> for Option<u8> {
    fn from(other: IdInstance) -> Self {
        match other {
            IdInstance::Some(value) => Some(value),
            _ => None,
        }
    }
}

/// An instance of a [`Command`] that can be sent over the serial port.
pub(crate) struct CommandInstance<'a> {
    /// The targetted device/axis
    pub target: Target,
    /// The message ID
    pub id: IdInstance,
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
            IdInstance::Some(id) => {
                write!(
                    writer,
                    "{} {} {}",
                    self.target.get_address(),
                    self.target.get_axis(),
                    id
                )?;
                true
            }
            IdInstance::NoReply => {
                write!(
                    writer,
                    "{} {} --",
                    self.target.get_address(),
                    self.target.get_axis()
                )?;
                true
            }
            IdInstance::None => {
                if self.target.get_axis() != 0 {
                    write!(
                        writer,
                        "{} {}",
                        self.target.get_address(),
                        self.target.get_axis()
                    )?;
                    true
                } else if self.target.get_address() != 0 {
                    write!(writer, "{}", self.target.get_address())?;
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

    /// Write the command packet into the specified writer
    pub fn write_into<W: io::Write + ?Sized>(&self, writer: &mut W) -> io::Result<()> {
        write!(writer, "/")?;
        self.write_contents_into(writer)?;

        if self.checksum {
            write!(writer, ":{:02X}", self.checksum())?;
        }
        writeln!(writer)
    }
}

/// The way in which a command should instantiate a message ID.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Id {
    /// A newly generated message ID should be used.
    Generate,
    /// No message ID should be included.
    None,
    /// The special marker (`--`) indicating no reply should be use.
    NoReply,
}

impl Default for Id {
    fn default() -> Self {
        Id::Generate
    }
}

/// An Zaber ASCII command
#[derive(Debug, Clone, PartialEq)]
pub struct Command {
    /// The targeted device or axis
    target: Target,
    /// The override for how to instantiate the message ID, if any
    id: Option<Id>,
    /// The command data
    data: Vec<u8>,
    /// The override for whether to generate a checksum or not, if any
    checksum: Option<bool>,
}

impl Command {
    /// Create the empty command (i.e. `/`)
    pub const fn empty() -> Command {
        Command {
            target: Target::all(),
            id: None,
            data: Vec::new(),
            checksum: None,
        }
    }

    /// Create a command with the given data.
    ///
    /// The command header (address, axis, and message ID) should not be included.
    /// By default the command is addressed to all devices and axes, and uses the [`Port`](crate::ascii::Port)'s preference for ID and checksum.
    pub fn new<T: AsRef<[u8]>>(data: T) -> Command {
        Command {
            target: Target::all(),
            id: None,
            data: data.as_ref().to_vec(),
            checksum: None,
        }
    }

    /// Set how the command's message ID should be instantiated.
    ///
    /// This overrides any default the [`Port`](crate::ascii::Port) specifies.
    pub fn id(&mut self, id: Id) -> &mut Self {
        self.id = Some(id);
        self
    }

    /// Defer to the [`Port`](crate::ascii::Port)'s default method of instantiating the message ID.
    pub fn id_default(&mut self) -> &mut Self {
        self.id = None;
        self
    }

    /// Set whether or not the command should include a checksum.
    ///
    /// This overrides any default the [`Port`](crate::ascii::Port) specifies.
    pub fn checksum(&mut self, checksum: bool) -> &mut Self {
        self.checksum = Some(checksum);
        self
    }

    /// Defer to the [`Port`](crate::ascii::Port)'s default checksum.
    pub fn checksum_default(&mut self) -> &mut Self {
        self.checksum = None;
        self
    }

    /// Set the device address and, optionally, axis number to send the command to.
    pub fn to<T: Into<Target>>(&mut self, target: T) -> &mut Self {
        self.target = target.into();
        self
    }

    /// Address the command to all devices.
    #[allow(clippy::wrong_self_convention)]
    pub fn to_all(&mut self) -> &mut Self {
        self.target = Target::all();
        self
    }

    /// Set the command data
    pub fn data<T: AsRef<[u8]>>(&mut self, data: T) -> &mut Self {
        self.data.clear();
        self.data.extend_from_slice(data.as_ref());
        self
    }

    /// Get the target of the command
    pub(crate) fn target(&self) -> Target {
        self.target
    }

    /// Create a instance of this command, which can be sent over the serial port.
    pub(crate) fn instance<G: id::Generator>(
        &self,
        mut generator: G,
        id_default: Id,
        checksum_default: bool,
    ) -> CommandInstance<'_> {
        CommandInstance {
            target: self.target,
            id: match self.id.unwrap_or(id_default) {
                Id::Generate => IdInstance::Some(generator.next_id(self)),
                Id::None => IdInstance::None,
                Id::NoReply => IdInstance::NoReply,
            },
            data: &self.data,
            checksum: self.checksum.unwrap_or(checksum_default),
        }
    }
}

impl AsRef<Command> for Command {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl Default for Command {
    /// Create the default command, which is the empty (`/`) command.
    fn default() -> Command {
        Command::empty()
    }
}

/// A trait to convert a type into the body of an ASCII [`Command`].
///
/// ```rust
/// # use zproto::ascii::Command;
/// use zproto::ascii::IntoCommand as _;
///
/// let command: Command = "get maxspeed".to(1);
/// ```
pub trait IntoCommand {
    /// Create a command targetting `target` with this type as the command's data
    fn to<T: Into<Target>>(&self, target: T) -> Command;
    /// Create a command targetting all devices with this type as the command's data
    fn to_all(&self) -> Command;
}

impl IntoCommand for [u8] {
    fn to<T: Into<Target>>(&self, target: T) -> Command {
        let mut cmd = Command::new(self);
        cmd.to(target);
        cmd
    }
    fn to_all(&self) -> Command {
        Command::new(self)
    }
}

impl<T> IntoCommand for T
where
    T: AsRef<[u8]>,
{
    fn to<S: Into<Target>>(&self, target: S) -> Command {
        self.as_ref().to(target)
    }
    fn to_all(&self) -> Command {
        self.as_ref().to_all()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct ConstId {}
    impl id::Generator for ConstId {
        fn next_id(&mut self, _: &Command) -> u8 {
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
        let mut expected = Command::new("get maxspeed");
        expected.to(1);
        assert_eq!(
            "get maxspeed".to((1, 2)),
            *Command::new("get maxspeed").to((1, 2))
        );
        assert_eq!("get maxspeed".to(1), expected);
        assert_eq!("get maxspeed".to_string().to(1), expected);
        assert_eq!(b"get maxspeed".to(1), expected);
        assert_eq!(b"get maxspeed".to_vec().to(1), expected);
    }

    #[test]
    fn test_write_command() {
        let mut generator = ConstId {};

        let mut buf = Vec::new();

        let cmd = "get maxspeed".to_all();
        cmd.instance(&mut generator, Id::None, true)
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
            .to(2)
            .id(Id::Generate)
            .instance(&mut generator, Id::None, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/2 0 5 get maxspeed:52\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        "".to_all()
            .instance(&mut generator, Id::None, true)
            .write_into(&mut buf)
            .unwrap();
        assert_eq!(
            buf,
            b"/:00\n",
            "Unexpectedly got `{}`",
            std::str::from_utf8(&buf).unwrap()
        );

        buf.clear();
        "".to(1)
            .instance(&mut generator, Id::None, true)
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
            .to(1)
            .checksum(true)
            .instance(ConstId {}, Id::None, false)
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
