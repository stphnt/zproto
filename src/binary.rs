//! Types and traits for communicating with Zaber products with Zaber's [Binary protocol](https://www.zaber.com/protocol-manual?protocol=Binary).
//!
//! The binary protocol is **deprecated** and Zaber recommends the [`ascii`](crate::ascii) protocol instead.
//!
//! ## Communicating with a Device
//!
//! All communication with Zaber products starts with a [`Port`], which can be either a serial or TCP port:
//!
//! ```
//! # use zproto::binary::Port;
//! # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
//! let mut port = Port::open_serial("/dev/ttyUSB0")?;
//! // OR
//! let mut port = Port::open_tcp("192.168.0.1:55550")?;
//! # Ok(())
//! # }
//! ```
//!
//! You can then transmit a command to and receive a reply from devices
//! connected to that port with the [`tx_rx`](Port::tx_rx) method, or any of the
//! other `tx_rx*` methods. Commands are constructed as tuples:
//! * Commands that do not require data take the form `(u8, Command)`, where
//!   * `u8` is the device address and
//!   * `Command` is some type that implements [`Command`](traits::Command) (the
//!     constants in the [`command`] module are highly recommended but you can
//!     also use a `u8`).
//!
//! ```
//! # use zproto::{
//! #     binary::Port,
//! #     backend::Backend,
//! # };
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! // Send the Home command to all devices (address 0)
//! use zproto::binary::command::HOME;
//! let reply = port.tx_rx((0, HOME))?;
//! # Ok(())
//! # }
//! ```
//!
//! * Commands that do require data take a form similar to above, `(u8, Command, Data)`,
//! where `u8` and `Command` are the same and `Data` is some type that implements
//! [`Data`](traits::Data). This includes types like `i32`, `bool`, [`command`],
//! and other types.
//!
//! ```
//! # use zproto::{
//! #     binary::Port,
//! #     backend::Backend,
//! # };
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! // Move device 1 to the absolute position 10,000.
//! use zproto::binary::command::*;
//! let reply = port.tx_rx((1, MOVE_ABSOLUTE, 10000))?;
//! // OR
//! let reply = port.tx_rx((0, RETURN_SETTING, SET_TARGET_SPEED))?;
//! # Ok(())
//! # }
//! ```
//!
//! If the response is an [`ERROR`](command::ERROR) or is otherwise unexpected
//! (e.g., from a different device) an error will be returned.
//!
//! **NOTE**: Address aliases other than 0 (all devices) are presently not
//! supported. Responses to such messages will be treated as unexpected by the
//! [`tx_rx*`](Port::tx_rx) methods.
//!
//! ## Other `Port` Methods
//!
//! [`tx_rx`](Port::tx_rx) works great when you need to send one command and
//! receive one response. But, when you have a chain of devices, you may need to
//! read a response from all devices in the chain. When you know the number of
//! devices in the chain, you can use [`tx_rx_n`](Port::tx_rx_n) to read `n`
//! responses to a command. If you don't know the number of devices in the chain
//! you can use [`tx_rx_until_timeout`](Port::tx_rx_until_timeout) to read as
//! many responses as possible.
//!
//! Sometimes you only want to transmit or receive data, but not both. In those
//! cases the [`tx`](Port::tx), [`rx`](Port::rx), [`rx_n`](Port::rx_n),
//! and [`rx_until_timeout`](Port::rx_until_timeout) are very helpful.
//!
//! Finally, a common pattern is to poll a device until it is in a desired
//! state, which you can do with the [`poll_until`](Port::poll_until) and
//! [`poll_until_idle`](Port::poll_until_idle) methods.
//!
//! ## Reading Data
//!
//! Reading data from a response is as simple as calling [`data()`](DeviceMessage::data)
//! on the response. If the command was sent using the strongly typed commands
//! in the [`command`] module, the library will pick the correct type conversion
//! for you at compile time.
//!
//! ```
//! # use zproto::{
//! #     binary::Port,
//! #     backend::Backend,
//! # };
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! use zproto::binary::command::*;
//! let reply = port.tx_rx((0, RETURN_SETTING, SET_TARGET_SPEED))?;
//! let speed = reply.data()?; // `speed` is an `i32`
//! let reply = port.tx_rx((0, RETURN_SETTING, SET_HOME_STATUS))?;
//! let homed = reply.data()?;  // `homed` is a `bool`.
//! # Ok(())
//! # }
//! ```
//!
//! ## Type Safety
//!
//! The library uses Rust's type system and the strongly-typed commands in the
//! [`command`] module to help enforce proper message structure and avoid
//! run-time bugs. For instance, not passing data to commands that require data,
//! or passing the wrong data type, will result in a compile time error.
//!
//! ```compile_fail
//! # use zproto::{
//! #     binary::Port,
//! #     backend::Backend,
//! # };
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! use zproto::binary::command::*;
//! let reply = port.tx_rx((0, MOVE_ABSOLUTE))?;  // ERROR: the data field is missing!
//! let reply = port.tx_rx((0, MOVE_ABSOLUTE, true))?;  // ERROR: the data has the incorrect type!
//! let reply = port.tx_rx((0, RESET));  // ERROR: Devices do not respond to the RESET command!
//! # Ok(())
//! # }
//! ```
//!
//! To do this, each command in the [`command`] module is given a unique type
//! that implement a set of [`traits`]. These tell the compiler what kind of
//! data the command takes, what kind of data it returns, and if it returns a
//! response at all. However, this means that the commands cannot be swapped out
//! for one another at run time or stored in a collection.
//!
//! ```compile_fail
//! // This will not compile because SET_TARGET_SPEED and SET_ACCELERATION
//! // are different types.
//! use zproto::binary::command::*;
//! let commands = [(0, SET_TARGET_SPEED, 10000), (0, SET_ACCELERATION, 5000)];
//! ```
//!
//! To accommodate situations like this, the library provides "untyped" versions
//! of the command in the [`command::untyped`] module. The commands in that
//! module are simply `u8`s and can be used anywhere their strongly-typed
//! counterparts can.
//!
//! ```
//! // This works now.
//! use zproto::binary::command::untyped::*;
//! let commands = [(0, SET_TARGET_SPEED, 10000), (0, SET_ACCELERATION, 5000)];
//! ```
//!
//! However, you will not have any of the nice compile time type safety or
//! automatic type conversion -- you will have to pick the types yourself.
//!
//! ```
//! # use zproto::{
//! #     binary::Port,
//! #     backend::Backend,
//! # };
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! use zproto::binary::command::untyped::*;
//! let reply = port.tx_rx((0, RETURN_SETTING, SET_TARGET_SPEED))?;
//! let speed: i32 = reply.data()?; // Notice that the type must be specified.
//! // The compiler cannot tell if you have picked the wrong type so this will
//! // compile, but the data conversion will fail at run time.
//! let speed: bool = reply.data()?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Handling Compile-time Errors
//!
//! The traits in this crate are named so that their meaning is hopefully clear,
//! but, nevertheless, the compiler errors can sometimes be confusing. Here are
//! some errors that you might see and suggestions for correcting them.
//!
//! #### `TakesData` is not Satisfied
//!
//! ```sh
//! the trait `TakesData<{integer}>` is not implemented for ...
//! ```
//!
//! There are two reasons for this error:
//! 1. The command does not take any data argument. Try restructuring the message from `(address, command, data)` to `(address, command)`.
//! 2. The command requires a data argument with a different type. Consult the [`documentation for the command`](command) and use the data type listed there.
//!
//! #### `TakesNoData` is not Satisfied
//!
//! ```sh
//! the trait `TakesNoData` is not implemented for ...
//! ```
//!
//! The command likely requires a data argument. Try restructuring the message from `(address, command)` to `(address, command, data)`.
//!
//! #### `ElicitsResponse` is not Satisfied
//!
//! ```sh
//! the trait `ElicitsResponse` is not implemented for ...
//! ```
//!
//! This means the command does not elicit a response from a device and the
//! [`Port::tx_rx`] family of functions would eventually time out waiting for
//! one. Try transmitting the command without reading a reply with
//! [`tx`](Port::tx) instead.
//!

pub mod command;
mod port;
pub mod traits;

use std::convert::Infallible;

use crate::error;
pub use port::*;

/// An Binary Protocol message received from a device.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct DeviceMessage<C = u8> {
    /// The targeted device
    target: u8,
    /// The command code
    command: C,
    /// The command data
    data: [u8; 4],
    /// The message ID
    id: Option<u8>,
}

impl<C> DeviceMessage<C> {
    /// Get the message target.
    pub const fn target(&self) -> u8 {
        self.target
    }

    /// Get the raw message data without decoding it.
    pub const fn raw_data(&self) -> [u8; 4] {
        self.data
    }

    /// Get the message id, if there is one.
    pub const fn id(&self) -> Option<u8> {
        self.id
    }
}

impl<C: traits::Command> DeviceMessage<C> {
    /// Get the message command code.
    pub fn command(&self) -> u8 {
        self.command.command()
    }

    /// Get the decoded message data.
    ///
    /// The type `T` to decode to is determined by the implementation of
    /// `ReplyData` for the command type `C`. For the types defined in
    /// [`command`], there is only one data type. For other command types, like
    /// `u8` (the type of the commands in [`command::untyped`]) any data type is
    /// supported (though the conversion may fail at run time), so you will need
    /// to specify the appropriate data type to decode.
    ///
    /// ## Example
    ///
    /// ```
    /// # use zproto::{
    /// #     binary::Port,
    /// #     backend::Backend,
    /// # };
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zproto::binary::command::*;
    /// let reply = port.tx_rx((0, RETURN_SETTING, SET_HOME_STATUS))?;
    /// let homed = reply.data()?;  // `homed` is a `bool`
    /// let reply = port.tx_rx((0, RETURN_SETTING, untyped::SET_HOME_STATUS))?;
    /// let homed: bool = reply.data()?;  // The data type must be explicitly defined here
    /// # Ok(())
    /// # }
    /// ```
    pub fn data<T>(&self) -> Result<T, T::Error>
    where
        T: traits::Data,
        C: traits::ReplyData<T>,
    {
        T::try_from_data(self.data)
    }

    /// Get the message without any type constraints on it.
    pub fn to_untyped(&self) -> DeviceMessage<u8> {
        DeviceMessage {
            target: self.target(),
            command: self.command.command(),
            data: self.data,
            id: self.id,
        }
    }

    /// Try to construct a strongly-typed `DeviceMessage` from an "untyped" one.
    pub fn try_from_untyped(
        other: DeviceMessage,
    ) -> Result<DeviceMessage<C>, error::BinaryUnexpectedCommandError> {
        if let Ok(command) = C::try_from(other.command()) {
            Ok(DeviceMessage {
                target: other.target,
                command,
                data: other.data,
                id: other.id,
            })
        } else {
            Err(error::BinaryUnexpectedCommandError::new(other))
        }
    }
}

impl DeviceMessage<u8> {
    /// Parse an array of 6 bytes into a [`DeviceMessage`].
    ///
    /// Set `id` to `true` if the response is expected to contain a message ID.
    pub(crate) const fn from_bytes(bytes: &[u8; 6], id: bool) -> DeviceMessage<u8> {
        DeviceMessage {
            target: bytes[0],
            command: bytes[1],
            data: [bytes[2], bytes[3], bytes[4], if id { 0 } else { bytes[5] }],
            id: if id { Some(bytes[5]) } else { None },
        }
    }
}

impl<C> std::fmt::Display for DeviceMessage<C>
where
    C: traits::Command,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "[{}, {}, {}",
            self.target,
            self.command.command(),
            i32::from_le_bytes(self.raw_data())
        )?;
        if let Some(id) = self.id() {
            write!(f, "{}]", id)
        } else {
            write!(f, "]")
        }
    }
}

/// Represents a version of firmware.
///
/// For convenience it is comparable to [`f32`].
///
/// ```
/// # use zproto::binary::Version;
/// let version = Version::new(7, 24).unwrap();
/// assert_eq!(version, 7.24);
/// ```
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Version(i32);

impl Version {
    /// Create a new version
    pub fn new(major: i32, minor: i32) -> Result<Self, std::num::TryFromIntError> {
        if minor % 100 != minor {
            Err(u8::try_from(-1).unwrap_err())
        } else {
            Ok(Version(major * 100 + minor % 100))
        }
    }

    /// Get the major version
    pub fn major(&self) -> i32 {
        self.0 / 100
    }
    /// Get the minor version
    pub fn minor(&self) -> i32 {
        self.0 % 100
    }
}

impl traits::Data for Version {
    type Error = Infallible;
    fn fill_data(&self, _: &mut [u8]) {
        // This type can never be sent
        unimplemented!();
    }
    fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        Ok(Version(i32::try_from_data(buffer)?))
    }
}

impl PartialEq<f32> for Version {
    // Ignore clippy warnings about allocating just for comparison -- it is a
    // false positive in this case.
    #[allow(clippy::cmp_owned)]
    fn eq(&self, other: &f32) -> bool {
        f32::from(*self) == *other
    }
}

impl PartialEq<Version> for f32 {
    // Ignore clippy warnings about allocating just for comparison -- it is a
    // false positive in this case.
    #[allow(clippy::cmp_owned)]
    fn eq(&self, other: &Version) -> bool {
        *self == f32::from(*other)
    }
}

impl From<Version> for f32 {
    fn from(other: Version) -> Self {
        other.major() as f32 + other.minor() as f32 / 100f32
    }
}

/// Define an enum for device status and all associated traits.
macro_rules! define_status {
    (pub enum $name:ident { $($variant:ident = $value:literal),+ $(,)? }) => {
        /// The status of a device.
        #[derive(Debug, Copy, Clone, PartialEq)]
        #[non_exhaustive]
        #[allow(missing_docs)]
        pub enum $name {
            $($variant = $value),+
        }

        impl From<$name> for i32 {
            fn from(value: $name) -> i32 {
                value as i32
            }
        }

        impl PartialEq<i32> for $name {
            fn eq(&self, other: &i32) -> bool {
                *self as i32 == *other
            }
        }

        impl PartialEq<$name> for i32 {
            fn eq(&self, other: &$name) -> bool {
                other == self
            }
        }

        impl TryFrom<i32> for $name {
            type Error = std::num::TryFromIntError;

            fn try_from(value: i32) -> Result<Self, Self::Error> {
                match value {
                    $($value => Ok($name::$variant)),+
                    ,
                    _ => Err(u8::try_from(-1).unwrap_err()),
                }
            }
        }

        impl traits::Data for $name {
            type Error = std::num::TryFromIntError;
            fn fill_data(&self, _: &mut[u8]) {
                // This data type should never be sent to a device.
                unimplemented!();
            }

            fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error> {
                $name::try_from(i32::from_le_bytes(buffer))
            }
        }
    };
}

define_status! {
    pub enum Status {
        Idle = 0,
        ExecutingHoming = 1,
        ExecutingManualVelocityMove = 10,
        ExecutingManualDisplacementMove = 11,
        StalledStoppedOrDisplacedWhileStationary = 13,
        ExecutingMoveStored = 18,
        ExecutingMoveAbsolute = 20,
        ExecutingMoveRelative = 21,
        ExecutingMoveConstantSpeed = 22,
        ExecutingStop = 23,
        Parked = 65,
        ExecutingMoveIndex = 78,
        DriverDisabled = 90,
        PeripheralInactive = 93,
        ExecutingMotion = 99,
    }
}

/// The state of multiple digital I/O channels.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct IoStates(u32);

impl IoStates {
    /// Return whether a channel is high.
    ///
    /// The `channel` index is one-based. This function will panic if `channel`
    /// is 0 or greater than 32.
    pub fn is_high(&self, channel: usize) -> bool {
        self.is_high_checked(channel)
            .expect("`channel` must be in the range (1, 32)")
    }

    /// Return whether a channel is high.
    ///
    /// The `channel` index is one-based. `None` is returned if `channel` is 0
    /// or greater than 32.
    pub const fn is_high_checked(&self, channel: usize) -> Option<bool> {
        if channel > 0 && channel <= 32 {
            Some(self.0 & (1 << (channel - 1)) != 0)
        } else {
            None
        }
    }
}

impl traits::Data for IoStates {
    type Error = Infallible;
    fn fill_data(&self, _: &mut [u8]) {
        // This type can never be sent
        unimplemented!();
    }
    fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        Ok(IoStates(i32::try_from_data(buffer)? as u32))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn version() {
        let version = Version(723);
        assert_eq!(version.major(), 7);
        assert_eq!(version.minor(), 23);

        assert_eq!(version, 7.23);
        assert_eq!(version, Version::new(7, 23).unwrap());

        assert!(Version::new(7, 100).is_err());
    }

    #[test]
    fn io_states() {
        let states = IoStates(0b101);
        assert_eq!(states.is_high(1), true);
        assert_eq!(states.is_high(2), false);
        assert_eq!(states.is_high(3), true);
        assert_eq!(states.is_high(4), false);
        assert_eq!(states.is_high(32), false);
    }

    #[test]
    #[should_panic]
    fn io_state_index_0() {
        let states = IoStates(1);
        states.is_high(0);
    }

    #[test]
    #[should_panic]
    fn io_state_index_gt_32() {
        let states = IoStates(1);
        states.is_high(33);
    }

    #[test]
    fn device_message_parsing_invalid_command_code() {
        use crate::binary::command::{types::*, untyped};

        // Incorrect command value results in an error.
        let err = DeviceMessage::<SetHomeSpeed>::try_from_untyped(DeviceMessage::from_bytes(
            &[3, untyped::SET_ACCELERATION, 0, 0, 0, 1],
            false,
        ))
        .unwrap_err();
        assert_eq!(
            DeviceMessage::from(err),
            DeviceMessage::from_bytes(&[3, untyped::SET_ACCELERATION, 0, 0, 0, 1], false)
        );
    }

    #[test]
    fn device_message_passing_with_id() {
        use crate::binary::command::{types::*, untyped};

        // Parsing a message with an ID works properly.
        let message: DeviceMessage<SetHomeSpeed> = DeviceMessage::try_from_untyped(
            DeviceMessage::from_bytes(&[3, untyped::SET_HOME_SPEED, 2, 0, 0, 1], true),
        )
        .unwrap();
        assert_eq!(message.target(), 3);
        assert_eq!(message.id(), Some(1));
        assert_eq!(message.command(), untyped::SET_HOME_SPEED);
        assert_eq!(message.data().unwrap(), 2);
    }

    #[test]
    fn device_message_passing_without_id() {
        use crate::binary::command::{types::*, untyped};

        // Parsing a message without an ID works properly.
        let message: DeviceMessage<SetHomeSpeed> = DeviceMessage::try_from_untyped(
            DeviceMessage::from_bytes(&[3, untyped::SET_HOME_SPEED, 2, 0, 0, 1], false),
        )
        .unwrap();
        assert_eq!(message.target(), 3);
        assert_eq!(message.id(), None);
        assert_eq!(message.command(), untyped::SET_HOME_SPEED);
        assert_eq!(message.data().unwrap(), i32::from_le_bytes([2, 0, 0, 1]));
    }
}
