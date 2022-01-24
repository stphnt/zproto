//! Types and traits for communicating with Zaber products over Zaber's [ASCII protocol](https://www.zaber.com/protocol-manual?protocol=ASCII).
//!
//! All communication with Zaber products starts with a [`Port`], which can be either a serial or TCP port:
//!
//! ```rust
//! # use zaber_protocol::{error::Error, ascii::Port};
//! # fn wrapper() -> Result<(), Error> {
//! let mut port = Port::open_serial("/dev/ttyUSB0")?;
//! // OR
//! let mut port = Port::open_tcp("192.168.0.1:7770")?;
//! # Ok(())
//! # }
//! ```
//!
//! You can then send a command and receive a reply to devices/axes connected to
//! that port with the [`command_reply`](Port::command_reply) method. A
//! [`Command`] can be created in two main ways:
//!
//! * using the [`Command`]'s builder methods
//!
//! ```rust
//! # use zaber_protocol::{
//! #     ascii::{Command, Port},
//! #     backend::Backend,
//! #     error::Error
//! # };
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! let reply = port.command_reply(Command::new("get device.id").to(1))?; // Sends `/1 get device.id`
//! let device_id: u32 = reply.data().parse()?;
//! # Ok(())
//! # }
//! ```
//!
//! * importing the [`IntoCommand`] trait which adds [`to(..)`](IntoCommand::to) or [`to_all()`](IntoCommand::to_all)
//! methods it `&str` and a few other types to easily convert them into a [`Command`]:
//!
//! ```rust
//! # use zaber_protocol::{ascii::Port, backend::Backend, error::Error};
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! use zaber_protocol::ascii::IntoCommand as _;
//!
//! let reply = port.command_reply("get device.id".to(1))?; // Sends `/1 get device.id`
//! let device_id: u32 = reply.data().parse()?;
//! # Ok(())
//! # }
//! ```
//!
//! You can determine the target devices/axes in the [`to(..)`](IntoCommand::to)
//! method by either:
//!   * passing in the device address (e.g., `to(2)`)
//!   * passing in the device address and axis number as a tuple (e.g., `to((1, 2))`)
//!   * passing in a [`Target`] type (e.g., `Target::device(1).axis(2)`)
//!
//! The library always checks the contents of responses and returns an error for
//! rejected commmands or replies with any warnings. However, that may not always
//! be desirable, in which case you can explicitly define how the response should
//! be checked:
//!
//! ```rust
//! # use zaber_protocol::{ascii::Port, backend::Backend, error::Error};
//! # use zaber_protocol::ascii::IntoCommand as _;
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! use zaber_protocol::ascii::check::{flag_ok_and, warning_is};
//!
//! let reply = port.command_reply_with_check(
//!    "get device.id".to(1),
//!    flag_ok_and(warning_is("WR"))
//! )?;
//! # Ok(())
//! # }
//! ```
//!
//! Most [`Port`] methods have a `_with_check` version so you can override
//! the default validation of a response.
//!
//! The [`Port`] has many other helpful methods for receiving any type and number
//! of responses: [`Reply`]s, [`Info`]s, and [`Alert`]s. For instance, to
//! read an [`Alert`] use the [`response`](Port::response) method:
//!
//! ```rust
//! # use zaber_protocol::{ascii::{Alert, Port}, backend::Backend};
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! let alert: Alert = port.response()?;
//! # Ok(())
//! # }
//! ```
//!
//! Or to read the reply and info messages sent in response to a command use the
//! [`command_reply_infos`](Port::command_reply_infos) method:
//!
//! ```rust
//! # use zaber_protocol::{ascii::Port, backend::Backend};
//! # use zaber_protocol::ascii::IntoCommand as _;
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! use zaber_protocol::ascii::check::{flag_ok_and, warning_is};
//!
//! let (reply, infos) = port.command_reply_infos("stream buffer 1 print".to(1))?;
//! println!("{}", reply);  // `@01 0 OK IDLE -- 0` (for example)
//! for info in infos {
//!     println!("{}", info); // `#01 0 setup store 1 1` (for example)
//! }
//! # Ok(())
//! # }
//! ```
//!
//! It also provides convenience functions for common tasks:
//!
//! ```rust
//! # use zaber_protocol::{
//! #     ascii::{Port, IntoCommand as _},
//! #     backend::Backend
//! # };
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! let target = (1, 2);
//! port.command_reply("move max".to(target))?;
//! port.poll_until_idle(target)?;
//! // Axis 2 on device 1 is now idle
//! # Ok(())
//! # }
//! ```

pub(crate) mod checksum;
mod command;
mod id;
mod parse; // Should never be public (contains private traits)
mod port;
mod response;

pub use command::*;
pub use port::*;
pub use response::*;

/// The device and axis number a command/response was sent to/from.
///
/// `Target` has multiple builder methods can can be chained to construct the
/// desired target.
///
/// ```rust
/// # use zaber_protocol::ascii::Target;
/// let target = Target::device(1).axis(2);
/// ```
///
/// Or you can create a target from a `u8` or `tuple` of `u8`s:
///
/// ```rust
/// # use zaber_protocol::ascii::Target;
/// assert_eq!(Target::device(1), Target::from(1));
/// assert_eq!(Target::device(1).axis(2), Target::from((1, 2)));
/// ```
///
/// The [`Default`](Target::default) target is all devices and axes.

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Target(u8, u8);

impl Target {
    /// Create a new target
    pub const fn new(address: u8, axis: u8) -> Target {
        Target(address, axis)
    }
    /// Get the target for all devices
    pub const fn all() -> Target {
        Target(0, 0)
    }
    /// Get the target for a specific device
    pub const fn device(address: u8) -> Target {
        Target(address, 0)
    }
    /// Get the target for the all axes on this device
    pub const fn all_axes(&self) -> Target {
        Target(self.0, 0)
    }
    /// Get the target for the specified axis on the device.
    pub const fn axis(&self, axis: u8) -> Target {
        Target(self.0, axis)
    }
    /// Get the address of the targeted device.
    pub const fn get_address(&self) -> u8 {
        self.0
    }
    /// Get the number of the targeted axis.
    pub const fn get_axis(&self) -> u8 {
        self.1
    }
    /// Assuming this target is that of a response, determine if the response
    /// could have been elicited by a command to the specified target.
    ///
    /// This is true if
    ///  * the command's device address was 0 or matches this target's address, and
    ///  * the command's axis number was 0 or matches this target's axis number
    pub(crate) const fn elicited_by_command_to(&self, target: Target) -> bool {
        if target.0 == 0 || self.0 == target.0 {
            target.1 == 0 || self.1 == target.1
        } else {
            false
        }
    }
}

impl Default for Target {
    /// Get the default target, which is all devices and axes in the chain.
    fn default() -> Target {
        Target::all()
    }
}

impl From<u8> for Target {
    fn from(other: u8) -> Target {
        Target(other, 0)
    }
}

impl From<(u8, u8)> for Target {
    fn from(other: (u8, u8)) -> Target {
        Target(other.0, other.1)
    }
}

#[cfg(test)]
mod test {
    use super::Target;

    #[test]
    fn target_default_is_all() {
        assert_eq!(Target::default(), Target::all());
    }
}
