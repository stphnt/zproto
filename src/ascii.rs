//! Types and traits for communicating with Zaber products over Zaber's [ASCII protocol](https://www.zaber.com/protocol-manual?protocol=ASCII).
//!
//! ## Communicating with Devices
//!
//! All communication with Zaber products starts with a [`Port`], which can be either a serial or TCP port:
//!
//! ```rust
//! # use zproto::{error::Error, ascii::Port};
//! # fn wrapper() -> Result<(), Error> {
//! let mut port = Port::open_serial("/dev/ttyUSB0")?;
//! // OR
//! let mut port = Port::open_tcp("192.168.0.1:55550")?;
//! # Ok(())
//! # }
//! ```
//!
//! Send a command to and receive a reply from devices/axes connected to
//! that port with the [`command_reply`](Port::command_reply) method. A
//! command can be created from most standard types that are effectively bytes,
//! such as `&str`, `String`, or `&[u8]`:
//!
//! ```rust
//! # use zproto::{
//! #     ascii::Port,
//! #     backend::Backend,
//! #     error::Error
//! # };
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! // Send `/get device.id` and receive a reply
//! let reply = port.command_reply("get device.id")?;
//! # Ok(())
//! # }
//! ```
//!
//! To target a specific device and/or axis prepend the target to the command
//! data as part of a tuple, `(target, data)`:
//!
//! ```rust
//! # use zproto::{
//! #     ascii::Port,
//! #     backend::Backend,
//! #     error::Error
//! # };
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! // Send `/2 get device.id` and receive a reply
//! let reply = port.command_reply((2, "get device.id"))?;
//! # Ok(())
//! # }
//! ```
//!
//! The target can be:
//!   * the device address: `(2, "home")`
//!   * the device address and axis number as a tuple: `((2, 1), "home")`
//!   * a [`Target`] type: `(Target::for_device(2).with_axis(1), "home")`
//!
//! As a convenience that reduces the number of nested parentheses, the command
//! tuple can also have the form `(device, axis, data)`:
//!
//! ```rust
//! # use zproto::{
//! #     ascii::Port,
//! #     backend::Backend,
//! #     error::Error
//! # };
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! // Send `/2 1 get pos` and receive a reply
//! let reply = port.command_reply((2, 1, "get pos"))?;
//! # Ok(())
//! # }
//! ```
//!
//! ## Reading Data
//!
//! Reading data from a response is as simple as calling [`data()`](Reply::data)
//! on the response and then using Rust's standard [`parse()`](https://doc.rust-lang.org/std/primitive.str.html#method.parse)
//! method to convert the string to your desired data type.
//!
//! ```rust
//! # use zproto::{ascii::Port, backend::Backend, error::Error};
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! let reply = port.command_reply("get device.id")?.flag_ok()?;
//! let device_id: u32 = reply.data().parse()?;
//! # Ok(())
//! # }
//! ```
//!
//! ## Checking Responses
//!
//! The library always checks the contents of responses and returns an error for
//! rejected commands or replies with any warnings. However, that may not always
//! be desirable, in which case you can explicitly define how the response should
//! be checked. The [`check`] module defines many common validation functions,
//! or you can write your own:
//!
//! ```rust
//! # use zproto::{ascii::{Port, Reply}, backend::Backend, error::Error};
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<Reply, Box<dyn std::error::Error>> {
//! use zproto::ascii::check::warning_is;
//!
//! let reply = port.command_reply((1, "get device.id"))?
//!     .flag_ok_and(warning_is("WR"))?;
//! # Ok(reply)
//! # }
//! ```
//!
//! Most [`Port`] methods have a `_with_check` version so you can override the
//! default validation of a response.
//!
//! ## Other `Port` Methods
//!
//! The [`Port`] has many other helpful methods for receiving any type and number
//! of responses: [`Reply`]s, [`Info`]s, and [`Alert`]s. For instance, to
//! read an [`Alert`] use the [`response`](Port::response) method:
//!
//! ```rust
//! # use zproto::{ascii::{Alert, Port}, backend::Backend};
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
//! # use zproto::{ascii::Port, backend::Backend};
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! let (reply, infos) = port.command_reply_infos("stream buffer 1 print")?;
//! println!("{}", reply);  // `@01 0 OK IDLE -- 0` (for example)
//! for info in infos {
//!     println!("{}", info); // `#01 0 setup store 1 1` (for example)
//! }
//! # Ok(())
//! # }
//! ```
//!
//! It also provides convenience functions for other common tasks, like waiting
//! until a specific target is `IDLE`:
//!
//! ```rust
//! # use zproto::{
//! #     ascii::Port,
//! #     backend::Backend
//! # };
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! let target = (1, 2);
//! port.command_reply((target, "move max"))?;
//! port.poll_until_idle(target)?;
//! // Axis 2 on device 1 is now idle
//! # Ok(())
//! # }
//! ```

pub mod chain;
pub(crate) mod checksum;
mod command;
pub mod data_type;
mod id;
mod marker;
pub mod parse;
mod port;
mod response;
pub mod scope;

pub use command::*;
pub use port::*;
pub use response::*;

/// The device address and axis number a command/response was sent to/from.
///
/// `Target` has multiple builder methods that can be chained to construct the
/// desired target.
///
/// ```rust
/// # use zproto::ascii::Target;
/// let target = Target::for_device(1).with_axis(2);
/// ```
///
/// Or you can create a target from a `u8` or `tuple` of `u8`s:
///
/// ```rust
/// # use zproto::ascii::Target;
/// assert_eq!(Target::for_device(1), Target::from(1));
/// assert_eq!(Target::for_device(1).with_axis(2), Target::from((1, 2)));
/// ```
///
/// The [`Default`](Target::default) target is all devices and axes.

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Target(u8, u8);

impl Target {
	/// Create a new target with the specified `device` address and `axis` number.
	pub const fn new(device: u8, axis: u8) -> Target {
		Target(device, axis)
	}
	/// Create a target for all devices and axes (i.e. device address 0 and axis number 0)
	///
	/// ```
	/// # use zproto::ascii::Target;
	/// assert_eq!(Target::for_all(), Target::new(0, 0));
	/// ```
	pub const fn for_all() -> Target {
		Target(0, 0)
	}
	/// Create a target for a specific device.
	pub const fn for_device(address: u8) -> Target {
		Target(address, 0)
	}
	/// Create a target for the all axes on this device.
	///
	/// The device address of the current target is copied to the new target.
	///
	/// ```
	/// # use zproto::ascii::Target;
	/// assert_eq!(Target::new(2, 1).with_all_axes(), Target::new(2, 0));
	/// ```
	pub const fn with_all_axes(self) -> Target {
		Target(self.0, 0)
	}
	/// Create a target for the specified axis on the device.
	///
	/// The device address of the current target is copied to the new target.
	///
	/// ```
	/// # use zproto::ascii::Target;
	/// assert_eq!(Target::new(2, 1).with_axis(3), Target::new(2, 3));
	/// ```
	pub const fn with_axis(self, axis: u8) -> Target {
		Target(self.0, axis)
	}
	/// Get the address of the targeted device.
	///
	/// ```
	/// # use zproto::ascii::Target;
	/// assert_eq!(Target::new(2, 1).device(), 2);
	/// ```
	pub const fn device(self) -> u8 {
		self.0
	}
	/// Get the number of the targeted axis.
	///
	/// ```
	/// # use zproto::ascii::Target;
	/// assert_eq!(Target::new(2, 1).axis(), 1);
	/// ```
	pub const fn axis(self) -> u8 {
		self.1
	}
	/// Assuming this target is that of a response, determine if the response
	/// could have been elicited by a command to the specified target.
	///
	/// This is true if
	///  * the command's device address was 0 or matches this target's address, and
	///  * the command's axis number was 0 or matches this target's axis number
	pub(crate) const fn elicited_by_command_to(self, target: Target) -> bool {
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
		Target::for_all()
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
		assert_eq!(Target::default(), Target::for_all());
	}
}
