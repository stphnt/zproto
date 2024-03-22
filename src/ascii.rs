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
//! Reading data from a response is as simple as calling [`data()`]
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
//! The library requires users to be explicit about checking the contents of responses.
//! To do this, many methods either take a validation function or return a
//! [`NotChecked<R>`] (where `R` is the response type, like a [`Reply`]). To
//! access the inner response, users must validate it's contents using the
//! methods on `NotChecked` and/or the validation functions in the [`check`] module.
//! If you want you can also write your own validation functions.
//!
//! ```rust
//! # use zproto::{ascii::{Port, response::Reply}, backend::Backend, error::Error};
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<Reply, Box<dyn std::error::Error>> {
//! use zproto::ascii::response::check::warning_is;
//!
//! let reply = port.command_reply((1, "get device.id"))?
//!     .flag_ok_and(warning_is("WR"))?; // check that the reply's flag is "OK"
//!                                      // and the warning flag is "WR".
//! # Ok(reply)
//! # }
//! ```
//!
//!
//! ## Other `Port` Methods
//!
//! The [`Port`] has many other helpful methods for receiving any type and number
//! of responses: [`Reply`]s, [`Info`]s, and [`Alert`]s. For instance, to
//! read an [`Alert`] use the [`response`](Port::response) method:
//!
//! ```rust
//! # use zproto::{ascii::{response::Alert, Port}, backend::Backend};
//! # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
//! let alert: Alert = port.response()?.check_minimal()?;
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
//! use zproto::ascii::response::check::minimal;
//! let (reply, infos) = port.command_reply_infos("stream buffer 1 print", minimal())?;
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
//! use zproto::ascii::response::check::flag_ok;
//! let target = (1, 2);
//! port.command_reply((target, "move max"))?.flag_ok()?;
//! port.poll_until_idle(target, flag_ok())?;
//! // Axis 2 on device 1 is now idle
//! # Ok(())
//! # }
//! ```
//! [`Alert`]: crate::ascii::response::Alert
//! [`Reply`]: crate::ascii::response::Reply
//! [`data()`]: crate::ascii::response::Reply::data
//! [`Info`]: crate::ascii::response::Info
//! [`NotChecked`]: crate::ascii::response::check::NotChecked
//! [`NotChecked<R>`]: crate::ascii::response::check::NotChecked
//! [`check`]: crate::ascii::response::check
//! [`Target`]: crate::ascii::command::Target

pub mod chain;
pub(crate) mod checksum;
pub mod command;
mod id;
mod marker;
pub mod packet;
pub mod port;
pub mod response;
pub mod setting;

pub use port::Port;
