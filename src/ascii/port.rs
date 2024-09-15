//! Types for opening and using a serial port with the ASCII protocol.

pub mod handlers;
pub mod iter;
mod options;
#[cfg(test)]
mod test;

use crate::backend::{Backend, Serial, UNKNOWN_BACKEND_NAME};
#[allow(clippy::wildcard_imports)]
use crate::error::*;
use crate::{
	ascii::{
		chain::Chain,
		checksum::Lrc,
		command::{Command, CommandWriter, MaxPacketSize, Target},
		id,
		packet::{Packet, PacketKind},
		response::{
			check::{self, NotChecked},
			Alert, AnyResponse, Info, Reply, Response, ResponseBuilder, Status,
		},
	},
	routine::{IntoRoutine, Routine},
	timeout_guard::TimeoutGuard,
};
use handlers::{Handlers, LocalHandlers, SendHandlers};
pub use options::*;
use std::{
	convert::TryFrom,
	io,
	net::{TcpStream, ToSocketAddrs},
	time::Duration,
};

/// The direction a packet was sent.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Direction {
	/// The packet was transmitted to a device.
	Tx,
	/// The packet was received from a device.
	Recv,
}

/// The default tag to mark types
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DefaultTag {}

/// The type of [`Port`] that implements `Send`.
pub type SendPort<'a, B, Tag = DefaultTag> = Port<'a, B, Tag, SendHandlers<'a>>;

/// A port configured to use the ASCII protocol.
///
/// See the [`ascii`] module-level documentation for details on how to use a `Port`.
///
/// A port is parameterized by three types:
///
/// 1. `B`: the type of [`Backend`] used to send/receive packets.
///    * Use the convenience methods [`open_serial`] and [`open_tcp`] to construct
///      a serial port (`Port<Serial>`) or a TCP port (`Port<TcpStream>`). To
///      customize the construction of these types, or to construct a port with a
///      dynamic backend, use the [`OpenSerialOptions`] and [`OpenTcpOptions`] builder
///      types.
/// 2. `Tag`: an optional type for "tagging" the port.
///    * This has a default and can be ignored if you only ever have one port open.
///      When working with multiple ports simultaneously, the `Tag` type can be used
///      to statically differentiate them and improve your programs type safety. Use
///      the [`into_tagged`] method to change a port's `Tag` type.
/// 3. `H`: the type of event [handlers].
///    * This has a default and can be ignored in single-threaded contexts.
///      There are two types for event handlers: one that implements `Send` and one
///      that does not (the default). To convert a port into a type that implements
///      `Send`, use the [`try_into_send`] method.
///
/// [`ascii`]: crate::ascii
/// [`into_tagged`]: Port::into_tagged
/// [`try_into_send`]: Port::try_into_send
/// [`open_serial`]: Port::open_serial
/// [`open_tcp`]: Port::open_tcp
pub struct Port<'a, B, Tag = DefaultTag, H = LocalHandlers<'a>> {
	/// The underlying backend
	backend: B,
	/// The message ID generator
	ids: id::Counter,
	/// Whether commands should include message IDs or not.
	generate_id: bool,
	/// Whether commands should include checksums or not.
	generate_checksum: bool,
	/// The maximum command packet size.
	max_packet_size: MaxPacketSize,
	/// If populated, the error that has "poisoned" the port. This error MUST be
	/// reported before the port is used for communication again.
	///
	/// A port becomes "poisoned" when an error occurs that
	///
	///  * cannot be recovered from,
	///  * panicking is ill advised,
	///  * and it is safe to delay reporting of the error until the next attempt
	///    to communicate over the port.
	///
	/// For instance, if a [`TimeoutGuard`] cannot restore the original timeout
	/// in its Drop implementation, rather than panicking (which would almost
	/// certainly cause the program to abort rather than unwind the stack) it
	/// can poison the port.
	poison: Option<io::Error>,
	/// The builder used to concatenate packets in to responses.
	builder: ResponseBuilder,
	/// User supplied event handlers
	handlers: H,
	/// The type differentiating this Port for other Ports at compile time.
	tag: std::marker::PhantomData<Tag>,
	/// Marker for the lifetime
	lifetime: std::marker::PhantomData<&'a ()>,
}

impl<'a, B: Backend, Tag, H> std::fmt::Debug for Port<'a, B, Tag, H> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Port")
			.field("name", &self.backend.name())
			.finish_non_exhaustive()
	}
}

impl<'a> Port<'a, Serial> {
	/// Open the serial port at the specified path using the default options.
	///
	/// Alternatively, use [`Port::open_serial_options`] to customize how the port is opened.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::ascii::Port;
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// let mut port = Port::open_serial("/dev/ttyUSB0")?;
	/// // Or equivalently
	/// let mut port = Port::open_serial_options().open("/dev/ttyUSB0")?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn open_serial(path: &str) -> Result<Port<'a, Serial>, AsciiError> {
		OpenSerialOptions::new().open(path)
	}

	/// Get an [`OpenSerialOptions`] to customize how a serial port is opened.
	pub fn open_serial_options() -> OpenSerialOptions {
		OpenSerialOptions::default()
	}
}

impl<'a> Port<'a, TcpStream> {
	/// Open the TCP port at the specified address using the default options.
	///
	/// Alternatively, use [`Port::open_tcp_options`] to customize how the port is opened.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::ascii::Port;
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// let mut port = Port::open_tcp("198.168.0.1:55550")?;
	/// // Or equivalently
	/// let mut port = Port::open_tcp_options().open("198.168.0.1:55550")?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn open_tcp<A: ToSocketAddrs>(address: A) -> Result<Port<'a, TcpStream>, io::Error> {
		OpenTcpOptions::default().open(address)
	}

	/// Get an [`OpenTcpOptions`] to customize how a TCP port is opened.
	pub fn open_tcp_options() -> OpenTcpOptions {
		OpenTcpOptions::default()
	}
}

impl<'a, B, Tag, H> Port<'a, B, Tag, H>
where
	B: Backend,
	H: Handlers,
	H::PacketHandler: FnMut(&[u8], Direction) + 'a,
	H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'a,
{
	/// Create a `Port` from a [`Backend`] type.
	fn from_backend(
		backend: B,
		generate_id: bool,
		generate_checksum: bool,
		max_packet_size: MaxPacketSize,
	) -> Self {
		Port {
			backend,
			ids: id::Counter::default(),
			generate_id,
			generate_checksum,
			max_packet_size,
			poison: None,
			builder: ResponseBuilder::default(),
			handlers: H::default(),
			tag: std::marker::PhantomData,
			lifetime: std::marker::PhantomData,
		}
	}

	/// Check if the port is poisoned and report the error if it exists.
	fn check_poisoned(&mut self) -> Result<(), io::Error> {
		if let Some(poison) = self.poison.take() {
			Err(poison)
		} else {
			Ok(())
		}
	}

	/// Convert the type of this port by "tagging" it with the type `T`.
	///
	/// This does not change the runtime behaviour of the port; it only changes
	/// how the compiler treats the port and the types generated from the port,
	/// such as [`Chain`], during compilation.
	///
	/// This is primarily used for differentiating multiple `Port`s from each
	/// other and preventing them from being used where they should not be.
	///
	/// # Examples
	///
	/// When communicating with products over more than one port it can be easy
	/// to pass the wrong port to the [`Routine`]s generated from a port's
	/// [`Chain`]. In the best case, the command fails at run time, but in many
	/// cases it simply results in incorrect data.
	///
	/// ```
	/// # use zproto::{ascii::Port, backend::Backend, error::AsciiError};
	/// # fn wrapper<B: Backend>(mut port_a: Port<B>, mut port_b: Port<B>) -> Result<(), AsciiError> {
	/// use zproto::ascii::setting::v_latest::SystemSerial;
	///
	/// let chain = port_a.chain()?;
	/// for device in &chain {
	///     let get_system_serial = device.settings().get(SystemSerial);
	///     let _serial = port_b.run(get_system_serial)?;
	///     //            ^^^^^^ OOPS! This is the wrong port!
	/// }
	/// # Ok(())
	/// # }
	/// ```
	///
	/// However, by tagging each port's type, we change the type of the port and
	/// the types of the routines generated from it, turning the above runtime
	/// error into a compiler error.
	///
	/// ```compile_fail
	/// # use zproto::{ascii::Port, backend::Backend, error::AsciiError};
	/// # fn wrapper<B: Backend>(mut port_a: Port<B>, mut port_b: Port<B>) -> Result<(), AsciiError> {
	/// use zproto::ascii::setting::v_latest::SystemSerial;
	///
	/// struct PortA;
	/// let mut port_a = port_a.into_tagged::<PortA>();
	/// let chain = port_a.chain()?; // Has a different type
	/// for device in &chain {
	///     let get_system_serial = device.settings().get(SystemSerial); // Has a different type
	///     // Yay! This no longer compiles because the type of port_b is
	///     // incompatible with the type of get_system_serial.
	///     let _id = port_b.run(get_system_serial)?;
	///     // ERROR:        ^^^ the trait `Routine<Port<B>>` is not implemented for
	///     //                   `Get<SystemSerial, PortA>`
	/// }
	/// # Ok(())
	/// # }
	/// ```
	///
	/// Using tagged ports can also be useful for ensuring functions receive
	/// appropriate types.
	///
	/// ```
	/// # use zproto::ascii::{Port, chain::Chain};
	/// fn do_something<Backend, Tag>(port: &mut Port<Backend, Tag>, chain: &Chain<Tag>) {
	///     // The chain is guaranteed to be associated with the port, assuming
	///     // each type Tag is a only used to tag one port.
	///     todo!()
	/// }
	/// ```
	pub fn into_tagged<T>(self) -> Port<'a, B, T, H> {
		Port {
			backend: self.backend,
			ids: self.ids,
			generate_id: self.generate_id,
			generate_checksum: self.generate_checksum,
			max_packet_size: self.max_packet_size,
			poison: self.poison,
			builder: self.builder,
			handlers: self.handlers,
			tag: std::marker::PhantomData,
			lifetime: std::marker::PhantomData,
		}
	}

	/// Convert this port into one that implements `Send` and can therefore be
	/// sent to another thread.
	///
	/// Returns an error if `Send` bounds could not be added. This is, if any
	/// event handlers are currently set. As such, it is recommended to call
	/// [`Port::try_into_send`] before setting handlers.
	///
	/// The [`Port::set_packet_handler`] and [`Port::set_unexpected_alert_handler`]
	/// methods on the new port will have an additional `Send` bound on any function
	/// used as an event handler.
	///
	/// # Example
	///
	/// ```
	/// # use zproto::{ascii::Port, backend::Backend};
	/// # use std::{error::Error, fmt::Debug};
	/// # fn wrapper<B: Backend + Debug + Send + 'static>(
	/// #     port: Port<'static, B>
	/// # ) -> Result<(), Box<dyn Error>> {
	/// use std::sync::{Arc, Mutex};
	///
	/// let port = Port::open_serial("...")?.try_into_send()?;
	/// let port = Arc::new(Mutex::new(port));
	///
	/// let mut handles = vec![];
	/// for i in 0..10 {
	///     let port = port.clone();
	///     let handle = std::thread::spawn(move || {
	///         let mut guard = port.lock().unwrap();
	///         // do something with the port ...
	///     });
	///     handles.push(handle);
	/// }
	///
	/// for handle in handles {
	///     let _ = handle.join();
	/// }
	/// # Ok(())
	/// # }
	/// ```
	pub fn try_into_send(mut self) -> Result<SendPort<'a, B, Tag>, TryIntoSendError> {
		if self.handlers.packet().is_some() || self.handlers.unexpected_alert().is_some() {
			return Err(TryIntoSendError::new());
		}
		Ok(Port {
			backend: self.backend,
			ids: self.ids,
			generate_id: self.generate_id,
			generate_checksum: self.generate_checksum,
			max_packet_size: self.max_packet_size,
			poison: self.poison,
			builder: self.builder,
			handlers: SendHandlers::default(),
			tag: std::marker::PhantomData,
			lifetime: std::marker::PhantomData,
		})
	}

	/// Send a command. A reply is not read.
	///
	/// On success, the generated message ID (if any) is returned.
	///
	/// If necessary, the command will be split into multiple packets so that no
	/// packet is longer than [`max_packet_size`](Self::max_packet_size).
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::Port, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// // Send the empty command.
	/// port.command("")?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn command<C: Command>(&mut self, cmd: C) -> Result<Option<u8>, AsciiError> {
		self.check_poisoned()?;

		let mut buffer = Vec::new();
		let mut writer = CommandWriter::new(
			&cmd,
			&mut self.ids,
			self.generate_id,
			self.generate_checksum,
			self.max_packet_size,
		)?;
		let mut more_packets = true;
		while more_packets {
			more_packets = writer.write_packet(&mut buffer)?;
			log::debug!(
				"{} TX:   {}",
				self.backend
					.name()
					.unwrap_or_else(|| UNKNOWN_BACKEND_NAME.to_string()),
				String::from_utf8_lossy(buffer.as_slice()).trim_end()
			);
			self.backend.write_all(buffer.as_slice())?;
			if let Some(callback) = self.handlers.packet() {
				(callback)(buffer.as_slice(), Direction::Tx);
			}
			buffer.clear();
		}
		Ok(writer.id)
	}

	/// Transmit a command and receive a reply.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If the reply is split across multiple packets, the continuation messages will automatically be read.
	///
	/// The contents of the reply are not checked, as the [`NotChecked<Reply>`](NotChecked) return type indicates.
	/// To access the reply, the caller must check the contents of reply via one of the methods on [`NotChecked`].
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::{Port, response::Reply}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<Reply, Box<dyn std::error::Error>> {
	/// let reply = port.command_reply("get maxspeed")?.check_minimal()?;
	/// # Ok(reply)
	/// # }
	/// ```
	pub fn command_reply<C>(&mut self, cmd: C) -> Result<NotChecked<Reply>, AsciiError>
	where
		C: Command,
	{
		self.internal_command_reply(&cmd)
	}

	/// Transmit a command and receive a reply.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If any response is spread across multiple packets, continuation packets will be read.
	fn internal_command_reply(
		&mut self,
		cmd: &dyn Command,
	) -> Result<NotChecked<Reply>, AsciiError> {
		let id = self.command(cmd)?;
		self.pre_receive_response();
		let response = self.receive_response(HeaderCheck::Matches {
			target: cmd.target(),
			id,
		})?;
		self.post_receive_response()?;
		Ok(response)
	}

	/// Transmit a command and then receive a reply and all subsequent info messages.
	///
	/// The reply and info messages are checked with the custom [`checker`](check::Check).
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size). If the reply or info messages are split across multiple
	/// packets, the continuation messages will automatically be read.
	///
	/// To avoid collecting the info messages into a vector use [`command_reply_infos_iter`](Port::command_reply_infos_iter).
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::{
	/// #     ascii::{response::{check, AnyResponse}, Port},
	/// #     backend::Backend,
	/// #     error::{AsciiCheckError, AsciiError}
	/// # };
	/// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), AsciiError> {
	/// let (reply, info_messages) = port.command_reply_infos(
	///    (1, "stream buffer 1 print"),
	///    check::minimal(),
	/// )?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn command_reply_infos<C, K>(
		&mut self,
		cmd: C,
		checker: K,
	) -> Result<(Reply, Vec<Info>), AsciiError>
	where
		C: Command,
		K: check::Check<AnyResponse>,
	{
		let checker: &dyn check::Check<_> = &checker;
		let reply_checker = |reply| {
			checker
				.check(AnyResponse::from(reply))
				.map(|response| Reply::try_from(response).unwrap())
				.map_err(|err| AsciiCheckError::try_from(err).unwrap())
		};
		let info_checker = |info| {
			checker
				.check(AnyResponse::from(info))
				.map(|response| Info::try_from(response).unwrap())
				.map_err(|err| AsciiCheckError::try_from(err).unwrap())
		};
		let (reply, info_iter) = self.command_reply_infos_iter(cmd)?;
		let reply = reply.check(reply_checker)?;
		let mut infos = Vec::new();
		for result in info_iter {
			let info = result?.check(info_checker)?;
			infos.push(info);
		}
		Ok((reply, infos))
	}

	/// Transmit a command and return it's reply and an iterator to read all subsequent info messages when used.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size). If the reply or info messages are split across multiple
	/// packets, the continuation messages will automatically be read.
	///
	/// The iterator produces a `Result<NotChecked<Info>>`, which must be handled by the caller on each iteration.
	///
	/// To simply check and collect all the info messages into a vector, use [`command_reply_infos`](Port::command_reply_infos).
	///
	/// ## Under The Hood
	///
	/// A common pattern in the ASCII protocol is to send additional information
	/// in info messages after replying to a command. In order to reliably
	/// receive all the info messages elicited by the command, an empty command
	/// (`/`) is sent to the same device. The reply to that message is
	/// guaranteed to come after the info messages from the previous command
	/// and so receipt of that reply signals the end of the previous command's
	/// info messages.
	///
	/// ## Errors
	///
	/// An error is returned if
	///   * any response other than the single reply and info messages elicited by the command are received or
	///   * a reply to the final empty command is never received.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::{
	/// #     ascii::{response::{check, AnyResponse}, Port},
	/// #     backend::Backend,
	/// #     error::{AsciiCheckError, AsciiError}
	/// # };
	/// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), AsciiError> {
	/// let (reply, info_iter) = port.command_reply_infos_iter((1, "stream buffer 1 print"))?;
	/// let reply = reply.flag_ok()?;
	/// for result in info_iter {
	///     let info = result?.check_minimal()?;
	///     println!("{info:?}");
	/// }
	/// # Ok(())
	/// # }
	/// ```
	// The return type is complex, but making a type alias doesn't make it better.
	#[allow(clippy::type_complexity)]
	pub fn command_reply_infos_iter<C: Command>(
		&mut self,
		cmd: C,
	) -> Result<
		(
			NotChecked<Reply>,
			iter::InfosUntilSentinel<'_, 'a, B, Tag, H>,
		),
		AsciiError,
	> {
		self.internal_command_reply_infos_iter(&cmd)
	}

	// The return type is complex, but making a type alias doesn't make it better.
	#[allow(clippy::type_complexity)]
	fn internal_command_reply_infos_iter(
		&mut self,
		cmd: &dyn Command,
	) -> Result<
		(
			NotChecked<Reply>,
			iter::InfosUntilSentinel<'_, 'a, B, Tag, H>,
		),
		AsciiError,
	> {
		let target = cmd.target();
		let reply = self.internal_command_reply(cmd)?;
		let old_generate_id = self.set_message_ids(true);
		let result = self.command((target, ""));
		self.set_message_ids(old_generate_id);
		let sentinel_id = result?;
		let info_id = reply.id();
		Ok((
			reply,
			iter::InfosUntilSentinel::new(self, target, info_id, sentinel_id),
		))
	}

	/// Transmit a command, receive n replies, and check each reply with the [`strict`](check::strict) check.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If any of the replies are split across multiple packets, the continuation messages will automatically be read.
	///
	/// To avoid collecting the responses into a vector use [`command_reply_n_iter`](Port::command_reply_n_iter).
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::Port, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::ascii::response::check::flag_ok;
	///
	/// let replies = port.command_reply_n("get system.serial", 5, flag_ok())?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn command_reply_n<C, K>(
		&mut self,
		cmd: C,
		n: usize,
		checker: K,
	) -> Result<Vec<Reply>, AsciiError>
	where
		C: Command,
		K: check::Check<Reply>,
	{
		let mut replies = Vec::new();
		let checker: &dyn check::Check<_> = &checker;
		for result in self.internal_command_reply_n_iter(&cmd, n)? {
			replies.push(result?.check(checker)?);
		}
		Ok(replies)
	}

	/// Transmit a command and get an iterator that will read `n` responses of type `R` from the port when used.
	///
	/// If the response is split across multiple packets, the continuation messages will automatically be read.
	///
	/// The iterator produces a `Result<NotChecked<Reply>>`, which must be handled by the caller on each iteration.
	///
	/// To simply check and collect all the replies into a vector, use [`command_reply_n`](Port::command_reply_n).
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::{ascii::{response::Info, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// for result in port.command_reply_n_iter("get device.id", 3)? {
	///     /// Handle any communication errors and check the contents of the reply.
	///     let reply = result?.flag_ok()?;
	///     println!("{}", reply.data());
	/// }
	/// # Ok(())
	/// # }
	/// ```
	pub fn command_reply_n_iter<C>(
		&mut self,
		cmd: C,
		n: usize,
	) -> Result<iter::NResponses<'_, 'a, B, Reply, Tag, H>, AsciiError>
	where
		C: Command,
	{
		self.internal_command_reply_n_iter(&cmd, n)
	}

	/// Transmit a command and get an iterator that will read `n` replies.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If any response is spread across multiple packets, continuation packets will be read.
	fn internal_command_reply_n_iter(
		&mut self,
		cmd: &dyn Command,
		n: usize,
	) -> Result<iter::NResponses<'_, 'a, B, Reply, Tag, H>, AsciiError> {
		let id = self.command(cmd)?;
		Ok(self.internal_response_n_iter(
			n,
			HeaderCheck::Matches {
				target: cmd.target(),
				id,
			},
		))
	}

	/// Transmit a command, receive replies until the port times out, and check each reply with the custom [`Check`](check::Check).
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If any of the replies are split across multiple packets, the continuation messages will automatically be read.
	///
	/// To avoid collecting the responses into a vector use [`command_replies_until_timeout_iter`](Port::command_replies_until_timeout_iter).
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::{ascii::Port, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::ascii::response::check::flag_ok;
	///
	/// let replies = port.command_replies_until_timeout(
	///     "get system.serial",
	///     flag_ok(),
	/// )?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn command_replies_until_timeout<C, K>(
		&mut self,
		cmd: C,
		checker: K,
	) -> Result<Vec<Reply>, AsciiError>
	where
		C: Command,
		K: check::Check<Reply>,
	{
		let mut replies = Vec::new();
		let checker: &dyn check::Check<_> = &checker;
		for result in self.internal_command_replies_until_timeout_iter(&cmd)? {
			replies.push(result?.check(checker)?);
		}
		Ok(replies)
	}

	/// Transmit a command and get an iterator that, when used, will read replies from the port until it times out.
	///
	/// If the response is split across multiple packets, the continuation messages will automatically be read.
	///
	/// The iterator produces a `Result<NotChecked<Reply>>`, which must be handled by the caller on each iteration.
	///
	/// To simply check and collect all the replies into a vector, use [`command_replies_until_timeout`](Port::command_replies_until_timeout).
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::{ascii::{response::Info, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// for result in port.command_replies_until_timeout_iter("get device.id")? {
	///     /// Handle any communication errors and check the contents of the reply.
	///     let reply = result?.flag_ok()?;
	///     println!("{}", reply.data());
	/// }
	/// # Ok(())
	/// # }
	/// ```
	pub fn command_replies_until_timeout_iter<C>(
		&mut self,
		cmd: C,
	) -> Result<iter::ResponsesUntilTimeout<'_, 'a, B, Reply, Tag, H>, AsciiError>
	where
		C: Command,
	{
		self.internal_command_replies_until_timeout_iter(&cmd)
	}

	/// Transmit a command and get an iterator that, when used, will read replies from the port until it times out.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If any response is spread across multiple packets, continuation packets will be read.
	fn internal_command_replies_until_timeout_iter(
		&mut self,
		cmd: &dyn Command,
	) -> Result<iter::ResponsesUntilTimeout<'_, 'a, B, Reply, Tag, H>, AsciiError> {
		let id = self.command(cmd)?;
		Ok(
			self.internal_responses_until_timeout_iter(HeaderCheck::Matches {
				target: cmd.target(),
				id,
			}),
		)
	}

	/// Read the bytes for a packet.
	fn read_packet_bytes(&mut self) -> Result<Vec<u8>, AsciiError> {
		use crate::ascii::packet::AsciiExt as _;

		let mut buf = Vec::with_capacity(100);
		let mut found_start = false;

		// Read the first byte at the original timeout,
		let byte = std::io::Read::bytes(&mut self.backend).next().unwrap()?;
		if byte.is_packet_start() {
			buf.push(byte);
			found_start = true;
		}

		// Read the reset of the bytes at the inter-char timeout, unless the
		// specified timeout is even shorter. Packets should be sent all at
		// once, so the time between bytes should be smaller than the time
		// between packets.
		let timeout = self.backend.read_timeout()?;
		let inter_char_timeout = Duration::from_millis(300);
		let effective_timeout = timeout.unwrap_or(Duration::MAX);
		let use_inter_char_timeout = inter_char_timeout < effective_timeout;
		if use_inter_char_timeout {
			self.backend.set_read_timeout(Some(inter_char_timeout))?;
		}
		let result = || -> Result<(), AsciiError> {
			for byte in std::io::Read::bytes(&mut self.backend) {
				let byte = byte?;
				if byte.is_packet_start() {
					if found_start {
						// We are already in the middle of a packet. Something has gone wrong.
						return Err(AsciiPacketMissingEndError::new(buf.clone()).into());
					}
					found_start = true;
				}
				if found_start {
					buf.push(byte);
				}
				if byte == crate::ascii::packet::LINE_FEED {
					break;
				}
			}
			Ok(())
		}();
		// Make sure to restore the timeout if we need to
		if use_inter_char_timeout {
			self.backend.set_read_timeout(timeout)?;
		}
		if !found_start {
			return Err(AsciiPacketMissingStartError::new(buf).into());
		}
		if buf.is_empty() || *buf.last().unwrap() != crate::ascii::packet::LINE_FEED {
			return Err(AsciiPacketMissingEndError::new(buf).into());
		}
		result.map(move |()| buf)
	}

	/// Receive a response [`Packet`]
	///
	/// The packet's LRC is verified, and guaranteed not to be a Command packet.
	/// The contents of the packet are otherwise unchecked.
	fn response_packet(&mut self) -> Result<Packet, AsciiError> {
		let backend_name = self
			.backend
			.name()
			.unwrap_or_else(|| UNKNOWN_BACKEND_NAME.to_string());

		let raw_packet = self.read_packet_bytes()?;
		// Log the packet
		log::debug!(
			"{} RECV: {}",
			&backend_name,
			String::from_utf8_lossy(&raw_packet).trim_end()
		);

		if let Some(callback) = self.handlers.packet() {
			(callback)(raw_packet.as_slice(), Direction::Recv);
		}

		// Parse the packet.
		let packet = Packet::try_from(&*raw_packet)?;
		// Verify the checksum, if one exists
		if let Some(checksum) = packet.checksum() {
			if !Lrc::verify(packet.hashed_content(), checksum) {
				return Err(AsciiInvalidChecksumError::new(packet).into());
			}
		}
		// Make sure it isn't a command packet
		if packet.kind() == PacketKind::Command {
			Err(AsciiUnexpectedPacketError::new(packet).into())
		} else {
			Ok(packet)
		}
	}

	/// Read packets until we build up a complete response message.
	fn build_response(&mut self) -> Result<AnyResponse, AsciiError> {
		loop {
			// See if we already have a response built.
			if let Some(response) = self.builder.get_complete_response() {
				return Ok(response);
			}

			// There is no response built so we we need to read in another packet.
			// If doing so causes the port to timeout, or produce any other error,
			// we should report it immediately. At most we have a partially
			// completed response that will be lost.
			let packet = self.response_packet()?;
			self.builder.push(packet)?;
		}
	}

	/// Perform any work necessary to start reading responses with [`receive_response`].
	/// In particular it clears the `builder`.
	///
	/// This function should be called before the first time [`receive_response`]
	/// is called.
	#[inline]
	fn pre_receive_response(&mut self) {
		self.builder.clear();
	}

	/// Perform any work necessary to clean up after receiving one or more
	/// responses with [`receive_response`]. In particular, it will ensure the
	/// builder is empty and raise an error for any data remaining in the
	/// builder.
	///
	/// This function should be called after the last time [`receive_response`]
	/// is called.
	fn post_receive_response(&mut self) -> Result<(), AsciiError> {
		let mut inner = || -> Result<(), AsciiError> {
			if let Some(response) = self.builder.get_complete_response() {
				return Err(AsciiUnexpectedResponseError::new(response).into());
			}
			if let Some(packet) = self.builder.get_incomplete_response_packet() {
				return Err(AsciiUnexpectedPacketError::new(packet).into());
			}
			// There must not be any data remaining.
			Ok(())
		};

		if let Some(callback) = &mut self.handlers.unexpected_alert() {
			// There is an handler for alerts, check if we need to call it for any remaining responses.
			loop {
				match inner() {
					Ok(()) => return Ok(()), // There is no data remaining
					Err(AsciiError::UnexpectedResponse(err)) => {
						let response = err.into();
						match response {
							AnyResponse::Alert(alert) => match (callback)(alert) {
								// The handler accepted the alert, so continue checking any other messages.
								Ok(()) => {}
								// The handler did not accept the alert, so return the error.
								Err(alert) => {
									self.builder.clear();
									return Err(AsciiUnexpectedResponseError::new(alert).into());
								}
							},
							// Although this is an unexpected response, it isn't an alert.
							response => {
								self.builder.clear();
								return Err(AsciiUnexpectedResponseError::new(response).into());
							}
						}
					}
					// This isn't an unexpected response
					err => {
						self.builder.clear();
						return err;
					}
				}
			}
		} else {
			// There is no handle for unexpected response, so simply report any
			// errors directly to the caller.
			let result = inner();
			self.builder.clear();
			result
		}
	}

	/// Receiving a response.
	///
	/// Prior to calling this function for the first time, `pre_receive_response`
	/// should be called. After the last call to this function,
	/// `post_receive_response` should be called. Both of these functions
	/// ensure `self.builder` is in the appropriate state and that all errors
	/// are appropriately handled.
	///
	/// If the response is spread across multiple packets, continuation packets will be read.
	/// `header_check` should be a function that produces data for validating the response's header.
	///
	/// If the `header_check` passes, the message will be converted to the desired message type `R`.
	fn receive_response<R>(
		&mut self,
		header_check: HeaderCheck,
	) -> Result<NotChecked<R>, AsciiError>
	where
		R: Response,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
	{
		self.check_poisoned()?;
		loop {
			let result = || -> Result<NotChecked<R>, AsciiError> {
				let mut response = self.build_response()?;
				response = header_check.check(response)?;
				R::try_from(response)
					.map(NotChecked::new)
					.map_err(AsciiUnexpectedResponseError::new)
					.map_err(From::from)
			}();
			if let Some(callback) = &mut self.handlers.unexpected_alert() {
				// There is an handler for alerts, check if we need to call it.
				match result {
					Err(AsciiError::UnexpectedResponse(err)) => {
						let response = err.into();
						match response {
							AnyResponse::Alert(alert) => match (callback)(alert) {
								// The handler accepted the alert, so receive another response.
								Ok(()) => {}
								// The handler did not accept the alert, so return the error.
								Err(alert) => {
									return Err(AsciiUnexpectedResponseError::new(alert).into());
								}
							},
							// Although this is an unexpected response, it isn't an alert.
							_ => return Err(AsciiUnexpectedResponseError::new(response).into()),
						}
					}
					// This isn't an unexpected response
					_ => return result,
				}
			} else {
				// There is no handler to try
				return result;
			}
		}
	}

	fn internal_response_n_iter<R>(
		&mut self,
		n: usize,
		header_check: HeaderCheck,
	) -> iter::NResponses<'_, 'a, B, R, Tag, H>
	where
		R: Response,
	{
		iter::NResponses::new(self, header_check, n)
	}

	/// Receive a response.
	///
	/// The type of response must be specified: [`Reply`], [`Info`], [`Alert`], or [`AnyResponse`].
	///
	/// If the response is split across multiple packets, the continuation messages will automatically be read.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::{response::{Reply, Info}, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// let reply: Reply = port.response()?.check_minimal()?;
	/// let info: Info = port.response()?.check_minimal()?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn response<R>(&mut self) -> Result<NotChecked<R>, AsciiError>
	where
		R: Response,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
	{
		self.pre_receive_response();
		let response = self.receive_response(HeaderCheck::DoNotCheck)?;
		self.post_receive_response()?;
		Ok(response)
	}

	/// Generate an iterator that will read `n` response of type `R` from the port when used.
	///
	/// The type of response must be specified: [`Reply`], [`Info`], [`Alert`], or [`AnyResponse`].
	///
	/// If the response is split across multiple packets, the continuation messages will automatically be read.
	///
	/// The iterator produces a `Result<NotChecked<R>>`, which must be handled by the caller on each iteration.
	///
	/// To simply check and collect all the responses into a vector, use [`response_n`](Port::response_n).
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::{ascii::{response::Info, Port}, backend::Backend};
	/// # fn do_something_with(_info: Info) {}
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// for result in port.response_n_iter(3) {
	///     /// Handle any communication errors and check the contents of the response.
	///     let response: Info = result?.check_minimal()?;
	///     do_something_with(response);
	/// }
	/// # Ok(())
	/// # }
	/// ```
	pub fn response_n_iter<R>(&mut self, n: usize) -> iter::NResponses<'_, 'a, B, R, Tag, H>
	where
		R: Response,
	{
		self.internal_response_n_iter(n, HeaderCheck::DoNotCheck)
	}

	/// Receive `n` responses, collecting them into a vector. Each one is checked with the custom [`Check`](check::Check).
	///
	/// The type of response must be specified: [`Reply`], [`Info`], [`Alert`], or [`AnyResponse`].
	///
	/// If the response is split across multiple packets, the continuation messages will automatically be read.
	///
	/// To avoid collecting the responses into a vector use [`response_n_iter`](Port::response_n_iter).
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::{response::Info, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::ascii::response::check::unchecked;
	/// let reply: Vec<Info> = port.response_n(3, unchecked())?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn response_n<R, K>(&mut self, n: usize, checker: K) -> Result<Vec<R>, AsciiError>
	where
		R: Response,
		K: check::Check<R>,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
	{
		let mut responses = Vec::new();
		let checker: &dyn check::Check<R> = &checker;
		for result in self.internal_response_n_iter(n, HeaderCheck::DoNotCheck) {
			responses.push(result?.check(checker)?);
		}
		Ok(responses)
	}

	/// Receive responses until the port times out and validate each one with the custom [`Check`](check::Check).
	///
	/// The type of response must be specified: [`Reply`], [`Info`], [`Alert`], or [`AnyResponse`].
	///
	/// If any of the responses are split across multiple packets, the continuation messages will automatically be read.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::ascii::{response::AnyResponse, Port};
	/// # use zproto::backend::Backend;
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::ascii::response::check::minimal;
	/// let reply: Vec<AnyResponse> = port.responses_until_timeout(minimal())?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn responses_until_timeout<R, K>(&mut self, checker: K) -> Result<Vec<R>, AsciiError>
	where
		R: Response,
		K: check::Check<R>,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
	{
		let mut responses = Vec::new();
		let checker: &dyn check::Check<_> = &checker;
		for result in self.internal_responses_until_timeout_iter(HeaderCheck::DoNotCheck) {
			responses.push(result?.check(checker)?);
		}
		Ok(responses)
	}

	/// Generate an iterator that will read responses of type `R` from the port until it times out.
	///
	/// The type of response must be specified: [`Reply`], [`Info`], [`Alert`], or [`AnyResponse`].
	///
	/// If the response is split across multiple packets, the continuation messages will automatically be read.
	///
	/// The iterator produces a `Result<NotChecked<R>>`, which must be handled by the caller on each iteration.
	///
	/// To simply check and collect all the responses into a vector, use [`responses_until_timeout`](Port::responses_until_timeout).
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::{ascii::{response::Info, Port}, backend::Backend};
	/// # fn do_something_with(_info: Info) {}
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// for result in port.responses_until_timeout_iter() {
	///     /// Handle any communication errors and check the contents of the response.
	///     let response: Info = result?.check_minimal()?;
	///     do_something_with(response);
	/// }
	/// # Ok(())
	/// # }
	/// ```
	pub fn responses_until_timeout_iter<R>(
		&mut self,
	) -> iter::ResponsesUntilTimeout<'_, 'a, B, R, Tag, H>
	where
		R: Response,
	{
		self.internal_responses_until_timeout_iter(HeaderCheck::DoNotCheck)
	}

	fn internal_responses_until_timeout_iter<R>(
		&mut self,
		header_check: HeaderCheck,
	) -> iter::ResponsesUntilTimeout<'_, 'a, B, R, Tag, H>
	where
		R: Response,
	{
		iter::ResponsesUntilTimeout::new(self, header_check)
	}

	/// Return a iterator that will repeatedly send `command` and read a reply.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::ascii::{Port, response::Reply};
	/// # use zproto::backend::Backend;
	/// # use zproto::error::AsciiError;
	/// # fn wrapper<B: Backend>(port: &mut Port<'_, B>) -> Result<(), AsciiError> {
	/// for result in port.poll("get pos") {
	///     let reply = result?.flag_ok()?;
	///     let pos: i32 = reply.data().parse().unwrap();
	///     println!("{pos}");
	///     if pos > 50_000 {
	///         break
	///     }
	/// }
	/// # Ok(())
	/// # }
	/// ```
	pub fn poll<C>(&mut self, command: C) -> iter::Poll<'_, 'a, B, C, Tag, H>
	where
		C: Command,
	{
		iter::Poll {
			port: self,
			command,
		}
	}

	/// Send the specified command repeatedly until the predicate returns true
	/// for a reply.
	///
	/// The first reply to satisfy the predicate is returned. The contents of
	/// the replies are checked with the specified `checker`.
	///
	/// If necessary, the command will be split into multiple packets so that no
	/// packet is longer than [`max_packet_size`](Self::max_packet_size).
	///
	/// If any of the replies are split across multiple packets, the
	/// continuation messages will automatically be read.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::{response::check, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// port.poll_until(
	///     (1, 1, ""),
	///     check::flag_ok(),
	///     |reply| reply.warning() != "FZ"
	/// )?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn poll_until<C, K, F>(
		&mut self,
		cmd: C,
		checker: K,
		predicate: F,
	) -> Result<Reply, AsciiError>
	where
		C: Command,
		K: check::Check<Reply>,
		F: FnMut(&Reply) -> bool,
	{
		self.internal_poll_until(&cmd, &checker, predicate)
	}

	/// Send the specified command repeatedly until the predicate returns true
	/// for a reply.
	///
	/// If necessary, the command will be split into multiple packets so that no
	/// packet is longer than [`max_packet_size`](Self::max_packet_size).
	///
	/// If any response is spread across multiple packets, continuation packets will be read.
	fn internal_poll_until<F>(
		&mut self,
		cmd: &dyn Command,
		checker: &dyn check::Check<Reply>,
		mut predicate: F,
	) -> Result<Reply, AsciiError>
	where
		F: FnMut(&Reply) -> bool,
	{
		let mut reply;
		loop {
			reply = self.internal_command_reply(cmd)?.check(checker)?;
			if predicate(&reply) {
				break;
			}
		}
		Ok(reply)
	}

	/// Poll the target with the empty command until the returned status is IDLE.
	///
	/// The first reply with the IDLE status is returned. The contents of
	/// the replies are checked with the specified `checker`.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::{ascii::{response::check, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// port.poll_until_idle((1,1), check::flag_ok())?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn poll_until_idle<T, K>(&mut self, target: T, checker: K) -> Result<Reply, AsciiError>
	where
		T: Into<Target>,
		K: check::Check<Reply>,
	{
		self.internal_poll_until(&(target.into(), ""), &checker, |reply| {
			reply.status() == Status::Idle
		})
	}

	/// Set the port timeout and return a "scope guard" that will reset the timeout when it goes out of scope.
	///
	/// If not timeout is specified, reads can block indefinitely.
	///
	/// While the guard is in scope, the port can only be accessed through the guard.
	/// However, because the guard implements [`Deref`](std::ops::Deref) and [`DerefMut`](std::ops::DerefMut) callers can treat the guard as the port.
	///
	/// ## Example
	/// ```rust
	/// # use zproto::{error::AsciiError, ascii::{Port, response::Reply}, backend::Backend};
	/// # use std::time::Duration;
	/// # fn helper<B: Backend>(mut port: Port<B>) -> Result<Reply, AsciiError> {
	/// {
	///     let mut guard = port.timeout_guard(Some(Duration::from_secs(3)))?;
	///     // All commands within this scope will use a 3 second timeout
	///     guard.command_reply("system reset")?.flag_ok()?;
	///
	/// }  // The guard is dropped and the timeout is reset.
	///
	/// // This command-reply uses the original timeout
	/// # Ok(
	/// port.command_reply("get device.id")?.flag_ok()?
	/// # )
	/// # }
	/// ```
	pub fn timeout_guard(
		&mut self,
		timeout: Option<Duration>,
	) -> Result<TimeoutGuard<'_, B, Self>, io::Error> {
		self.check_poisoned()?;

		TimeoutGuard::new(self, timeout)
	}

	/// Set whether commands sent on this port should include a checksum or not.
	///
	/// The previous value is returned.
	pub fn set_checksums(&mut self, value: bool) -> bool {
		std::mem::replace(&mut self.generate_checksum, value)
	}

	/// Get whether the port will include checksums or not in commands.
	pub fn checksums(&self) -> bool {
		self.generate_checksum
	}

	/// Set whether commands sent on this port should include an automatically generated message ID or not.
	///
	/// The previous value is returned.
	pub fn set_message_ids(&mut self, value: bool) -> bool {
		std::mem::replace(&mut self.generate_id, value)
	}

	/// Get whether the port will include message IDs or not in commands.
	pub fn message_ids(&self) -> bool {
		self.generate_id
	}

	/// Set the maximum command packet size.
	///
	/// The previous value is returned.
	pub fn set_max_packet_size(&mut self, value: MaxPacketSize) -> MaxPacketSize {
		std::mem::replace(&mut self.max_packet_size, value)
	}

	/// Get the maximum command packet size.
	pub fn max_packet_size(&self) -> MaxPacketSize {
		self.max_packet_size
	}

	/// Set the read timeout and return the old timeout.
	///
	/// If timeout is `None`, reads will block indefinitely.
	pub fn set_read_timeout(
		&mut self,
		timeout: Option<Duration>,
	) -> Result<Option<Duration>, io::Error> {
		let old = self.backend.read_timeout()?;
		self.backend.set_read_timeout(timeout)?;
		Ok(old)
	}

	/// Get the read timeout.
	///
	/// If it is `None`, reads will block indefinitely.
	pub fn read_timeout(&self) -> Result<Option<Duration>, io::Error> {
		self.backend.read_timeout()
	}

	/// Get the "name" of the port's backend.
	///
	/// This is often the "name" passed to [`Port::open_serial`] or [`Port::open_tcp`].
	pub fn name(&self) -> Option<String> {
		self.backend.name()
	}

	/// Get a referenced to the backend.
	pub fn backend(&self) -> &B {
		&self.backend
	}

	/// Get a mutable reference to the backend.
	pub fn backend_mut(&mut self) -> &mut B {
		&mut self.backend
	}

	/// Consume the port and return the underlying backend.
	///
	/// Note that any data the port has buffered will be lost. Callers should
	/// ensure that all expected data has been sent and received.
	pub fn into_backend(self) -> B {
		self.backend
	}

	/// Create a [`Chain`].
	pub fn chain(&mut self) -> Result<Chain<Tag>, AsciiError> {
		Chain::new(self)
	}

	/// Converts the specified `item` into a [`Routine`] and runs it, returning the result.
	pub fn run<R: IntoRoutine<Self>>(&mut self, item: R) -> Result<R::Output, R::Error> {
		item.into_routine().run(self)
	}

	/// Set a callback that will be called immediately after any ASCII packet is
	/// sent or received.
	///
	/// If a previous callback was set, it is returned.
	///
	/// To clear a previously registered callback use [`clear_packet_handler`](Port::clear_packet_handler).
	///
	/// The callback will be passed the raw bytes of a possible packet and the
	/// direction of the packet. The bytes are not guaranteed to be a valid
	/// ASCII packet. Parse the bytes with [`Packet`] or [`Tokens`](crate::ascii::packet::Tokens)
	/// to inspect the contents of the packet.
	///
	/// Note, the Port already logs packets (along with other metadata) via the
	/// [`log`] crate, so logging is best handled via a log handler, such as
	/// [`simple_logger`](https://crates.io/crates/simple_logger), rather than
	/// a packet callback. However, there are instances when you need access to
	/// the packets directly (for instance, to show them in an application),
	/// which is when a packet callback is most useful.
	///
	/// ## Examples
	///
	/// Any closure can be used as a callback.
	///
	/// ```
	/// # use zproto::ascii::Port;
	/// #
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// # let mut port = Port::open_serial("...")?;
	/// port.set_packet_handler(|packet, dir| {
	///     println!("{dir:?}: {packet:?}");
	/// });
	/// # Ok(())
	/// # }
	/// ```
	///
	/// However, if the closure captures any variables those variables must
	/// either be moved into the closure with the `move` keyword (e.g., `move |packet, dir| {...}`)
	/// or live at least as long as the `Port` instance. Additionally, if those
	/// variables are mutated but also accessed outside of the closure, then a
	/// [`RefCell`](std::cell::RefCell)/[`Mutex`](std::sync::Mutex) should be
	/// used to facilitate the sharing.
	///
	/// ```
	/// # use zproto::ascii::Port;
	/// # use std::cell::RefCell;
	/// #
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// let packet_list = RefCell::new(Vec::new());
	/// let mut port = Port::open_serial("...")?;
	/// port.set_packet_handler(|packet, _| {
	///     if let Ok(mut packets) = packet_list.try_borrow_mut() {
	///         packets.push(String::from_utf8_lossy(packet).into_owned());
	///     }
	/// });
	///
	/// port.command_reply((1, "home"));
	///
	/// for packet_str in packet_list.borrow().iter() {
	///     println!("{packet_str}");
	/// }
	/// # Ok(())
	/// # }
	/// ```
	pub fn set_packet_handler<F>(&mut self, callback: F) -> Option<H::PacketHandler>
	where
		H::PacketHandler: crate::convert::From<F>,
	{
		std::mem::replace(
			self.handlers.packet(),
			Some(crate::convert::From::from(callback)),
		)
	}

	/// Clear any callback registered via [`set_packet_handler`](Port::set_packet_handler) and return it.
	pub fn clear_packet_handler(&mut self) -> Option<H::PacketHandler> {
		self.handlers.packet().take()
	}

	/// Set a callback that will be called whenever an unexpected Alert is
	/// received.
	///
	/// If a previous callback was set, it is returned.
	///
	/// To clear a previously registered callback use [`clear_unexpected_alert_handler`](Port::clear_unexpected_alert_handler).
	///
	/// If the callback consumes the alert, returning `Ok(())`, the unexpected
	/// alert will not be reported to the caller of whatever method read the
	/// alert, and another response will be read in its place. If the callback
	/// does not consume the alert, returning it as an `Err`, whatever method
	/// read the alert will return it as an [`AsciiUnexpectedResponseError`].
	///
	/// Note that a `Port` does not try to read a response unless the caller
	/// explicitly calls a method to do so. So any Alert sent while the `Port`
	/// is not reading will not trigger this callback. Furthermore, explicitly
	/// reading an alert will also not trigger this callback -- only unexpected
	/// alert messages will trigger this callback.
	///
	/// ## Example
	///
	///
	/// ```
	/// # use std::cell::Cell;
	/// #
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::ascii::{Port, response::check::minimal};
	///
	/// let mut port = Port::open_serial("...")?;
	///
	/// // Read a potentially large number of info messages. However, to ensure
	/// // that the read isn't interrupted by any unexpected alerts, first
	/// // configure the port to effectively ignore the alerts it may receive by
	/// // dropping them.
	/// port.set_unexpected_alert_handler(|_alert| Ok(()));
	/// let (_reply, _infos) = port.command_reply_infos((1, "storage all print"), minimal())?;
	/// // ...
	/// # Ok(())
	/// # }
	/// ```
	pub fn set_unexpected_alert_handler<F>(
		&mut self,
		callback: F,
	) -> Option<H::UnexpectedAlertHandler>
	where
		H::UnexpectedAlertHandler: crate::convert::From<F>,
	{
		std::mem::replace(
			self.handlers.unexpected_alert(),
			Some(crate::convert::From::from(callback)),
		)
	}

	/// Clear any callback registered via [`set_unexpected_alert_handler`](Port::set_unexpected_alert_handler) and return it.
	pub fn clear_unexpected_alert_handler(&mut self) -> Option<H::UnexpectedAlertHandler> {
		self.handlers.unexpected_alert().take()
	}
}

impl<'a, B, Tag, H> io::Write for Port<'a, B, Tag, H>
where
	B: Backend,
	H: Handlers,
	H::PacketHandler: FnMut(&[u8], Direction) + 'a,
	H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'a,
{
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		self.check_poisoned()?;
		self.backend.write(buf)
	}

	fn flush(&mut self) -> io::Result<()> {
		self.check_poisoned()?;
		self.backend.flush()
	}
}

impl<'a, B, Tag, H> io::Read for Port<'a, B, Tag, H>
where
	B: Backend,
	H: Handlers,
	H::PacketHandler: FnMut(&[u8], Direction) + 'a,
	H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'a,
{
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		self.check_poisoned()?;
		self.backend.read(buf)
	}
}

impl<'a, B: Backend, Tag, H> crate::timeout_guard::Port<B> for Port<'a, B, Tag, H> {
	fn backend_mut(&mut self) -> &mut B {
		&mut self.backend
	}
	fn poison(&mut self, e: io::Error) {
		self.poison = Some(e);
	}
}

/// How the header of a message should be checked.
#[derive(Debug, Copy, Clone)]
pub(super) enum HeaderCheck {
	/// Do not check the header
	DoNotCheck,
	/// Check that the header has a target and ID that matches these.
	Matches { target: Target, id: Option<u8> },
	/// Check that all responses have the specified target. Replies should have
	/// the `sentinel_id` and info messages should have the `info_id`.
	InfoSentinelReplyMatches {
		target: Target,
		info_id: Option<u8>,
		sentinel_id: Option<u8>,
	},
}

impl HeaderCheck {
	fn check(self, response: AnyResponse) -> Result<AnyResponse, AsciiError> {
		use HeaderCheck as HC;
		match self {
			HC::DoNotCheck => Ok(response),
			HC::Matches { target, id } => {
				if !response.target().elicited_by_command_to(target) || response.id() != id {
					Err(AsciiUnexpectedResponseError::new(response).into())
				} else {
					Ok(response)
				}
			}
			HC::InfoSentinelReplyMatches {
				target,
				info_id,
				sentinel_id,
			} => {
				if !response.target().elicited_by_command_to(target) {
					return Err(AsciiUnexpectedResponseError::new(response).into());
				}
				match response {
					AnyResponse::Info(ref info) if info.id() == info_id => Ok(response),
					AnyResponse::Reply(ref reply) if reply.id() == sentinel_id => Ok(response),
					_ => Err(AsciiUnexpectedResponseError::new(response).into()),
				}
			}
		}
	}
}
