//! Types for opening and using a serial port with the ASCII protocol.

mod options;
#[cfg(test)]
mod test;

use crate::backend::{Backend, Serial, UNKNOWN_BACKEND_NAME};
use crate::{
	ascii::{
		chain::{Chain, SyncChain},
		check::{self, NotChecked},
		checksum::Lrc,
		id,
		parse::{Packet, PacketKind},
		Alert, AnyResponse, Command, CommandWriter, Info, MaxPacketSize, Reply, Response,
		ResponseBuilder, Status, Target,
	},
	error::*,
	timeout_guard::TimeoutGuard,
};
pub use options::*;
use std::{
	convert::TryFrom,
	io,
	net::{TcpStream, ToSocketAddrs},
	time::Duration,
};

/// A callback that is called after a packet is either transmitted or received.
///
/// See [`Port::set_packet_handler`] for more details.
pub type PacketCallback<'a> = Box<dyn FnMut(&[u8], Direction) + 'a>;

/// A wrapper around an [`PacketCallback`] that simply implements `Debug` so
/// that the [`Port`] can derive `Debug`.
#[repr(transparent)]
struct PacketCallbackDebugWrapper<'a>(PacketCallback<'a>);

impl<'a> std::fmt::Debug for PacketCallbackDebugWrapper<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "PacketCallback")
	}
}

/// A callback that is called when an unexpected Alert is received.
///
/// See [`Port::set_unexpected_alert_handler`] for more details.
pub type UnexpectedAlertCallback<'a> = Box<dyn FnMut(Alert) -> Result<(), Alert> + 'a>;

/// A wrapper around an [`UnexpectedAlertCallback`] that simply implements `Debug` so
/// that the [`Port`] can derive `Debug`.
#[repr(transparent)]
struct UnexpectedAlertDebugWrapper<'a>(UnexpectedAlertCallback<'a>);

impl<'a> std::fmt::Debug for UnexpectedAlertDebugWrapper<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "UnexpectedAlertCallback")
	}
}

/// The direction a packet was sent.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Direction {
	/// The packet was transmitted to a device.
	Tx,
	/// The packet was received from a device.
	Recv,
}

/// A port configured to use the ASCII protocol.
///
/// A port is parameterized by some [`Backend`] type, `B`. Use the convenience
/// methods [`Port::open_serial`] and [`Port::open_tcp`] to construct a serial
/// port (`Port<Serial>`) or a TCP port (`Port<TcpStream>`). To customize the
/// construction of these types, or to construct a port with a dynamic backend,
/// use the [`OpenSerialOptions`] and [`OpenTcpOptions`] builder types.
///
/// See the [`ascii`](crate::ascii) module-level documentation for more details.
#[derive(Debug)]
pub struct Port<'a, B> {
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
	///  * cannot be recovered from,
	///  * panicking is ill advised,
	///  * and it is safe to delay reporting of the error until the next attempt
	///    to communicate over the port.
	/// For instance, if a [`TimeoutGuard`] cannot restore the original timeout
	/// in its Drop implementation, rather than panicking (which would almost
	/// certainly cause the program to abort rather than unwind the stack) it
	/// can poison the port.
	poison: Option<io::Error>,
	/// The builder used to concatenate packets in to responses.
	builder: ResponseBuilder,
	/// Optional hook to call after a packet is sent/received.
	packet_hook: Option<PacketCallbackDebugWrapper<'a>>,
	/// Optional hook to call when an unexpected Alert is received.
	unexpected_alert_hook: Option<UnexpectedAlertDebugWrapper<'a>>,
}

impl<'a> Port<'a, Serial> {
	/// Open the serial port at the specified path using the default options.
	///
	/// Alternatively, use [`OpenSerialOptions`] to customize how the port is opened.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::ascii::{OpenSerialOptions, Port};
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// let mut port = Port::open_serial("/dev/ttyUSB0")?;
	/// // Or equivalently
	/// let mut port = OpenSerialOptions::new().open("/dev/ttyUSB0")?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn open_serial(path: &str) -> Result<Port<'a, Serial>, AsciiError> {
		OpenSerialOptions::new().open(path)
	}
}

impl<'a> Port<'a, TcpStream> {
	/// Open the TCP port at the specified address using the default options.
	///
	/// Alternatively, use [`OpenTcpOptions`] to customize how the port is opened.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::ascii::{OpenTcpOptions, Port};
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// let mut port = Port::open_tcp("198.168.0.1:55550")?;
	/// // Or equivalently
	/// let mut port = OpenTcpOptions::new().open("198.168.0.1:55550")?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn open_tcp<A: ToSocketAddrs>(address: A) -> Result<Port<'a, TcpStream>, io::Error> {
		OpenTcpOptions::new().open(address)
	}
}

impl<'a, B: Backend> Port<'a, B> {
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
			packet_hook: None,
			unexpected_alert_hook: None,
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
			if let Some(ref mut callback) = self.packet_hook {
				(callback.0)(buffer.as_slice(), Direction::Tx);
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
	/// # use zproto::{ascii::{Port, Reply}, backend::Backend};
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
		let response = self.receive_response(|_| HeaderCheckAction::Check {
			target: cmd.target(),
			id,
		})?;
		self.post_receive_response()?;
		Ok(response)
	}

	/// Transmit a command and then receive a reply and all subsequent info messages.
	///
	/// The reply and info messages are checked with the [`strict`](check::strict) check.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If the reply or info messages are split across multiple packets, the continuation messages will automatically be read.
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
	/// ```rust
	/// # use zproto::{ascii::Port, backend::Backend, error::AsciiError};
	/// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), AsciiError> {
	/// let (reply, info_messages) = port.command_reply_infos("stream buffer 1 print")?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn command_reply_infos<C: Command>(
		&mut self,
		cmd: C,
	) -> Result<(Reply, Vec<Info>), AsciiError> {
		self.internal_command_reply_infos_with_check(&cmd, &check::strict())
	}

	/// Same as [`Port::command_reply_infos`] except that the messages are validated with the custom [`Check`](check::Check).
	///
	/// ## Example
	/// ```rust
	/// # use zproto::{
	/// #     ascii::{Port, AnyResponse},
	/// #     backend::Backend,
	/// #     error::{AsciiCheckError, AsciiError}
	/// # };
	/// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), AsciiError> {
	/// let (reply, info_messages) = port.command_reply_infos_with_check(
	///    (1, "stream buffer 1 print"),
	///    |response| match response {
	///        // Don't check replies or info messages, but error on alerts
	///        AnyResponse::Reply(_) | AnyResponse::Info(_) => Ok(response),
	///        AnyResponse::Alert(_) => Err(AsciiCheckError::custom("Alerts are not allowed!", response))
	///    }
	/// )?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn command_reply_infos_with_check<C, K>(
		&mut self,
		cmd: C,
		checker: K,
	) -> Result<(Reply, Vec<Info>), AsciiError>
	where
		C: Command,
		K: check::Check<AnyResponse>,
	{
		self.internal_command_reply_infos_with_check(&cmd, &checker)
	}

	/// Transmit a command and then receive a reply and all subsequent info messages.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If any response is spread across multiple packets, continuation packets will be read.
	fn internal_command_reply_infos_with_check(
		&mut self,
		cmd: &dyn Command,
		checker: &dyn check::Check<AnyResponse>,
	) -> Result<(Reply, Vec<Info>), AsciiError> {
		let target = cmd.target();
		// It should be reasonably safe to unwrap here. All checks implemented by this crate return the same response
		// they received. It is possible for a user to create their own check that doesn't do that but it would
		// be hard to do since only this crate can create responses (users can create packets from bytes, but
		// they can't create `Reply`s, `Info`s, or `Alert`s directly).
		let reply_checker = |reply: Reply| {
			checker
				.check(AnyResponse::from(reply))
				.map(|response| Reply::try_from(response).unwrap())
				.map_err(|err| AsciiCheckError::<Reply>::try_from(err).unwrap())
		};
		let reply = self.internal_command_reply(cmd)?.check(reply_checker)?;
		let old_generate_id = self.set_message_ids(true);
		let sentinel_id = self.command((target, ""));
		self.set_message_ids(old_generate_id);
		let sentinel_id = sentinel_id?;
		let mut infos = Vec::new();
		let header_check = |response: &AnyResponse| match response {
			AnyResponse::Info(_) => HeaderCheckAction::Check {
				target,
				id: reply.id(),
			},
			AnyResponse::Reply(_) => HeaderCheckAction::Check {
				target,
				id: sentinel_id,
			},
			_ => HeaderCheckAction::Unexpected,
		};
		self.pre_receive_response();
		loop {
			match self.receive_response(&header_check)?.check(checker)? {
				AnyResponse::Info(info) => infos.push(info),
				AnyResponse::Reply(_) => {
					// This is the reply we've been waiting for. Stop.
					break;
				}
				_ => unreachable!(),
			}
		}
		self.post_receive_response()?;
		Ok((reply, infos))
	}

	/// Transmit a command, receive n replies, and check each reply with the [`strict`](check::strict) check.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If any of the replies are split across multiple packets, the continuation messages will automatically be read.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::Port, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// let replies = port.command_reply_n("get system.serial", 5)?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn command_reply_n<C: Command>(
		&mut self,
		cmd: C,
		n: usize,
	) -> Result<Vec<Reply>, AsciiError> {
		self.internal_command_reply_n_with_check(&cmd, n, &check::strict())
	}

	/// Same as [`Port::command_reply_n`] except that the replies are validated with the custom [`Check`](check::Check).
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::Port, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::ascii::check::unchecked;
	///
	/// let replies = port.command_reply_n_with_check("get system.serial", 5, unchecked())?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn command_reply_n_with_check<C, K>(
		&mut self,
		cmd: C,
		n: usize,
		checker: K,
	) -> Result<Vec<Reply>, AsciiError>
	where
		C: Command,
		K: check::Check<Reply>,
	{
		self.internal_command_reply_n_with_check(&cmd, n, &checker)
	}

	/// Transmit a command and then receive n replies.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If any response is spread across multiple packets, continuation packets will be read.
	fn internal_command_reply_n_with_check(
		&mut self,
		cmd: &dyn Command,
		n: usize,
		checker: &dyn check::Check<Reply>,
	) -> Result<Vec<Reply>, AsciiError> {
		let id = self.command(cmd)?;
		self.internal_response_n_with_check(
			n,
			|_| HeaderCheckAction::Check {
				target: cmd.target(),
				id,
			},
			checker,
		)
	}

	/// Transmit a command, receive replies until the port times out, and check each reply with the [`strict`](check::strict) check.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If any of the replies are split across multiple packets, the continuation messages will automatically be read.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::Port, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// let replies = port.command_replies_until_timeout("get system.serial")?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn command_replies_until_timeout<C: Command>(
		&mut self,
		cmd: C,
	) -> Result<Vec<Reply>, AsciiError> {
		self.internal_command_replies_until_timeout_with_check(&cmd, &check::strict())
	}

	/// Same as [`Port::command_replies_until_timeout`] except that the replies are validated with the custom [`Check`](check::Check).
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::Port, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::ascii::check::flag_ok;
	///
	/// let replies = port.command_replies_until_timeout_with_check(
	///     "get system.serial",
	///     flag_ok(),
	/// )?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn command_replies_until_timeout_with_check<C, K>(
		&mut self,
		cmd: C,
		checker: K,
	) -> Result<Vec<Reply>, AsciiError>
	where
		C: Command,
		K: check::Check<Reply>,
	{
		self.internal_command_replies_until_timeout_with_check(&cmd, &checker)
	}

	/// Transmit a command and then receive replies until the port times out.
	///
	/// If necessary, the command will be split into multiple packets so that no packet is longer than
	/// [`max_packet_size`](Self::max_packet_size).
	///
	/// If any response is spread across multiple packets, continuation packets will be read.
	fn internal_command_replies_until_timeout_with_check(
		&mut self,
		cmd: &dyn Command,
		checker: &dyn check::Check<Reply>,
	) -> Result<Vec<Reply>, AsciiError> {
		let id = self.command(cmd)?;
		self.internal_responses_until_timeout_with_check(
			|_| HeaderCheckAction::Check {
				target: cmd.target(),
				id,
			},
			checker,
		)
	}

	/// Read the bytes for a packet.
	fn read_packet_bytes(&mut self) -> Result<Vec<u8>, AsciiError> {
		use crate::ascii::parse::AsciiExt as _;

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
				if byte == crate::ascii::parse::LINE_FEED {
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
		if buf.is_empty() || *buf.last().unwrap() != crate::ascii::parse::LINE_FEED {
			return Err(AsciiPacketMissingEndError::new(buf).into());
		}
		result.map(move |_| buf)
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

		if let Some(ref mut callback) = self.packet_hook {
			(callback.0)(raw_packet.as_slice(), Direction::Recv);
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

		if let Some(UnexpectedAlertDebugWrapper(callback)) = &mut self.unexpected_alert_hook {
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
	fn receive_response<R, F>(&mut self, mut header_check: F) -> Result<NotChecked<R>, AsciiError>
	where
		R: Response,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
		F: FnMut(&AnyResponse) -> HeaderCheckAction,
	{
		self.check_poisoned()?;
		loop {
			let result = || -> Result<NotChecked<R>, AsciiError> {
				let mut response = self.build_response()?;
				response = header_check(&response).check(response)?;
				R::try_from(response)
					.map(NotChecked::new)
					.map_err(AsciiUnexpectedResponseError::new)
					.map_err(From::from)
			}();
			if let Some(UnexpectedAlertDebugWrapper(callback)) = &mut self.unexpected_alert_hook {
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

	/// Receive `n` responses.
	///
	/// If any response is spread across multiple packets, continuation packets will be read.
	/// `header_check` should be a function that produces data for validating the response's header.
	///
	/// If the `header_check` passes, the message will be converted to the desired message type `R` and then passed to
	/// `checker` to validate the contents of the message. If `checker` is `None`, then the port's default check is used.
	fn internal_response_n_with_check<R, F>(
		&mut self,
		n: usize,
		mut header_check: F,
		checker: &dyn check::Check<R>,
	) -> Result<Vec<R>, AsciiError>
	where
		R: Response,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
		F: FnMut(&AnyResponse) -> HeaderCheckAction,
	{
		self.pre_receive_response();
		let mut responses = Vec::new();
		for _ in 0..n {
			responses.push(self.receive_response(|r| header_check(r))?.check(checker)?);
		}
		self.post_receive_response()?;
		Ok(responses)
	}

	/// Receive responses until the port times out.
	///
	/// If any response is spread across multiple packets, continuation packets will be read.
	/// `header_check` should be a function that produces data for validating the response's header.
	///
	/// If the `header_check` passes, the message will be converted to the desired message type `R` and then passed to
	/// `checker` to validate the contents of the message. If `checker` is `None`, then the port's default check is used.
	fn internal_responses_until_timeout_with_check<R, F>(
		&mut self,
		mut header_check: F,
		checker: &dyn check::Check<R>,
	) -> Result<Vec<R>, AsciiError>
	where
		R: Response,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
		F: FnMut(&AnyResponse) -> HeaderCheckAction,
	{
		self.pre_receive_response();
		let mut responses = Vec::new();
		loop {
			match self.receive_response(|response| header_check(response)) {
				Ok(r) => responses.push(r.check(checker)?),
				Err(e) if e.is_timeout() => break,
				Err(e) => return Err(e),
			}
		}
		self.post_receive_response()?;
		Ok(responses)
	}

	/// Receive a response and check it with the [`strict`](check::strict) check.
	///
	/// The type of response must be specified: [`Reply`], [`Info`], [`Alert`], or [`AnyResponse`].
	///
	/// If the response is split across multiple packets, the continuation messages will automatically be read.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::{Reply, Info, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// let reply: Reply = port.response()?;
	/// let info: Info = port.response()?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn response<R>(&mut self) -> Result<R, AsciiError>
	where
		R: Response,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
	{
		self.pre_receive_response();
		let response = self
			.receive_response(|_| HeaderCheckAction::DoNotCheck)?
			.check(check::strict())?;
		self.post_receive_response()?;
		Ok(response)
	}

	/// Same as [`Port::response`] except that the response is validated with the custom [`Check`](check::Check).
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::{Alert, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::ascii::check::warning_below_fault;
	/// let reply: Alert = port.response_with_check(warning_below_fault())?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn response_with_check<R, K>(&mut self, checker: K) -> Result<R, AsciiError>
	where
		R: Response,
		K: check::Check<R>,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
	{
		self.pre_receive_response();
		let response = self
			.receive_response(|_| HeaderCheckAction::DoNotCheck)?
			.check(checker)?;
		self.post_receive_response()?;
		Ok(response)
	}

	/// Receive `n` responses and check each one with the [`strict`](check::strict) check.
	///
	/// The type of response must be specified: [`Reply`], [`Info`], [`Alert`], or [`AnyResponse`].
	///
	/// If the response is split across multiple packets, the continuation messages will automatically be read.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::{Info, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// let reply: Vec<Info> = port.response_n(3)?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn response_n<R>(&mut self, n: usize) -> Result<Vec<R>, AsciiError>
	where
		R: Response,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
	{
		self.internal_response_n_with_check(n, |_| HeaderCheckAction::DoNotCheck, &check::strict())
	}

	/// Same as [`Port::response_n`] except that the responses are validated with the custom [`Check`](check::Check).
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::{Reply, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::ascii::check::{flag_ok_and, warning_below_fault};
	///
	/// let reply: Vec<Reply> = port.response_n_with_check(3, flag_ok_and(warning_below_fault()))?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn response_n_with_check<R, K>(
		&mut self,
		n: usize,
		checker: K,
	) -> Result<Vec<R>, AsciiError>
	where
		R: Response,
		K: check::Check<R>,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
	{
		self.internal_response_n_with_check(n, |_| HeaderCheckAction::DoNotCheck, &checker)
	}

	/// Receive responses until the port times out and check each one with the [`strict`](check::strict) check.
	///
	/// The type of response must be specified: [`Reply`], [`Info`], [`Alert`], or [`AnyResponse`].
	///
	/// If any of the responses are split across multiple packets, the continuation messages will automatically be read.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::ascii::{AnyResponse, Port};
	/// # use zproto::backend::Backend;
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// let reply: Vec<AnyResponse> = port.responses_until_timeout()?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn responses_until_timeout<R>(&mut self) -> Result<Vec<R>, AsciiError>
	where
		R: Response,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
	{
		self.internal_responses_until_timeout_with_check(
			|_| HeaderCheckAction::DoNotCheck,
			&check::strict(),
		)
	}

	/// Same as [`Port::responses_until_timeout`] except that the responses are validated with the custom [`Check`](check::Check).
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::ascii::{AnyResponse, Port};
	/// # use zproto::backend::Backend;
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::ascii::check::unchecked;
	/// let reply: Vec<AnyResponse> = port.responses_until_timeout_with_check(unchecked())?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn responses_until_timeout_with_check<R, K>(
		&mut self,
		checker: K,
	) -> Result<Vec<R>, AsciiError>
	where
		R: Response,
		K: check::Check<R>,
		AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
		AsciiError: From<AsciiCheckError<R>>,
	{
		self.internal_responses_until_timeout_with_check(
			|_| HeaderCheckAction::DoNotCheck,
			&checker,
		)
	}

	/// Send the specified command repeatedly until the predicate returns true
	/// for a reply.
	///
	/// The first reply to satisfy the predicate is returned. The contents of
	/// the replies are checked with the [`strict`](check::strict)
	/// check.
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
	/// # use zproto::{ascii::Port, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// port.poll_until(
	///     (1, 1, ""),
	///     |reply| reply.warning() != "FZ"
	/// )?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn poll_until<C, F>(&mut self, cmd: C, predicate: F) -> Result<Reply, AsciiError>
	where
		C: Command,
		F: FnMut(&Reply) -> bool,
	{
		self.internal_poll_until_with_check(&cmd, predicate, &check::strict())
	}

	/// Same as [`Port::poll_until`] except that the replies are validated with
	/// the custom [`Check`](check::Check).
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::{check, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// port.poll_until_with_check(
	///     (1, 1, ""),
	///     |reply| reply.warning() != "FZ",
	///     check::flag_ok()
	/// )?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn poll_until_with_check<C, F, K>(
		&mut self,
		cmd: C,
		predicate: F,
		checker: K,
	) -> Result<Reply, AsciiError>
	where
		C: Command,
		F: FnMut(&Reply) -> bool,
		K: check::Check<Reply>,
	{
		self.internal_poll_until_with_check(&cmd, predicate, &checker)
	}

	/// Send the specified command repeatedly until the predicate returns true
	/// for a reply.
	///
	/// If necessary, the command will be split into multiple packets so that no
	/// packet is longer than [`max_packet_size`](Self::max_packet_size).
	///
	/// If any response is spread across multiple packets, continuation packets will be read.
	fn internal_poll_until_with_check<F>(
		&mut self,
		cmd: &dyn Command,
		mut predicate: F,
		checker: &dyn check::Check<Reply>,
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
	/// the replies are checked with the [`strict`](check::strict)
	/// check.
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::Port, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// port.poll_until_idle((1,1))?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn poll_until_idle<T>(&mut self, target: T) -> Result<Reply, AsciiError>
	where
		T: Into<Target>,
	{
		self.poll_until((target.into(), ""), |reply| reply.status() == Status::Idle)
	}

	/// The same as [`Port::poll_until_idle`] except that the replies are
	/// validated with the custom [`Check`](check::Check).
	///
	/// ## Example
	///
	/// ```rust
	/// # use zproto::{ascii::{check, Port}, backend::Backend};
	/// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// port.poll_until_idle_with_check((1,1), check::flag_ok())?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn poll_until_idle_with_check<T, K>(
		&mut self,
		target: T,
		checker: K,
	) -> Result<Reply, AsciiError>
	where
		T: Into<Target>,
		K: check::Check<Reply>,
	{
		self.poll_until_with_check(
			(target.into(), ""),
			|reply| reply.status() == Status::Idle,
			checker,
		)
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
	/// # use zproto::{error::AsciiError, ascii::{Port, Reply}, backend::Backend};
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
	) -> Result<TimeoutGuard<B, Self>, io::Error> {
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

	/// Consume the port, converting it into a [`Chain`].
	pub fn into_chain(self) -> Result<Chain<'a, B>, AsciiError> {
		Chain::new(self)
	}

	/// Consume the port, converting it into a [`SyncChain`].
	pub fn into_sync_chain(self) -> Result<SyncChain<'a, B>, AsciiError> {
		Chain::new_sync(self)
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
	/// ASCII packet. Parse the bytes with [`Packet`] or [`Tokens`](crate::ascii::parse::Tokens)
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
	///     println!("{:?}: {:?}", dir, packet);
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
	///     println!("{}", packet_str);
	/// }
	/// # Ok(())
	/// # }
	/// ```
	pub fn set_packet_handler<F>(&mut self, callback: F) -> Option<PacketCallback<'a>>
	where
		F: FnMut(&[u8], Direction) + 'a,
	{
		std::mem::replace(
			&mut self.packet_hook,
			Some(PacketCallbackDebugWrapper(Box::new(callback))),
		)
		.map(|wrapper| wrapper.0)
	}

	/// Clear any callback registered via [`set_packet_handler`](Port::set_packet_handler) and return it.
	pub fn clear_packet_handler(&mut self) -> Option<PacketCallback<'a>> {
		self.packet_hook.take().map(|wrapper| wrapper.0)
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
	/// # use zproto::ascii::Port;
	/// # use std::cell::Cell;
	/// #
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// let mut port = Port::open_serial("...")?;
	///
	/// // Read a potentially large number of info messages. However, to ensure
	/// // that the read isn't interrupted by any unexpected alerts, first
	/// // configure the port to effectively ignore the alerts it may receive by
	/// // dropping them.
	/// port.set_unexpected_alert_handler(|_alert| Ok(()));
	/// let (_reply, _infos) = port.command_reply_infos((1, "storage all print"))?;
	/// // ...
	/// # Ok(())
	/// # }
	/// ```
	pub fn set_unexpected_alert_handler<F>(
		&mut self,
		callback: F,
	) -> Option<UnexpectedAlertCallback<'a>>
	where
		F: FnMut(Alert) -> Result<(), Alert> + 'a,
	{
		std::mem::replace(
			&mut self.unexpected_alert_hook,
			Some(UnexpectedAlertDebugWrapper(Box::new(callback))),
		)
		.map(|wrapper| wrapper.0)
	}

	/// Clear any callback registered via [`set_unexpected_alert_handler`](Port::set_unexpected_alert_handler) and return it.
	pub fn clear_unexpected_alert_handler(&mut self) -> Option<UnexpectedAlertCallback<'a>> {
		self.unexpected_alert_hook.take().map(|wrapper| wrapper.0)
	}
}

impl<'a, B: Backend> io::Write for Port<'a, B> {
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		self.check_poisoned()?;
		self.backend.write(buf)
	}

	fn flush(&mut self) -> io::Result<()> {
		self.check_poisoned()?;
		self.backend.flush()
	}
}

impl<'a, B: Backend> io::Read for Port<'a, B> {
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		self.check_poisoned()?;
		self.backend.read(buf)
	}
}

impl<'a, B: Backend> crate::timeout_guard::Port<B> for Port<'a, B> {
	fn backend_mut(&mut self) -> &mut B {
		&mut self.backend
	}
	fn poison(&mut self, e: io::Error) {
		self.poison = Some(e)
	}
}

#[derive(Debug, Clone)]
enum HeaderCheckAction {
	/// Check the response against the specified target and optional message ID.
	Check { target: Target, id: Option<u8> },
	/// Do not check the response's header
	DoNotCheck,
	/// The response is unexpected, return an error.
	Unexpected,
}

impl HeaderCheckAction {
	fn check(&self, response: AnyResponse) -> Result<AnyResponse, AsciiError> {
		use HeaderCheckAction::*;
		match self {
			Check { target, id } => {
				if !response.target().elicited_by_command_to(*target) || response.id() != *id {
					Err(AsciiUnexpectedResponseError::new(response).into())
				} else {
					Ok(response)
				}
			}
			Unexpected => Err(AsciiUnexpectedResponseError::new(response).into()),
			DoNotCheck => Ok(response),
		}
	}
}
