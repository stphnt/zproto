//! A port for sending and receiving Zaber Binary protocol messages.

#[cfg(any(test, feature = "mock"))]
use crate::backend::Mock;
use crate::{
	backend::{Backend, Serial, UNKNOWN_BACKEND_NAME},
	binary::{
		command,
		handlers::{Handlers, LocalHandlers, SendHandlers},
		traits, Message,
	},
	error::{
		BinaryCommandFailureError, BinaryError, BinaryUnexpectedCommandError,
		BinaryUnexpectedIdError, BinaryUnexpectedTargetError, TryIntoSendError,
	},
	timeout_guard::TimeoutGuard,
};
use serialport as sp;
use std::{
	io,
	net::{TcpStream, ToSocketAddrs},
	time::Duration,
};

/// Options for configuring and opening a serial port.
///
/// ## Example
///
/// ```rust
/// # use zproto::binary::OpenSerialOptions;
/// # use std::time::Duration;
/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
/// let mut port = OpenSerialOptions::new()
///     .timeout(Some(Duration::from_millis(50)))
///     .open("/dev/ttyUSB0")?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug)]
pub struct OpenSerialOptions {
	/// The custom baud rate
	baud_rate: u32,
	/// The custom timeout
	timeout: Option<Duration>,
}

impl OpenSerialOptions {
	/// The default baud rate for the Binary protocol: 9,600.
	pub const DEFAULT_BAUD_RATE: u32 = 9_600;

	/// Create a blank set of options ready for configuration.
	///
	/// The default baud rate and read timeout are 9,600 and 5 seconds, respectively.
	///
	/// Equivalent to [`default`](OpenSerialOptions::default).
	pub fn new() -> Self {
		OpenSerialOptions {
			baud_rate: OpenSerialOptions::DEFAULT_BAUD_RATE,
			timeout: Some(Duration::from_secs(5)),
		}
	}

	/// Set a custom baud rate.
	pub fn baud_rate(&mut self, baud_rate: u32) -> &mut Self {
		self.baud_rate = baud_rate;
		self
	}

	/// Set a custom read timeout.
	///
	/// If duration is `None`, reads will block indefinitely.
	pub fn timeout(&mut self, duration: Option<Duration>) -> &mut Self {
		self.timeout = duration;
		self
	}

	/// Open a [`Serial`] port configured for the Binary protocol at the specified path.
	fn open_serial_port(&self, path: &str) -> Result<Serial, BinaryError> {
		// Due to https://gitlab.com/susurrus/serialport-rs/-/issues/102, the
		// baud rate passed to new is ignored. It must be defined using the
		// baud_rate method below. Use the default baud_rate as it should be a
		// valid baud rate.
		sp::new(path, OpenSerialOptions::DEFAULT_BAUD_RATE)
			.data_bits(sp::DataBits::Eight)
			.parity(sp::Parity::None)
			.flow_control(sp::FlowControl::None)
			.stop_bits(sp::StopBits::One)
			// The serialport API does not support infinite timeouts, so simply
			// set the timeout to the largest possible duration if `timeout` is
			// `None`, which is practically infinite.
			.timeout(self.timeout.unwrap_or(Duration::MAX))
			.baud_rate(self.baud_rate)
			.open_native()
			.map(Serial)
			.map_err(Into::into)
	}

	/// Open the port at the specified path with the custom options.
	pub fn open<'a>(&self, path: &str) -> Result<Port<'a, Serial>, BinaryError> {
		Ok(Port::from_backend(self.open_serial_port(path)?))
	}

	/// Open the port at the specified path with the custom options.
	///
	/// The type of the underlying backend is erased via dynamic dispatch,
	/// which does have runtime overhead. [`open`](OpenSerialOptions::open)
	/// should generally be used instead, except when the type of the underlying
	/// backend may not be known at compile time.
	pub fn open_dyn<'a>(&self, path: &str) -> Result<Port<'a, Box<dyn Backend>>, BinaryError> {
		Ok(Port::from_backend(Box::new(self.open_serial_port(path)?)))
	}
}

impl Default for OpenSerialOptions {
	fn default() -> Self {
		OpenSerialOptions::new()
	}
}

/// Options for configuring and opening a TCP port.
///
/// ## Example
///
/// ```rust
/// # use zproto::binary::OpenTcpOptions;
/// # use std::time::Duration;
/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
/// let mut port = OpenTcpOptions::new()
///     .timeout(Some(Duration::from_millis(50)))
///     .open("192.168.0.1:55550")?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug)]
pub struct OpenTcpOptions {
	/// The custom timeout
	timeout: Option<Duration>,
}

impl OpenTcpOptions {
	/// Create a blank set of options ready for configuration.
	///
	/// The default read timeout is 5 seconds. Message IDs and checksums are also enabled by default.
	///
	/// Equivalent to [`default`](OpenSerialOptions::default).
	pub fn new() -> Self {
		OpenTcpOptions {
			timeout: Some(Duration::from_secs(5)),
		}
	}

	/// Set a custom read timeout.
	///
	/// If duration is `None`, reads will block indefinitely.
	pub fn timeout(&mut self, duration: Option<Duration>) -> &mut Self {
		self.timeout = duration;
		self
	}

	/// Open a [`TcpStream`] configured for the Binary protocol at the specified address.
	fn open_tcp_stream<A: ToSocketAddrs>(&self, address: A) -> io::Result<TcpStream> {
		let stream = TcpStream::connect(address)?;
		stream.set_read_timeout(self.timeout)?;
		Ok(stream)
	}

	/// Open the port at the specified path with the custom options.
	pub fn open<'a, A: ToSocketAddrs>(&self, address: A) -> io::Result<Port<'a, TcpStream>> {
		Ok(Port::from_backend(self.open_tcp_stream(address)?))
	}

	/// Open the port at the specified path with the custom options.
	///
	/// The type of the underlying backend is erased via dynamic dispatch,
	/// which does have runtime overhead. [`open`](OpenTcpOptions::open) should
	/// generally be used instead, except when the type of the underlying
	/// backend may not be known at compile time.
	pub fn open_dyn<'a, A: ToSocketAddrs>(
		&self,
		address: A,
	) -> io::Result<Port<'a, Box<dyn Backend>>> {
		Ok(Port::from_backend(Box::new(self.open_tcp_stream(address)?)))
	}
}

impl Default for OpenTcpOptions {
	fn default() -> Self {
		OpenTcpOptions::new()
	}
}

/// The state of message ids on a port.
#[derive(Debug)]
enum MessageId {
	/// Message Ids are enabled. The value is the last message id used.
	Enabled(u8),
	/// Message Ids are disabled. The value is the last message id used.
	Disabled(u8),
}

impl MessageId {
	/// Get the next message ID, if any.
	fn next(&mut self) -> Option<u8> {
		if let MessageId::Enabled(ref mut id) = self {
			*id = id.wrapping_add(1);
			Some(*id)
		} else {
			None
		}
	}
	/// Enable message Ids.
	///
	/// Future calls to [`next`] will return a value.
	fn enable(&mut self) {
		if let MessageId::Disabled(value) = *self {
			*self = MessageId::Enabled(value);
		}
	}
	/// Disable message Ids.
	///
	/// Future calls to [`next`] will return a `None`.
	fn disable(&mut self) {
		if let MessageId::Enabled(value) = *self {
			*self = MessageId::Disabled(value);
		}
	}
	fn is_enabled(&self) -> bool {
		matches!(self, MessageId::Enabled(_))
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
/// The type of [`Port`] that implements `Send`.
pub type SendPort<'a, B> = Port<'a, B, SendHandlers<'a>>;

/// A Port for transmitting and receiving Zaber Binary protocol messages.
///
/// See the [`binary`] module documentation for details on how to use a `Port`.
///
/// A port is parameterized by two types:
/// 1. `B`: the type of [`Backend`] used to send/receive data.
///    * Use the convenience methods [`open_serial`] and [`open_tcp`] to construct
///      a serial port (`Port<Serial>`) and a TCP port (`Port<TcpStream>`). To
///      customize the construction of these types, or to construct a port with a
///      dynamic backend, use the [`OpenSerialOptions`] and [`OpenTcpOptions`]
///      builder types.
/// 2. `H`: the type of event [handlers].
///    * This has a default and can be ignored in single-threaded contexts. There
///      are two types for event handlers: on that implements `Send` and one that
///      does not (the default). To convert a port into a type that implements
///      `Send`, use the [`try_into_send`] method.
///
/// [`binary`]: crate::binary
/// [`open_serial`]: Port::open_serial
/// [`open_tcp`]: Port::open_tcp
/// [handlers]: Handlers
/// [`try_into_send`]: Port::try_into_send
pub struct Port<'a, B, H = LocalHandlers<'a>> {
	/// The backend to transmit/receive commands with
	backend: B,
	/// The message ID state
	id: MessageId,
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
	/// User specified event handlers.
	handlers: H,
	/// Lifetime marker
	lifetime: std::marker::PhantomData<&'a ()>,
}

impl<'a, B: Backend, H> std::fmt::Debug for Port<'a, B, H> {
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
	/// # use zproto::binary::Port;
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// let mut port = Port::open_serial("/dev/ttyUSB0")?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn open_serial(path: &str) -> Result<Port<'a, Serial>, BinaryError> {
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
	/// # use zproto::binary::Port;
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// let mut port = Port::open_tcp("/dev/ttyUSB0")?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn open_tcp<A: ToSocketAddrs>(address: A) -> Result<Port<'a, TcpStream>, io::Error> {
		OpenTcpOptions::new().open(address)
	}

	/// Get an [`OpenTcpOptions`] to customize how a TCP port is opened.
	pub fn open_tcp_options() -> OpenTcpOptions {
		OpenTcpOptions::default()
	}
}

#[cfg(any(test, feature = "mock"))]
impl<'a> Port<'a, Mock> {
	/// Open a Port with a [`Mock`] [`Backend`].
	///
	/// This is useful for writing unit/integration tests when an actual device is not available.
	///
	/// See the [`Mock`]'s documentation for more details on its behaviour.
	///
	/// # Example
	///
	/// ```
	/// # use zproto::binary::Port;
	/// use zproto::binary::command::HOME;
	/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
	/// let mut port = Port::open_mock();
	/// port.backend_mut().push([1, 1, 0, 0, 0, 0]);
	/// let reply = port.tx_recv((0, HOME)).unwrap();
	/// assert_eq!(reply.command(), HOME);
	/// # Ok(())
	/// # }
	/// ```
	#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "mock")))]
	pub fn open_mock() -> Port<'a, Mock> {
		Port::from_backend(Mock::new())
	}
}

impl<'a, B, H> Port<'a, B, H>
where
	B: Backend,
	H: Handlers,
	H::PacketHandler: FnMut(&[u8], Message, Direction) + 'a,
{
	/// Get a `Port` from the given backend.
	fn from_backend(backend: B) -> Port<'a, B, H> {
		Port {
			backend,
			id: MessageId::Disabled(0),
			poison: None,
			handlers: H::default(),
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

	/// Convert this port into one that implements `Send` and can therefore be sent
	/// to another thread.
	///
	/// Returns an error if `Send` bounds could not be added. In particular if
	/// an event handler is currently set. As such, it is recommended to call
	/// [`Port::try_into_send`] before setting a callback if you need to do both.
	///
	/// The [`Port::set_packet_handler`] on the new port will have an additional
	/// `Send` bound on any function used as a callback.
	///
	/// # Example
	///
	/// ```
	/// # use zproto::{binary::Port, backend::Backend};
	/// # use std::{error::Error, fmt::Debug};
	/// # fn wrapper<B: Backend + Debug + Send + 'static>(
	/// #     port: Port<'static, B>
	/// # ) -> Result<(), Box<dyn Error>> {
	/// use std::sync::{Arc, Mutex};
	///
	/// let port = Port::open_serial("...")?
	///     .try_into_send()  // Required in order to send the port to another thread.
	///     .unwrap();
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
	pub fn try_into_send(mut self) -> Result<SendPort<'a, B>, TryIntoSendError> {
		if self.handlers.packet().is_some() {
			return Err(TryIntoSendError::new());
		}
		Ok(Port {
			backend: self.backend,
			id: self.id,
			poison: self.poison,
			handlers: SendHandlers::default(),
			lifetime: std::marker::PhantomData,
		})
	}

	/// Transmit a message and then receive a response.
	///
	/// ## Errors
	///
	/// An error is returned if the [`ERROR`](command::ERROR) is received, the
	/// command code is unexpected, the device address is unexpected, or, if
	/// message IDs are enabled, the message ID doesn't match the command.
	///
	/// Note: sending messages to a device alias other than `0` (all devices)
	/// will result in an [`BinaryUnexpected*`](crate::error::BinaryUnexpectedError).
	///
	/// ## Example
	/// ```
	/// # use zproto:: {
	/// #   backend::Backend,
	/// #   binary::{Message, Port},
	/// # };
	/// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::binary::command::*;
	/// let reply = port.tx_recv((0, RETURN_SETTING, SET_TARGET_SPEED))?;
	/// let value = reply.data()?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn tx_recv<M>(&mut self, message: M) -> Result<Message<M::Response>, BinaryError>
	where
		M: traits::TxMessage + traits::ElicitsResponse,
	{
		let id = self.tx(&message)?;
		let response = self.recv_internal(|response| {
			check_unexpected_id(response, id)?;
			check_unexpected_target(response, message.command(), message.target())?;
			check_command_failure(response)?;
			check_unexpected_elicited_command(response, &message)?;
			Ok(())
		})?;
		Message::try_from_untyped(response).map_err(Into::into)
	}

	/// Transmit a message and then receive `n` responses.
	///
	/// See [`tx_recv`](Port::tx_recv) for more details.
	pub fn tx_recv_n<M>(
		&mut self,
		message: M,
		n: usize,
	) -> Result<Vec<Message<M::Response>>, BinaryError>
	where
		M: traits::TxMessage + traits::ElicitsResponse,
	{
		let id = self.tx(&message)?;
		let responses = self.recv_n_internal(
			|response| {
				check_unexpected_id(response, id)?;
				check_unexpected_target(response, message.command(), message.target())?;
				check_command_failure(response)?;
				check_unexpected_elicited_command(response, &message)?;
				Ok(())
			},
			n,
		)?;
		Ok(responses)
	}

	/// Transmit a message and then receive messages until the port times out.
	///
	/// This useful for sending messages to all devices on the chain and collecting
	/// all of their responses. See [`tx_recv`](Port::tx_recv) for more details.
	pub fn tx_recv_until_timeout<M>(
		&mut self,
		message: M,
	) -> Result<Vec<Message<M::Response>>, BinaryError>
	where
		M: traits::TxMessage + traits::ElicitsResponse,
	{
		let id = self.tx(&message)?;
		let responses = self.recv_until_timeout_internal(|response| {
			check_unexpected_id(response, id)?;
			check_unexpected_target(response, message.command(), message.target())?;
			check_command_failure(response)?;
			check_unexpected_elicited_command(response, &message)?;
			Ok(())
		})?;
		Ok(responses)
	}

	/// Transmit a message.
	///
	/// On success the generated message ID (if any) is returned.
	pub fn tx<M: traits::TxMessage>(&mut self, message: M) -> Result<Option<u8>, BinaryError> {
		self.check_poisoned()?;

		// Fill the buffer with the message and add a message Id, if necessary.
		let mut buffer = [0u8; 6];
		message.populate_message(&mut buffer[..]);
		let id = self.id.next();
		if let Some(id) = id {
			buffer[5] = id;
		}

		log::debug!(
			"{} TX: {:?}",
			self.backend
				.name()
				.unwrap_or_else(|| UNKNOWN_BACKEND_NAME.to_string()),
			buffer
		);

		if let Some(callback) = self.handlers.packet() {
			(callback)(
				&buffer,
				Message::from_bytes(buffer, id.is_some()),
				Direction::Tx,
			);
		}

		self.backend.write_all(&buffer[..])?;
		Ok(id)
	}

	/// Receive a single message.
	///
	/// `checks` will be called after a response has been received and parsed,
	/// but before returning the value. Any error it produces is forwarded to
	/// to the caller.
	fn recv_internal(
		&mut self,
		checks: impl Fn(Message) -> Result<(), BinaryError>,
	) -> Result<Message, BinaryError> {
		self.check_poisoned()?;

		let mut buf = [0; 6];
		self.backend.read_exact(&mut buf)?;
		log::debug!(
			"{} RX: {:?}",
			self.backend
				.name()
				.unwrap_or_else(|| UNKNOWN_BACKEND_NAME.to_string()),
			&buf
		);
		let response = Message::from_bytes(buf, self.id.is_enabled());

		if let Some(callback) = self.handlers.packet() {
			(callback)(&buf, response, Direction::Recv);
		}
		checks(response)?;
		Ok(response)
	}

	/// Receive a message.
	///
	/// If the received command does not match `expected` an error is returned.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto:: {
	/// #   backend::Backend,
	/// #   binary::{Message, Port},
	/// # };
	/// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::binary::command::*;
	/// let reply = port.recv(MANUAL_MOVE_TRACKING)?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn recv<C: traits::Command>(&mut self, expected: C) -> Result<Message<C>, BinaryError> {
		let response = self.recv_internal(|response| {
			check_command_failure(response)?;
			check_unexpected_command(response, expected.command())?;
			Ok(())
		})?;
		Message::try_from_untyped(response).map_err(Into::into)
	}

	/// Receive a message containing any command.
	pub fn recv_any(&mut self) -> Result<Message, BinaryError> {
		self.recv_internal(|response| {
			check_command_failure(response)?;
			Ok(())
		})
	}

	/// Receive `n` messages.
	///
	/// `checks` will be called immediately after each response has been
	/// received and parsed. If it produces an error, the error is immediately
	/// forwarded to the user and no further messages are received.
	fn recv_n_internal<C: traits::Command>(
		&mut self,
		checks: impl Fn(Message) -> Result<(), BinaryError>,
		n: usize,
	) -> Result<Vec<Message<C>>, BinaryError> {
		let mut responses = Vec::with_capacity(n);
		for _ in 0..n {
			responses.push(Message::try_from_untyped(self.recv_internal(&checks)?)?);
		}
		Ok(responses)
	}

	/// Receive `n` messages.
	///
	/// If any of the received messages have a command that does not match
	/// `expected` an error is returned and no further messages are received.
	pub fn recv_n<C: traits::Command>(
		&mut self,
		expected: C,
		n: usize,
	) -> Result<Vec<Message<C>>, BinaryError> {
		self.recv_n_internal(
			|response| {
				check_command_failure(response)?;
				check_unexpected_command(response, expected.command())?;
				Ok(())
			},
			n,
		)
	}

	/// Receive `n` messages containing any command.
	///
	/// Each message may contain a different command.
	pub fn recv_any_n(&mut self, n: usize) -> Result<Vec<Message>, BinaryError> {
		self.recv_n_internal(
			|response| {
				check_command_failure(response)?;
				Ok(())
			},
			n,
		)
	}

	/// Receive messages until the port times out.
	///
	/// `checks` will be called immediately after each response has been
	/// received and parsed. If it produces an error, the error is immediately
	/// forwarded to the user and no further messages are received.
	fn recv_until_timeout_internal<C: traits::Command>(
		&mut self,
		checks: impl Fn(Message) -> Result<(), BinaryError>,
	) -> Result<Vec<Message<C>>, BinaryError> {
		let mut responses = Vec::new();
		loop {
			match self.recv_internal(&checks) {
				Ok(r) => responses.push(Message::try_from_untyped(r)?),
				Err(e) if e.is_timeout() => break,
				Err(e) => return Err(e),
			}
		}
		Ok(responses)
	}

	/// Receive messages until the port times out.
	///
	/// If any of the received messages have a command that does not match
	/// `expected` an error is returned and no further messages are received.
	pub fn recv_until_timeout<C>(&mut self, expected: C) -> Result<Vec<Message<C>>, BinaryError>
	where
		C: traits::Command,
	{
		self.recv_until_timeout_internal(|response| {
			check_command_failure(response)?;
			check_unexpected_command(response, expected.command())?;
			Ok(())
		})
	}

	/// Receive messages containing any command until the port times out.
	///
	/// Each message may contain a different command.
	pub fn recv_any_until_timeout(&mut self) -> Result<Vec<Message>, BinaryError> {
		self.recv_until_timeout_internal(|response| {
			check_command_failure(response)?;
			Ok(())
		})
	}

	/// Transmit the specified message repeatedly until the predicate returns true for a response.
	///
	/// The reply that first passes the predicate is returned.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto:: {
	/// #   backend::Backend,
	/// #   binary::{Message, Port},
	/// # };
	/// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// use zproto::binary::command::*;
	/// // Wait until device 1 has passed position 10000.
	/// let last_reply = port.poll_until(
	///     (1, RETURN_CURRENT_POSITION),
	///     |response| response.data().unwrap() > 10000
	/// )?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn poll_until<M, F>(
		&mut self,
		message: M,
		mut predicate: F,
	) -> Result<Message<M::Response>, BinaryError>
	where
		M: traits::TxMessage + traits::ElicitsResponse,
		F: FnMut(&Message<M::Response>) -> bool,
	{
		let mut response;
		loop {
			response = self.tx_recv(&message)?;
			if predicate(&response) {
				break;
			}
		}
		Ok(response)
	}

	/// Transmit the [`RETURN_STATUS`](command::RETURN_STATUS) command until the
	/// specified target reports that it is idle.
	pub fn poll_until_idle(
		&mut self,
		target: u8,
	) -> Result<Message<command::types::ReturnStatus>, BinaryError> {
		self.poll_until((target, command::RETURN_STATUS), |reply| {
			reply.data().unwrap() == super::Status::Idle
		})
	}

	/// Enable/disable transmission and parsing of message IDs.
	///
	/// Returns the previous value.
	///
	/// This will configure all devices on the port with the
	/// [`SET_MESSAGE_ID_MODE`](command::SET_MESSAGE_ID_MODE) command and
	/// configure the port to generate, transmit and parse (or not) message IDs
	/// for all future messages.
	///
	/// By default message ids are not enabled as it consumes one of the 4 data
	/// bytes in a message, potentially truncating larger data values.
	///
	/// On success, whether message IDs were previously enabled is returned.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto:: {
	/// #   backend::Backend,
	/// #   binary::{Message, Port},
	/// # };
	/// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), Box<dyn std::error::Error>> {
	/// port.set_message_ids(true)?;
	/// // Message Ids will be used in all future messages.
	/// # Ok(())
	/// # }
	/// ```
	pub fn set_message_ids(&mut self, enable: bool) -> Result<bool, BinaryError> {
		let prev = self.id.is_enabled();
		self.tx_recv_until_timeout((0, command::SET_MESSAGE_ID_MODE, enable))?;
		if enable {
			self.id.enable();
		} else {
			self.id.disable();
		}
		Ok(prev)
	}

	/// Return whether message IDs will be transmitted and parsed.
	pub fn message_ids(&self) -> bool {
		self.id.is_enabled()
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

	/// Set a callback that will be called immediately after any Binary packet
	/// is sent or received.
	///
	/// If a previous callback was set, it is returned.
	///
	/// To clear a previously registered callback use [`clear_packet_handler`](Port::clear_packet_handler).
	///
	/// The callback will be passed the packet bytes, the parsed version of the
	/// bytes, and the direction of the packet.
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
	/// # use zproto::binary::Port;
	/// #
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// # let mut port = Port::open_serial("...")?;
	/// port.set_packet_handler(|bytes, message, dir| {
	///     println!("{bytes:?} {message:?} {dir:?}");
	/// });
	/// # Ok(())
	/// # }
	/// ```
	///
	/// However, if the closure captures any variables those variables must
	/// either be moved into the closure with the `move` keyword (e.g., `move |bytes, message, dir| {...}`)
	/// or live at least as long as the `Port` instance. Additionally, if those
	/// variables are mutated but also accessed outside of the closure, then a
	/// [`RefCell`](std::cell::RefCell)/[`Mutex`](std::sync::Mutex) should be
	/// used to facilitate the sharing.
	///
	/// ```
	/// # use zproto::binary::{command, Port};
	/// # use std::cell::RefCell;
	/// #
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// let packet_list = RefCell::new(Vec::new());
	/// let mut port = Port::open_serial("...")?;
	///
	/// port.set_packet_handler(|_bytes, message, _dir| {
	///     if let Ok(mut packets) = packet_list.try_borrow_mut() {
	///         packets.push(message);
	///     }
	/// });
	///
	/// port.tx_recv((1, command::HOME));
	///
	/// for packet in packet_list.borrow().iter() {
	///     println!("{packet:?}");
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

	/// Set the port timeout and return a "scope guard" that will reset the
	/// timeout when it goes out of scope.
	///
	/// While the guard is in scope, the port can only be accessed through the
	/// guard. However, because the guard implements [`Deref`](std::ops::Deref)
	/// and [`DerefMut`](std::ops::DerefMut) callers can treat the guard as the
	/// port.
	///
	/// ## Example
	/// ```rust
	/// # use zproto::{error::BinaryError, binary::Port, backend::Backend};
	/// # use std::time::Duration;
	/// # fn helper<B: Backend>(mut port: Port<B>) -> Result<(), BinaryError> {
	/// use zproto::binary::command::*;
	/// {
	///     let mut guard = port.timeout_guard(Some(Duration::from_secs(25)))?;
	///     // All commands within this scope will use a 25 second timeout
	///     guard.tx_recv((1, MOVE_ABSOLUTE, 10000))?;
	///
	/// }  // The guard is dropped and the timeout is reset.
	///
	/// // This transaction uses the original timeout
	/// let speed = port.tx_recv((1, RETURN_SETTING, SET_TARGET_SPEED))?.data()?;
	/// # Ok(())
	/// # }
	/// ```
	pub fn timeout_guard(
		&mut self,
		timeout: Option<Duration>,
	) -> Result<TimeoutGuard<'_, B, Self>, io::Error> {
		self.check_poisoned()?;
		TimeoutGuard::new(self, timeout)
	}
}

impl<'a, B: Backend, H> crate::timeout_guard::Port<B> for Port<'a, B, H> {
	fn backend_mut(&mut self) -> &mut B {
		&mut self.backend
	}

	fn poison(&mut self, error: io::Error) {
		self.poison = Some(error);
	}
}

/// Check for an unexpected command in the response.
fn check_unexpected_command(
	response: Message,
	expected: u8,
) -> Result<(), BinaryUnexpectedCommandError> {
	// If the response type, C, only encodes one command then because it
	// already exists we know it must be valid (assuming the constructor
	// validates the data, which all of our commands do). However, if the
	// type can encode more than one command, like a `u8` can, then we must
	// check the specific value in this instance of the response to make
	// sure it is the expected value.
	if response.command() != expected {
		return Err(BinaryUnexpectedCommandError::new(response));
	}
	Ok(())
}

/// Check for an unexpected command in the response.
///
/// The expected command is calculated based on the message that was `sent`.
fn check_unexpected_elicited_command<M>(
	response: Message,
	sent: &M,
) -> Result<(), BinaryUnexpectedCommandError>
where
	M: traits::TxMessage + traits::ElicitsResponse,
{
	use traits::ExpectedCommandResult as ECR;
	match sent.expected_command() {
		ECR::AnyAcceptable => {}
		ECR::AnyUnexpected => return Err(BinaryUnexpectedCommandError::new(response)),
		ECR::Exactly(value) => {
			if traits::Command::command(&value) != response.command() {
				return Err(BinaryUnexpectedCommandError::new(response));
			}
		}
	}
	Ok(())
}

/// Check for an unexpected message ID in the response.
fn check_unexpected_id(response: Message, id: Option<u8>) -> Result<(), BinaryUnexpectedIdError> {
	if response.id() == id {
		Ok(())
	} else {
		Err(BinaryUnexpectedIdError::new(response))
	}
}

/// Check for an unexpected target in the response.
fn check_unexpected_target(
	response: Message,
	sent_command: u8,
	sent_target: u8,
) -> Result<(), BinaryUnexpectedTargetError> {
	// The target should always match the command's target except when:
	//  * the command is Renumber (which necessarily changes the device address)
	//  * the target is 0 (all devices)
	//  * the target is an alias (which we don't handle yet)
	if sent_command != command::RENUMBER && sent_target != 0 && sent_target != response.target() {
		Err(BinaryUnexpectedTargetError::new(response))
	} else {
		Ok(())
	}
}

/// Check if the response's command is ERROR (255).
fn check_command_failure(response: Message) -> Result<(), BinaryCommandFailureError> {
	if response.command() == command::ERROR {
		Err(BinaryCommandFailureError::new(response))
	} else {
		Ok(())
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use command::*;

	#[test]
	fn tx_recv_ok() {
		let mut port = Port::open_mock();
		port.backend.push([1, 1, 0, 0, 0, 0]);
		let reply = port.tx_recv((0, HOME)).unwrap();
		assert_eq!(reply.command(), HOME);
		assert_eq!(reply.target(), 1);
		assert_eq!(reply.data().unwrap(), 0);
	}

	#[test]
	fn tx_recv_fail() {
		use crate::error::{binary_code, BinaryError};

		let mut port = Port::open_mock();

		// Unexpected command in response
		port.backend
			.push([1, untyped::MANUAL_MOVE_TRACKING, 0, 0, 0, 0]);
		let err = port.tx_recv((0, HOME)).unwrap_err();
		if let BinaryError::UnexpectedCommand(err) = err {
			assert_eq!(err.as_ref().command(), untyped::MANUAL_MOVE_TRACKING);
		} else {
			panic!("unexpected error {err:?}")
		}

		// Unexpected target in reply
		port.backend.push([1, untyped::HOME, 0, 0, 0, 0]);
		let err = port.tx_recv((2, HOME)).unwrap_err();
		if let BinaryError::UnexpectedTarget(err) = err {
			assert_eq!(err.as_ref().command(), untyped::HOME);
		} else {
			panic!("unexpected error {err:?}")
		}

		// Command failure
		port.backend
			.push([1, untyped::ERROR, binary_code::CANNOT_HOME as u8, 0, 0, 0]);
		let err = port.tx_recv((1, HOME)).unwrap_err();
		if let BinaryError::CommandFailure(err) = err {
			assert_eq!(err.code(), binary_code::CANNOT_HOME);
			assert_eq!(err.as_ref().command(), untyped::ERROR);
		} else {
			panic!("unexpected error {err:?}")
		}

		// Unexpected message ID
		port.backend.push([1, untyped::MOVE_ABSOLUTE, 5, 0, 0, 2]);
		port.id = MessageId::Enabled(0);
		let err = port.tx_recv((1, MOVE_ABSOLUTE, 5)).unwrap_err();
		if let BinaryError::UnexpectedId(err) = err {
			assert_eq!(Message::from(err).id().unwrap(), 2);
		} else {
			panic!("unexpected error {err:?}")
		}
	}

	#[test]
	fn tx_recv_n_ok() {
		let mut port = Port::open_mock();

		port.backend.push([1, 1, 0, 0, 0, 0]);
		let replies = port.tx_recv_n((0, HOME), 1).unwrap();
		assert_eq!(replies.len(), 1);
		let reply = replies[0];
		assert_eq!(reply.command(), HOME);
		assert_eq!(reply.target(), 1);
		assert_eq!(reply.data().unwrap(), 0);

		port.backend.push([1, 1, 0, 0, 0, 0]);
		port.backend.push([2, 1, 0, 0, 0, 0]);
		let replies = port.tx_recv_n((0, HOME), 2).unwrap();
		assert_eq!(replies.len(), 2);
		for (i, reply) in replies.iter().enumerate() {
			eprintln!("{reply:?}");
			assert_eq!(reply.command(), HOME);
			assert_eq!(reply.target(), i as u8 + 1);
			assert_eq!(reply.data().unwrap(), 0);
		}
	}

	#[test]
	fn tx_recv_n_fail() {
		use crate::error::*;

		let mut port = Port::open_mock();

		// Timeout waiting for second message
		port.backend.push([1, 1, 0, 0, 0, 0]);
		assert!(port.tx_recv_n((0, HOME), 2).unwrap_err().is_timeout());

		// Unexpected command in second message
		port.backend.push([1, untyped::HOME, 0, 0, 0, 0]);
		port.backend.push([2, untyped::RESET, 0, 0, 0, 0]);
		let err = port.tx_recv_n((0, HOME), 2).unwrap_err();
		if let BinaryError::UnexpectedCommand(err) = err {
			assert_eq!(Message::from(err).command(), untyped::RESET);
		} else {
			panic!("unexpected error: {err:?}");
		}

		// Unexpected target in second message
		port.backend.push([1, untyped::HOME, 0, 0, 0, 0]);
		port.backend.push([2, untyped::HOME, 0, 0, 0, 0]);
		let err = port.tx_recv_n((1, HOME), 2).unwrap_err();
		if let BinaryError::UnexpectedTarget(err) = err {
			assert_eq!(Message::from(err).target(), 2);
		} else {
			panic!("unexpected error: {err:?}");
		}

		// Unexpected message ID in second message
		port.backend
			.push([2, untyped::SET_TARGET_SPEED, 0, 0, 0, 1]);
		port.backend
			.push([3, untyped::SET_TARGET_SPEED, 0, 0, 0, 55]);
		port.id = MessageId::Enabled(0);
		let err = port.tx_recv_n((0, SET_TARGET_SPEED, 1), 2).unwrap_err();
		if let BinaryError::UnexpectedId(err) = err {
			assert_eq!(err.as_ref().id().unwrap(), 55);
		} else {
			panic!("unexpected error: {err:?}");
		}

		// Command failure in second message.
		port.id.disable();
		port.backend.push([1, 1, 0, 0, 0, 0]);
		port.backend.push([2, 255, 5, 0, 0, 0]);
		let err = port.tx_recv_n((0, HOME), 2).unwrap_err();
		if let BinaryError::CommandFailure(err) = err {
			assert_eq!(err.code(), 5);
		} else {
			panic!("unexpected error: {err:?}");
		}
	}

	#[test]
	fn tx_recv_until_timeout_ok() {
		let mut port = Port::open_mock();

		port.backend.push([1, 1, 0, 0, 0, 0]);
		let replies = port.tx_recv_until_timeout((0, HOME)).unwrap();
		assert_eq!(replies.len(), 1);
		let reply = replies[0];
		assert_eq!(reply.command(), HOME);
		assert_eq!(reply.target(), 1);
		assert_eq!(reply.data().unwrap(), 0);

		port.backend.push([1, 1, 0, 0, 0, 0]);
		port.backend.push([2, 1, 0, 0, 0, 0]);
		let replies = port.tx_recv_until_timeout((0, HOME)).unwrap();
		assert_eq!(replies.len(), 2);
		for (i, reply) in replies.iter().enumerate() {
			eprintln!("{reply:?}");
			assert_eq!(reply.command(), HOME);
			assert_eq!(reply.target(), i as u8 + 1);
			assert_eq!(reply.data().unwrap(), 0);
		}
	}

	#[test]
	fn message_ids() {
		let mut port = Port::open_mock();
		assert!(!port.message_ids());
		// Enable message IDs
		port.backend
			.push([1, command::untyped::SET_MESSAGE_ID_MODE, 0, 0, 0, 0]);
		let last_state = port.set_message_ids(true).unwrap();
		assert!(!last_state);
		assert!(port.message_ids());
	}

	#[test]
	fn set_packet_handler() {
		use std::{cell::RefCell, rc::Rc};
		let packets = Rc::new(RefCell::new(Vec::new()));
		let packets_handle = Rc::clone(&packets);

		let mut port = Port::open_mock();
		port.set_message_ids(true).unwrap();
		port.set_packet_handler(move |raw, packet, dir| {
			if let Ok(mut packets) = packets_handle.try_borrow_mut() {
				packets.push((raw.to_vec(), packet, dir));
			}
		});

		port.backend.push([1, 1, 0, 0, 0, 1]);
		port.backend.push([2, 1, 0, 0, 0, 1]);
		let _ = port.tx_recv_until_timeout((0, HOME)).unwrap();

		assert_eq!(
			*packets.borrow(),
			vec![
				(
					[0, 1, 0, 0, 0, 1].to_vec(),
					Message::from_bytes([0, 1, 0, 0, 0, 1], true),
					Direction::Tx
				),
				(
					[1, 1, 0, 0, 0, 1].to_vec(),
					Message::from_bytes([1, 1, 0, 0, 0, 1], true),
					Direction::Recv
				),
				(
					[2, 1, 0, 0, 0, 1].to_vec(),
					Message::from_bytes([2, 1, 0, 0, 0, 1], true),
					Direction::Recv
				),
			]
		);
	}

	#[test]
	fn compile_time_errors() {
		let t = trybuild::TestCases::new();
		t.compile_fail("test/binary/*.rs");
	}
}
