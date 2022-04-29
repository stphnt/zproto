//! A port for sending and receiving Zaber Binary protocol messages.

#[cfg(test)]
use crate::backend::Mock;
use crate::{
    backend::{Backend, Serial, UNKNOWN_BACKEND_NAME},
    binary::{command, traits, DeviceMessage},
    error::{
        BinaryCommandFailureError, BinaryError, BinaryUnexpectedCommandError,
        BinaryUnexpectedIdError, BinaryUnexpectedTargetError,
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
    /// The default baud rate for the ASCII protocol.
    const DEFAULT_BAUD_RATE: u32 = 9_600;

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
    pub fn open(&self, path: &str) -> Result<Port<Serial>, BinaryError> {
        Ok(Port::from_backend(self.open_serial_port(path)?))
    }

    /// Open the port at the specified path with the custom options.
    ///
    /// The type of the underlying backend is erased via dynamic dispatch,
    /// which does have runtime overhead. [`open`](OpenSerialOptions::open)
    /// should generally be used instead, except when the type of the underlying
    /// backend may not be known at compile time.
    pub fn open_dyn(&self, path: &str) -> Result<Port<Box<dyn Backend>>, BinaryError> {
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
    pub fn open<A: ToSocketAddrs>(&self, address: A) -> io::Result<Port<TcpStream>> {
        Ok(Port::from_backend(self.open_tcp_stream(address)?))
    }

    /// Open the port at the specified path with the custom options.
    ///
    /// The type of the underlying backend is erased via dynamic dispatch,
    /// which does have runtime overhead. [`open`](OpenTcpOptions::open) should
    /// generally be used instead, except when the type of the underlying
    /// backend may not be known at compile time.
    pub fn open_dyn<A: ToSocketAddrs>(&self, address: A) -> io::Result<Port<Box<dyn Backend>>> {
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

/// A Port for transmitting and receiving Zaber Binary protocol messages.
///
/// See the [`binary`](crate::binary) module documentation details on how to use
/// a `Port`.
#[derive(Debug)]
pub struct Port<B> {
    /// The backend to transmit/receive commands with
    backend: B,
    /// The message ID state
    id: MessageId,
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
}

impl Port<Serial> {
    /// Open the serial port at the specified path using the default options.
    ///
    /// Alternatively, use [`OpenSerialOptions`] to customize how the port is opened.
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
    pub fn open_serial(path: &str) -> Result<Port<Serial>, BinaryError> {
        OpenSerialOptions::new().open(path)
    }
}

impl Port<TcpStream> {
    /// Open the TCP port at the specified address using the default options.
    ///
    /// Alternatively, use [`OpenTcpOptions`] to customize how the port is opened.
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
    pub fn open_tcp<A: ToSocketAddrs>(address: A) -> Result<Port<TcpStream>, io::Error> {
        OpenTcpOptions::new().open(address)
    }
}

#[cfg(test)]
impl Port<Mock> {
    /// Open a mock port.
    pub fn open_mock() -> Port<Mock> {
        Port::from_backend(Mock::new())
    }
}

impl<B: Backend> Port<B> {
    /// Get a `Port` from the given backend.
    fn from_backend(backend: B) -> Port<B> {
        Port {
            backend,
            id: MessageId::Disabled(0),
            poison: None,
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
    /// #   binary::{DeviceMessage, Port},
    /// # };
    /// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zproto::binary::command::*;
    /// let reply = port.tx_recv((0, RETURN_SETTING, SET_TARGET_SPEED))?;
    /// let value = reply.data()?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn tx_recv<M>(&mut self, message: M) -> Result<DeviceMessage<M::Response>, BinaryError>
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
        DeviceMessage::try_from_untyped(response).map_err(Into::into)
    }

    /// Transmit a message and then receive `n` responses.
    ///
    /// See [`tx_recv`](Port::tx_recv) for more details.
    pub fn tx_recv_n<M>(
        &mut self,
        message: M,
        n: usize,
    ) -> Result<Vec<DeviceMessage<M::Response>>, BinaryError>
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
    ) -> Result<Vec<DeviceMessage<M::Response>>, BinaryError>
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
        checks: impl Fn(DeviceMessage) -> Result<(), BinaryError>,
    ) -> Result<DeviceMessage, BinaryError> {
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
        let response = DeviceMessage::from_bytes(&buf, self.id.is_enabled());
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
    /// #   binary::{DeviceMessage, Port},
    /// # };
    /// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zproto::binary::command::*;
    /// let reply = port.recv(MANUAL_MOVE_TRACKING)?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn recv<C: traits::Command>(
        &mut self,
        expected: C,
    ) -> Result<DeviceMessage<C>, BinaryError> {
        let response = self.recv_internal(|response| {
            check_command_failure(response)?;
            check_unexpected_command(response, expected.command())?;
            Ok(())
        })?;
        DeviceMessage::try_from_untyped(response).map_err(Into::into)
    }

    /// Receive a message containing any command.
    pub fn recv_any(&mut self) -> Result<DeviceMessage, BinaryError> {
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
        checks: impl Fn(DeviceMessage) -> Result<(), BinaryError>,
        n: usize,
    ) -> Result<Vec<DeviceMessage<C>>, BinaryError> {
        let mut responses = Vec::with_capacity(n);
        for _ in 0..n {
            responses.push(DeviceMessage::try_from_untyped(
                self.recv_internal(&checks)?,
            )?);
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
    ) -> Result<Vec<DeviceMessage<C>>, BinaryError> {
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
    pub fn recv_any_n(&mut self, n: usize) -> Result<Vec<DeviceMessage>, BinaryError> {
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
        checks: impl Fn(DeviceMessage) -> Result<(), BinaryError>,
    ) -> Result<Vec<DeviceMessage<C>>, BinaryError> {
        let mut responses = Vec::new();
        loop {
            match self.recv_internal(&checks) {
                Ok(r) => responses.push(DeviceMessage::try_from_untyped(r)?),
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
    pub fn recv_until_timeout<C>(
        &mut self,
        expected: C,
    ) -> Result<Vec<DeviceMessage<C>>, BinaryError>
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
    pub fn recv_any_until_timeout(&mut self) -> Result<Vec<DeviceMessage>, BinaryError> {
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
    /// #   binary::{DeviceMessage, Port},
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
    ) -> Result<DeviceMessage<M::Response>, BinaryError>
    where
        M: traits::TxMessage + traits::ElicitsResponse,
        F: FnMut(&DeviceMessage<M::Response>) -> bool,
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
    ) -> Result<DeviceMessage<command::types::ReturnStatus>, BinaryError> {
        self.poll_until((target, command::RETURN_STATUS), |reply| {
            reply.data().unwrap() == super::Status::Idle
        })
    }

    /// Enable/disable transmission and parsing of message IDs.
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
    /// #   binary::{DeviceMessage, Port},
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
    ) -> Result<TimeoutGuard<B, Self>, io::Error> {
        self.check_poisoned()?;
        TimeoutGuard::new(self, timeout)
    }
}

impl<B: Backend> crate::timeout_guard::Port<B> for Port<B> {
    fn backend_mut(&mut self) -> &mut B {
        &mut self.backend
    }

    fn poison(&mut self, error: io::Error) {
        self.poison = Some(error)
    }
}

/// Check for an unexpected command in the response.
fn check_unexpected_command(
    response: DeviceMessage,
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
    response: DeviceMessage,
    sent: &M,
) -> Result<(), BinaryUnexpectedCommandError>
where
    M: traits::TxMessage + traits::ElicitsResponse,
{
    use traits::ExpectedCommandResult::*;
    match sent.expected_command() {
        AnyAcceptable => {}
        AnyUnexpected => return Err(BinaryUnexpectedCommandError::new(response)),
        Exactly(value) => {
            if traits::Command::command(&value) != response.command() {
                return Err(BinaryUnexpectedCommandError::new(response));
            }
        }
    }
    Ok(())
}

/// Check for an unexpected message ID in the response.
fn check_unexpected_id(
    response: DeviceMessage,
    id: Option<u8>,
) -> Result<(), BinaryUnexpectedIdError> {
    if response.id() != id {
        Err(BinaryUnexpectedIdError::new(response))
    } else {
        Ok(())
    }
}

/// Check for an unexpected target in the response.
fn check_unexpected_target(
    response: DeviceMessage,
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
fn check_command_failure(response: DeviceMessage) -> Result<(), BinaryCommandFailureError> {
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
        port.backend.append_data([1, 1, 0, 0, 0, 0]);
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
            .append_data([1, untyped::MANUAL_MOVE_TRACKING, 0, 0, 0, 0]);
        let err = port.tx_recv((0, HOME)).unwrap_err();
        if let BinaryError::UnexpectedCommand(err) = err {
            assert_eq!(err.as_ref().command(), untyped::MANUAL_MOVE_TRACKING);
        } else {
            panic!("unexpected error {:?}", err)
        }

        // Unexpected target in reply
        port.backend.append_data([1, untyped::HOME, 0, 0, 0, 0]);
        let err = port.tx_recv((2, HOME)).unwrap_err();
        if let BinaryError::UnexpectedTarget(err) = err {
            assert_eq!(err.as_ref().command(), untyped::HOME);
        } else {
            panic!("unexpected error {:?}", err)
        }

        // Command failure
        port.backend
            .append_data([1, untyped::ERROR, binary_code::CANNOT_HOME as u8, 0, 0, 0]);
        let err = port.tx_recv((1, HOME)).unwrap_err();
        if let BinaryError::CommandFailure(err) = err {
            assert_eq!(err.code(), binary_code::CANNOT_HOME);
            assert_eq!(err.as_ref().command(), untyped::ERROR);
        } else {
            panic!("unexpected error {:?}", err)
        }

        // Unexpected message ID
        port.backend
            .append_data([1, untyped::MOVE_ABSOLUTE, 5, 0, 0, 2]);
        port.id = MessageId::Enabled(0);
        let err = port.tx_recv((1, MOVE_ABSOLUTE, 5)).unwrap_err();
        if let BinaryError::UnexpectedId(err) = err {
            assert_eq!(DeviceMessage::from(err).id().unwrap(), 2);
        } else {
            panic!("unexpected error {:?}", err)
        }
    }

    #[test]
    fn tx_recv_n_ok() {
        let mut port = Port::open_mock();

        port.backend.append_data([1, 1, 0, 0, 0, 0]);
        let replies = port.tx_recv_n((0, HOME), 1).unwrap();
        assert_eq!(replies.len(), 1);
        let reply = replies[0];
        assert_eq!(reply.command(), HOME);
        assert_eq!(reply.target(), 1);
        assert_eq!(reply.data().unwrap(), 0);

        port.backend.append_data([1, 1, 0, 0, 0, 0]);
        port.backend.append_data([2, 1, 0, 0, 0, 0]);
        let replies = port.tx_recv_n((0, HOME), 2).unwrap();
        assert_eq!(replies.len(), 2);
        for (i, reply) in replies.iter().enumerate() {
            eprintln!("{:?}", reply);
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
        port.backend.append_data([1, 1, 0, 0, 0, 0]);
        assert!(port.tx_recv_n((0, HOME), 2).unwrap_err().is_timeout());

        // Unexpected command in second message
        port.backend.append_data([1, untyped::HOME, 0, 0, 0, 0]);
        port.backend.append_data([2, untyped::RESET, 0, 0, 0, 0]);
        let err = port.tx_recv_n((0, HOME), 2).unwrap_err();
        if let BinaryError::UnexpectedCommand(err) = err {
            assert_eq!(DeviceMessage::from(err).command(), untyped::RESET);
        } else {
            panic!("unexpected error: {:?}", err);
        }

        // Unexpected target in second message
        port.backend.append_data([1, untyped::HOME, 0, 0, 0, 0]);
        port.backend.append_data([2, untyped::HOME, 0, 0, 0, 0]);
        let err = port.tx_recv_n((1, HOME), 2).unwrap_err();
        if let BinaryError::UnexpectedTarget(err) = err {
            assert_eq!(DeviceMessage::from(err).target(), 2);
        } else {
            panic!("unexpected error: {:?}", err);
        }

        // Unexpected message ID in second message
        port.backend
            .append_data([2, untyped::SET_TARGET_SPEED, 0, 0, 0, 1]);
        port.backend
            .append_data([3, untyped::SET_TARGET_SPEED, 0, 0, 0, 55]);
        port.id = MessageId::Enabled(0);
        let err = port.tx_recv_n((0, SET_TARGET_SPEED, 1), 2).unwrap_err();
        if let BinaryError::UnexpectedId(err) = err {
            assert_eq!(err.as_ref().id().unwrap(), 55);
        } else {
            panic!("unexpected error: {:?}", err);
        }

        // Command failure in second message.
        port.id.disable();
        port.backend.append_data([1, 1, 0, 0, 0, 0]);
        port.backend.append_data([2, 255, 5, 0, 0, 0]);
        let err = port.tx_recv_n((0, HOME), 2).unwrap_err();
        if let BinaryError::CommandFailure(err) = err {
            assert_eq!(err.code(), 5);
        } else {
            panic!("unexpected error: {:?}", err);
        }
    }

    #[test]
    fn tx_recv_until_timeout_ok() {
        let mut port = Port::open_mock();

        port.backend.append_data([1, 1, 0, 0, 0, 0]);
        let replies = port.tx_recv_until_timeout((0, HOME)).unwrap();
        assert_eq!(replies.len(), 1);
        let reply = replies[0];
        assert_eq!(reply.command(), HOME);
        assert_eq!(reply.target(), 1);
        assert_eq!(reply.data().unwrap(), 0);

        port.backend.append_data([1, 1, 0, 0, 0, 0]);
        port.backend.append_data([2, 1, 0, 0, 0, 0]);
        let replies = port.tx_recv_until_timeout((0, HOME)).unwrap();
        assert_eq!(replies.len(), 2);
        for (i, reply) in replies.iter().enumerate() {
            eprintln!("{:?}", reply);
            assert_eq!(reply.command(), HOME);
            assert_eq!(reply.target(), i as u8 + 1);
            assert_eq!(reply.data().unwrap(), 0);
        }
    }

    #[test]
    fn message_ids() {
        let mut port = Port::open_mock();
        assert_eq!(port.message_ids(), false);
        // Enable message IDs
        port.backend
            .append_data([1, command::untyped::SET_MESSAGE_ID_MODE, 0, 0, 0, 0]);
        let last_state = port.set_message_ids(true).unwrap();
        assert_eq!(last_state, false);
        assert_eq!(port.message_ids(), true);
    }

    #[test]
    fn compile_time_errors() {
        let t = trybuild::TestCases::new();
        t.compile_fail("test/binary/*.rs");
    }
}
