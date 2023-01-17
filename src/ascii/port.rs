//! Types for opening and using a serial port with the ASCII protocol.

#[cfg(test)]
use crate::backend::Mock;
use crate::backend::{Backend, Serial, UNKNOWN_BACKEND_NAME};
use crate::{
    ascii::{
        check,
        checksum::Lrc,
        id,
        parse::{Packet, PacketKind},
        Alert, AnyResponse, Command, CommandInstance, Info, Reply, Response, ResponseBuilder,
        Status, Target,
    },
    error::*,
    timeout_guard::TimeoutGuard,
};
use serialport as sp;
use std::convert::TryFrom;
use std::io;
use std::net::{TcpStream, ToSocketAddrs};
use std::time::Duration;

/// Options for configuring and opening a serial port.
///
/// ## Example
///
/// ```rust
/// # use zproto::ascii::OpenSerialOptions;
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
    /// Whether commands should include message IDs or not.
    generate_id: bool,
    /// Whether commands should include a checksum or not.
    generate_checksum: bool,
}

impl OpenSerialOptions {
    /// The default baud rate for the ASCII protocol: 115,200.
    pub const DEFAULT_BAUD_RATE: u32 = 115_200;

    /// Create a blank set of options ready for configuration.
    ///
    /// The default baud rate and read timeout are 115,200 and 3 seconds, respectively. Message IDs and checksums are also enabled by default.
    ///
    /// Equivalent to [`default`](OpenSerialOptions::default).
    pub fn new() -> Self {
        OpenSerialOptions {
            baud_rate: OpenSerialOptions::DEFAULT_BAUD_RATE,
            timeout: Some(Duration::from_secs(3)),
            generate_id: true,
            generate_checksum: true,
        }
    }

    /// Set a custom baud rate.
    ///
    /// The default is 115,200.
    pub fn baud_rate(&mut self, baud_rate: u32) -> &mut Self {
        self.baud_rate = baud_rate;
        self
    }

    /// Set a custom read timeout.
    ///
    /// If duration is `None`, reads will block indefinitely. The default is 3 seconds.
    pub fn timeout(&mut self, duration: Option<Duration>) -> &mut Self {
        self.timeout = duration;
        self
    }

    /// Set whether commands sent on the port should include a checksum or not.
    ///
    /// The default is `true` (checksums will be included).
    pub fn checksums(&mut self, checksum: bool) -> &mut Self {
        self.generate_checksum = checksum;
        self
    }

    /// Set whether commands sent on the port should include a message ID or not.
    ///
    /// The default is `true` (message IDs will be included).
    pub fn message_ids(&mut self, id: bool) -> &mut Self {
        self.generate_id = id;
        self
    }

    /// Open a [`Serial`] port configured for the ASCII protocol at the specified path.
    fn open_serial_port(&self, path: &str) -> Result<Serial, AsciiError> {
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
    pub fn open<'a>(&self, path: &str) -> Result<Port<'a, Serial>, AsciiError> {
        Ok(Port::from_backend(
            self.open_serial_port(path)?,
            self.generate_id,
            self.generate_checksum,
        ))
    }

    /// Open the port at the specified path with the custom options.
    ///
    /// The type of the underlying backend is erased via dynamic dispatch,
    /// which does have runtime overhead. [`OpenSerialOptions::open`] should
    /// generally be used instead, except when the type of the underlying
    /// backend may not be known at compile time.
    pub fn open_dyn<'a>(&self, path: &str) -> Result<Port<'a, Box<dyn Backend>>, AsciiError> {
        Ok(Port::from_backend(
            Box::new(self.open_serial_port(path)?),
            self.generate_id,
            self.generate_checksum,
        ))
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
/// # use zproto::ascii::OpenTcpOptions;
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
    /// Whether commands should include message IDs or not.
    generate_id: bool,
    /// Whether commands should include a checksum or not.
    generate_checksum: bool,
}

impl OpenTcpOptions {
    /// Create a blank set of options ready for configuration.
    ///
    /// The default read timeout is 3 seconds. Message IDs and checksums are also enabled by default.
    ///
    /// Equivalent to [`default`](OpenSerialOptions::default).
    pub fn new() -> Self {
        OpenTcpOptions {
            timeout: Some(Duration::from_secs(3)),
            generate_id: true,
            generate_checksum: true,
        }
    }

    /// Set a custom read timeout.
    ///
    /// If duration is `None`, reads will block indefinitely. The default is 3 seconds.
    pub fn timeout(&mut self, duration: Option<Duration>) -> &mut Self {
        self.timeout = duration;
        self
    }

    /// Set whether commands sent on the port should include a checksum or not.
    ///
    /// The default is `true` (checksums will be included).
    pub fn checksums(&mut self, checksum: bool) -> &mut Self {
        self.generate_checksum = checksum;
        self
    }

    /// Set whether commands sent on the port should include a message ID or not.
    ///
    /// The default is `true` (message IDs will be included).
    pub fn message_ids(&mut self, id: bool) -> &mut Self {
        self.generate_checksum = id;
        self
    }

    /// Open a [`TcpStream`] configured for the ASCII protocol at the specified address.
    fn open_tcp_stream<A: ToSocketAddrs>(&self, address: A) -> io::Result<TcpStream> {
        let stream = TcpStream::connect(address)?;
        stream.set_read_timeout(self.timeout)?;
        Ok(stream)
    }

    /// Open the port at the specified path with the custom options.
    pub fn open<'a, A: ToSocketAddrs>(&self, address: A) -> io::Result<Port<'a, TcpStream>> {
        Ok(Port::from_backend(
            self.open_tcp_stream(address)?,
            self.generate_id,
            self.generate_checksum,
        ))
    }

    /// Open the port at the specified path with the custom options.
    ///
    /// The type of the underlying backend is erased via dynamic dispatch,
    /// which does have runtime overhead. [`OpenTcpOptions::open`] should
    /// generally be used instead, except when the type of the underlying
    /// backend may not be known at compile time.
    pub fn open_dyn<'a, A: ToSocketAddrs>(
        &self,
        address: A,
    ) -> io::Result<Port<'a, Box<dyn Backend>>> {
        Ok(Port::from_backend(
            Box::new(self.open_tcp_stream(address)?),
            self.generate_id,
            self.generate_checksum,
        ))
    }
}

impl Default for OpenTcpOptions {
    fn default() -> Self {
        OpenTcpOptions::new()
    }
}

/// A callback that is called after a packet is either transmitted or received.
///
/// See [`Port::on_packet`] for more details.
pub type OnPacketCallback<'a> = Box<dyn FnMut(&[u8], Direction) + 'a>;

/// A wrapper around an [`OnPacketCallback`] that simply implements `Debug` so
/// that the [`Port`] can derive `Debug`.
#[repr(transparent)]
struct OnPacketCallbackDebugWrapper<'a>(OnPacketCallback<'a>);

impl<'a> std::fmt::Debug for OnPacketCallbackDebugWrapper<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "OnPacketCallback")
    }
}

/// A callback that is called when an unexpected Alert is received.
///
/// See [`Port::on_unexpected_alert`] for more details.
pub type OnUnexpectedAlertCallback<'a> = Box<dyn FnMut(Alert) -> Result<(), Alert> + 'a>;

/// A wrapper around an [`OnUnexpectedAlertCallback`] that simply implements `Debug` so
/// that the [`Port`] can derive `Debug`.
#[repr(transparent)]
struct OnUnexpectedAlertDebugWrapper<'a>(OnUnexpectedAlertCallback<'a>);

impl<'a> std::fmt::Debug for OnUnexpectedAlertDebugWrapper<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "OnUnexpectedAlertCallback")
    }
}

/// A wrapper around a boxed `Check` that simply implements `Debug` so that
/// the [`Port`] can derive `Debug`
#[repr(transparent)]
struct DefaultResponseCheckWrapper<'a>(Box<dyn check::Check<AnyResponse> + 'a>);

impl<'a> std::fmt::Debug for DefaultResponseCheckWrapper<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "DefaultResponseCheck")
    }
}

impl<'a, K: check::Check<AnyResponse> + 'a> From<K> for DefaultResponseCheckWrapper<'a> {
    fn from(other: K) -> Self {
        Self(Box::new(other))
    }
}

type DefaultCheck<R> = Option<fn(R) -> Result<R, AsciiCheckError<R>>>;

/// Returns the value expected by any function that takes an optional `checker`
/// that indicates the default check should be used (i.e. `None`).
///
/// Using this method has a few benefits over manually passing `None`:
/// * the meaning of the value is made clear by the name of the function
/// * it provides the type information necessary to satisfy Rust's type checking.
///   Otherwise, something like `None::<fn(R) -> Result<R, AsciiCheckError<R>>>`
///   would need to be passed.
const fn use_default_check<R>() -> DefaultCheck<R> {
    None
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
    packet_hook: Option<OnPacketCallbackDebugWrapper<'a>>,
    /// Optional hook to call when an unexpected Alert is received.
    unexpected_alert_hook: Option<OnUnexpectedAlertDebugWrapper<'a>>,
    /// The default check to validate all responses with
    default_response_check: Option<DefaultResponseCheckWrapper<'a>>,
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

#[cfg(test)]
impl<'a> Port<'a, Mock> {
    /// Open a mock Port. Message Id and checksums are disabled by default for easier testing.
    pub fn open_mock() -> Port<'a, Mock> {
        Port::from_backend(Mock::new(), false, false)
    }
}

impl<'a, B: Backend> Port<'a, B> {
    /// Create a `Port` from a [`Backend`] type.
    fn from_backend(backend: B, generate_id: bool, generate_checksum: bool) -> Self {
        Port {
            backend,
            ids: id::Counter::default(),
            generate_id,
            generate_checksum,
            poison: None,
            builder: ResponseBuilder::default(),
            packet_hook: None,
            unexpected_alert_hook: None,
            // Use check::default to check all responses by default
            default_response_check: Some(DefaultResponseCheckWrapper::from(
                check::AnyResponseCheck::from(check::default::<AnyResponse>()),
            )),
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
        let instance = CommandInstance::new(
            &cmd,
            &mut self.ids,
            self.generate_id,
            self.generate_checksum,
        );
        instance.write_into(&mut buffer)?;
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
        Ok(instance.id)
    }

    /// Transmit a command, receive a reply, and check it with the [`default`](Port::set_default_response_check) check.
    ///
    /// If the reply is split across multiple packets, the continuation messages will automatically be read.
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zproto::{ascii::Port, backend::Backend};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// let reply = port.command_reply("get maxspeed")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn command_reply<C: Command>(&mut self, cmd: C) -> Result<Reply, AsciiError> {
        self.internal_command_reply_with_check(cmd, use_default_check())
    }

    /// Same as [`Port::command_reply`] except that the reply is validated with the custom [`Check`](check::Check).
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zproto::{ascii::Port, backend::Backend};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zproto::ascii::check::flag_ok;
    /// // Home, but ignore any warning flags that might be present
    /// let reply = port.command_reply_with_check((1, "home"), flag_ok())?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn command_reply_with_check<C, K>(
        &mut self,
        cmd: C,
        checker: K,
    ) -> Result<Reply, AsciiError>
    where
        C: Command,
        K: check::Check<Reply>,
    {
        self.internal_command_reply_with_check(cmd, Some(checker))
    }

    /// Transmit a command and receive a reply.
    ///
    /// If any response is spread across multiple packets, continuation packets will be read.
    /// `header_check` should be a function that produces data for validating the response's header.
    ///
    /// If the `header_check` passes, the message will be converted to the desired message type `R` and then passed to
    /// `checker` to validate the contents of the message. If `checker` is `None`, then the port's default check is used.
    fn internal_command_reply_with_check<C, K>(
        &mut self,
        cmd: C,
        checker: Option<K>,
    ) -> Result<Reply, AsciiError>
    where
        C: Command,
        K: check::Check<Reply>,
    {
        let cmd = cmd.as_ref();
        let id = self.command(cmd)?;
        self.pre_receive_response();
        let response = self.receive_response(
            |_| HeaderCheckAction::Check {
                target: cmd.target(),
                id,
            },
            checker,
        )?;
        self.post_receive_response()?;
        Ok(response)
    }

    /// Transmit a command and then receive a reply and all subsequent info messages.
    ///
    /// The reply and info messages are checked with the [`default`](Port::set_default_response_check) check.
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
        self.internal_command_reply_infos_with_check(cmd, use_default_check())
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
        self.internal_command_reply_infos_with_check(cmd, Some(checker))
    }

    /// Transmit a command and then receive a reply and all subsequent info messages.
    ///
    /// If any response is spread across multiple packets, continuation packets will be read.
    /// `header_check` should be a function that produces data for validating the response's header.
    ///
    /// If the `header_check` passes, the message will be converted to the desired message type `R` and then passed to
    /// `checker` to validate the contents of the message. If `checker` is `None`, then the port's default check is used.
    fn internal_command_reply_infos_with_check<C, K>(
        &mut self,
        cmd: C,
        checker: Option<K>,
    ) -> Result<(Reply, Vec<Info>), AsciiError>
    where
        C: Command,
        K: check::Check<AnyResponse>,
    {
        let target = cmd.as_ref().target();
        let reply = self.command_reply(cmd)?;
        let old_generate_id = self.replace_message_ids(true);
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
            match self.receive_response(&header_check, gen_new_checker(&checker))? {
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

    /// Transmit a command, receive n replies, and check each reply with the [`default`](Port::set_default_response_check) check.
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
        self.internal_command_reply_n_with_check(cmd, n, use_default_check())
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
        self.internal_command_reply_n_with_check(cmd, n, Some(checker))
    }

    /// Transmit a command and then receive n replies.
    ///
    /// If any response is spread across multiple packets, continuation packets will be read.
    /// `header_check` should be a function that produces data for validating the response's header.
    ///
    /// If the `header_check` passes, the message will be converted to the desired message type `R` and then passed to
    /// `checker` to validate the contents of the message. If `checker` is `None`, then the port's default check is used.
    fn internal_command_reply_n_with_check<C, K>(
        &mut self,
        cmd: C,
        n: usize,
        checker: Option<K>,
    ) -> Result<Vec<Reply>, AsciiError>
    where
        C: Command,
        K: check::Check<Reply>,
    {
        let cmd = cmd.as_ref();
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

    /// Transmit a command, receive replies until the port times out, and check each reply with the [`default`](Port::set_default_response_check) check.
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
        self.internal_command_replies_until_timeout_with_check(cmd, use_default_check())
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
        self.internal_command_replies_until_timeout_with_check(cmd, Some(checker))
    }

    /// Transmit a command and then receive replies until the port times out.
    ///
    /// If any response is spread across multiple packets, continuation packets will be read.
    /// `header_check` should be a function that produces data for validating the response's header.
    ///
    /// The contents of the responses are validated with `checker`. If `checker` is `None`, then the port's default
    /// check is used.
    fn internal_command_replies_until_timeout_with_check<C, K>(
        &mut self,
        cmd: C,
        checker: Option<K>,
    ) -> Result<Vec<Reply>, AsciiError>
    where
        C: Command,
        K: check::Check<Reply>,
    {
        let cmd = cmd.as_ref();
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

        if let Some(OnUnexpectedAlertDebugWrapper(callback)) = &mut self.unexpected_alert_hook {
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
    /// If the `header_check` passes, the message will be converted to the desired message type `R` and then passed to
    /// `checker` to validate the contents of the message.
    fn receive_response<R, F, K>(
        &mut self,
        mut header_check: F,
        checker: Option<K>,
    ) -> Result<R, AsciiError>
    where
        R: Response,
        AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
        AsciiError: From<AsciiCheckError<R>>,
        F: FnMut(&AnyResponse) -> HeaderCheckAction,
        K: check::Check<R>,
    {
        self.check_poisoned()?;
        loop {
            let result = || -> Result<R, AsciiError> {
                let mut response = self.build_response()?;
                response = header_check(&response).check(response)?;
                if let Some(checker) = &checker {
                    let response =
                        R::try_from(response).map_err(AsciiUnexpectedResponseError::new)?;
                    checker.check(response).map_err(Into::into)
                } else if let Some(DefaultResponseCheckWrapper(checker)) =
                    &self.default_response_check
                {
                    // We need to check that the correct response type was
                    // received before we check the contents of the response.
                    // This is most easily done by actually trying to convert
                    // the type. However, the default check only accepts
                    // `AnyResponse`, so we would have to convert right back
                    // immediately afterwards. Instead, check if the conversion
                    // _would_ succeed and, if it would, continue checking the
                    // message's contents before actually converting to the
                    // expected type.
                    if !R::will_try_from_succeed(&response) {
                        return Err(AsciiUnexpectedResponseError::new(response).into());
                    }
                    checker
                        .check(response)
                        .map(|response| {
                            response.try_into().unwrap_or_else(|_| {
                                panic!("could not convert message back from AnyResponse")
                            })
                        })
                        .map_err(Into::into)
                } else {
                    Ok(R::try_from(response).map_err(AsciiUnexpectedResponseError::new)?)
                }
            }();
            if let Some(OnUnexpectedAlertDebugWrapper(callback)) = &mut self.unexpected_alert_hook {
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
    fn internal_response_n_with_check<R, F, K>(
        &mut self,
        n: usize,
        mut header_check: F,
        checker: Option<K>,
    ) -> Result<Vec<R>, AsciiError>
    where
        R: Response,
        AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
        AsciiError: From<AsciiCheckError<R>>,
        F: FnMut(&AnyResponse) -> HeaderCheckAction,
        K: check::Check<R>,
    {
        self.pre_receive_response();
        let mut responses = Vec::new();
        for _ in 0..n {
            responses.push(self.receive_response(|r| header_check(r), gen_new_checker(&checker))?);
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
    fn internal_responses_until_timeout_with_check<R, F, K>(
        &mut self,
        mut header_check: F,
        check: Option<K>,
    ) -> Result<Vec<R>, AsciiError>
    where
        R: Response,
        AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
        AsciiError: From<AsciiCheckError<R>>,
        F: FnMut(&AnyResponse) -> HeaderCheckAction,
        K: check::Check<R>,
    {
        self.pre_receive_response();
        let mut responses = Vec::new();
        loop {
            match self.receive_response(|response| header_check(response), gen_new_checker(&check))
            {
                Ok(r) => responses.push(r),
                Err(e) if e.is_timeout() => break,
                Err(e) => return Err(e),
            }
        }
        self.post_receive_response()?;
        Ok(responses)
    }

    /// Receive a response and check it with the [`default`](Port::set_default_response_check) check.
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
        let response =
            self.receive_response(|_| HeaderCheckAction::DoNotCheck, use_default_check())?;
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
        let response = self.receive_response(|_| HeaderCheckAction::DoNotCheck, Some(checker))?;
        self.post_receive_response()?;
        Ok(response)
    }

    /// Receive `n` responses and check each one with the [`default`](Port::set_default_response_check) check.
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
        self.internal_response_n_with_check(
            n,
            |_| HeaderCheckAction::DoNotCheck,
            use_default_check(),
        )
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
        self.internal_response_n_with_check(n, |_| HeaderCheckAction::DoNotCheck, Some(checker))
    }

    /// Receive responses until the port times out and check each one with the [`default`](Port::set_default_response_check) check.
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
            use_default_check(),
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
        check: K,
    ) -> Result<Vec<R>, AsciiError>
    where
        R: Response,
        K: check::Check<R>,
        AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
        AsciiError: From<AsciiCheckError<R>>,
    {
        self.internal_responses_until_timeout_with_check(
            |_| HeaderCheckAction::DoNotCheck,
            Some(check),
        )
    }

    /// Send the specified command repeatedly until the predicate returns true for a reply.
    ///
    /// The first reply to satisfy the predicate is returned. The contents of the replies are *not* checked.
    ///
    /// If any of the replies are split across multiple packets, the continuation messages will automatically be read.
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
    pub fn poll_until<C, F>(&mut self, cmd: C, mut predicate: F) -> Result<Reply, AsciiError>
    where
        C: Command,
        F: FnMut(&Reply) -> bool,
    {
        let mut reply;
        loop {
            reply = self.command_reply_with_check(cmd.as_ref(), check::unchecked())?;
            if predicate(&reply) {
                break;
            }
        }
        Ok(reply)
    }

    /// Poll the target with the empty command until the returned status is IDLE.
    ///
    /// The first reply with the IDLE status is returned. The contents of the replies are *not* checked.
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
    pub fn poll_until_idle<T: Into<Target>>(&mut self, target: T) -> Result<Reply, AsciiError> {
        self.poll_until((target.into(), ""), |reply| reply.status() == Status::Idle)
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
    ///     guard.command_reply("system reset");
    ///
    /// }  // The guard is dropped and the timeout is reset.
    ///
    /// // This command-reply uses the original timeout
    /// port.command_reply("get device.id")
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
    pub fn set_checksums(&mut self, value: bool) {
        self.generate_checksum = value;
    }

    /// Set whether commands sent on this port should include a checksum or not and return the previous value.
    pub fn replace_checksums(&mut self, value: bool) -> bool {
        std::mem::replace(&mut self.generate_checksum, value)
    }

    /// Get whether the port will include checksums or not in commands.
    pub fn checksums(&self) -> bool {
        self.generate_checksum
    }

    /// Set whether commands sent on this port should include an automatically generated message ID or not.
    ///
    /// The previous value is returned.
    pub fn set_message_ids(&mut self, value: bool) {
        self.generate_id = value;
    }

    /// Set whether commands sent on this port should include an automatically generated message ID or not.
    ///
    /// The previous value is returned.
    pub fn replace_message_ids(&mut self, value: bool) -> bool {
        std::mem::replace(&mut self.generate_id, value)
    }

    /// Get whether the port will include message IDs or not in commands.
    pub fn message_ids(&self) -> bool {
        self.generate_id
    }

    /// Set the read timeout.
    ///
    /// If timeout is `None`, reads will block indefinitely.
    pub fn set_read_timeout(&mut self, timeout: Option<Duration>) -> Result<(), io::Error> {
        self.backend.set_read_timeout(timeout)
    }

    /// Set the read timeout and return the old timeout.
    ///
    /// If timeout is `None`, reads will block indefinitely.
    pub fn replace_read_timeout(
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

    /// Set how the contents of responses will be checked when they are received.
    ///
    /// The contents of all responses are checked by the port when they are
    /// received using the `checker` defined here. By default the `Port` uses
    /// [`check::default`], unless it is explicit set otherwise. To temporarily
    /// change how the contents of responses are checked, it is advisable to use
    /// one of the `Port`'s [`*_with_check`](Port::command_reply_with_check)
    /// methods, which take a temporary override that will be used for that call.
    ///
    /// The default `checker` can accept any [`Response`] type (i.e., [`Reply`],
    /// [`Info`], [`Alert`], or [`AnyResponse`]). However, if a different
    /// response type is received, it will _not_ be checked unless the received
    /// type is [`AnyResponse`] and the variant contains the correct type.
    ///
    /// For example, suppose [`flag_ok`](check::flag_ok) is set as the default
    /// check. If a [`Reply`] is received, which is the type `flag_ok`
    /// accepts, it will be checked. An [`AnyResponse::Reply`] will also be
    /// checked. But if an [`Info`], [`Alert`], or other variant of
    /// [`AnyResponse`] are received they will not be checked.
    ///
    /// ```
    /// # use zproto::backend::Backend;
    /// # use zproto::ascii::{Alert, AnyResponse, Port};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zproto::ascii::check;
    /// port.set_default_response_check(check::flag_ok());
    ///
    /// // The Reply to the `home` command will be checked to ensure the flag is
    /// // OK, because it matches the type of the default check.
    /// port.command_reply("home")?;
    ///
    /// // When the response type is `AnyResponse`, it will also be checked if
    /// // it is the `AnyResponse::Reply` variant, but not if it is any other
    /// // variant.
    /// port.command("home")?;
    /// port.response::<AnyResponse>()?;
    ///
    /// // Other types of responses, such as an `Alert`, will not be checked.
    /// let response = port.response::<Alert>()?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// To check different kinds of responses (say replies _and_ alerts), the
    /// supplied checker should accept `AnyResponse`s.
    ///
    /// ```
    /// # use zproto::backend::Backend;
    /// # use zproto::ascii::{Alert, AnyResponse, Port, Status};
    /// # use zproto::error::{AsciiCheckError, AsciiCheckStatusError};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// // A silly check to ensure that all status' are IDLE.
    /// fn always_idle(response: AnyResponse) -> Result<AnyResponse, AsciiCheckError<AnyResponse>> {
    ///     match &response {
    ///         AnyResponse::Reply(reply) if reply.status() == Status::Idle => Ok(response),
    ///         AnyResponse::Alert(alert) if alert.status() == Status::Idle => Ok(response),
    ///         AnyResponse::Info(_) => Ok(response), // Info messages don't have a status
    ///         _ => Err(AsciiCheckStatusError::new(Status::Idle, response).into())
    ///     }
    /// }
    /// port.set_default_response_check(always_idle);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// In some cases, the compiler will not be able to infer the response type
    /// the check accepts. For instance, [`check::warning_is_none`] can accept
    /// replies or alerts as both have warnings.
    ///
    /// ```compile_fail
    /// # use zproto::backend::Backend;
    /// # use zproto::ascii::{check, Port};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// // Compiler error because it isn't clear if this should check replies or alerts
    /// port.set_default_response_check(check::warning_is_none());
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Provide an explicit type to solve the compiler error.
    ///
    /// ```
    /// # use zproto::backend::Backend;
    /// # use zproto::ascii::{check, Port, Reply};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// // Only check warning flags in replies.
    /// port.set_default_response_check(check::warning_is_none::<Reply>());
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// To restore the checks set when the `Port` was created use the following:
    ///
    /// ```
    /// # use zproto::backend::Backend;
    /// # use zproto::ascii::{check, AnyResponse, Port};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// port.set_default_response_check(check::default::<AnyResponse>());
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Use [`clear_default_response_check`](Port::clear_default_response_check)
    /// to not check the contents of responses.
    pub fn set_default_response_check<K, R>(&mut self, checker: K)
    where
        K: check::Check<R> + 'a,
        R: Response,
        check::AnyResponseCheck<K, R>: check::Check<AnyResponse> + 'a,
    {
        self.default_response_check = Some(DefaultResponseCheckWrapper::from(
            check::AnyResponseCheck::from(checker),
        ));
    }

    /// Set how the contents of responses will be checked. The previous default
    /// check will be returned, if there was one.
    ///
    /// See [`set_default_response_check`](Port::set_default_response_check) for
    /// more details on default response checks.
    pub fn replace_default_response_check(
        &mut self,
        checker: impl check::Check<AnyResponse> + 'a,
    ) -> Option<Box<dyn check::Check<AnyResponse> + 'a>> {
        self.default_response_check
            .replace(DefaultResponseCheckWrapper(Box::new(checker)))
            .map(|wrapper| wrapper.0)
    }

    /// Clear the default response check. The contents of responses will not be
    /// checked.
    ///
    /// In most cases this is not recommended. In the vast majority of cases
    /// reply flags should be checked, ensuring that they are OK.
    ///
    /// See [`set_default_response_check`](Port::set_default_response_check) for
    /// more details on default response checks.
    pub fn clear_default_response_check(
        &mut self,
    ) -> Option<Box<dyn check::Check<AnyResponse> + 'a>> {
        self.default_response_check.take().map(|wrapper| wrapper.0)
    }

    /// Get the default response check.
    ///
    /// If no default check is set, `None` is returned, indicating the `Port` is
    /// not checking the contents of responses.
    ///
    /// See [`set_default_response_check`](Port::set_default_response_check) for
    /// more details on default response checks.
    pub fn default_response_check(&self) -> Option<&(dyn check::Check<AnyResponse> + 'a)> {
        self.default_response_check
            .as_ref()
            .map(|wrapper| &*wrapper.0)
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

    /// Set a callback that will be called immediately after any ASCII packet is
    /// sent or received.
    ///
    /// If a previous callback was set, it is returned.
    ///
    /// To clear a previously registered callback use [`clear_on_packet`](Port::clear_on_packet).
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
    /// port.on_packet(|packet, dir| {
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
    /// port.on_packet(|packet, _| {
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
    pub fn on_packet<F>(&mut self, callback: F) -> Option<OnPacketCallback<'a>>
    where
        F: FnMut(&[u8], Direction) + 'a,
    {
        std::mem::replace(
            &mut self.packet_hook,
            Some(OnPacketCallbackDebugWrapper(Box::new(callback))),
        )
        .map(|wrapper| wrapper.0)
    }

    /// Clear any callback registered via [`on_packet`](Port::on_packet) and return it.
    pub fn clear_on_packet(&mut self) -> Option<OnPacketCallback<'a>> {
        self.packet_hook.take().map(|wrapper| wrapper.0)
    }

    /// Set a callback that will be called whenever an unexpected Alert is
    /// received.
    ///
    /// If a previous callback was set, it is returned.
    ///
    /// To clear a previously registered callback use [`clear_on_unexpected_alert`](Port::clear_on_unexpected_alert).
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
    /// port.on_unexpected_alert(|_alert| Ok(()));
    /// let (_reply, _infos) = port.command_reply_infos((1, "storage all print"))?;
    /// // ...
    /// # Ok(())
    /// # }
    /// ```
    pub fn on_unexpected_alert<F>(&mut self, callback: F) -> Option<OnUnexpectedAlertCallback<'a>>
    where
        F: FnMut(Alert) -> Result<(), Alert> + 'a,
    {
        std::mem::replace(
            &mut self.unexpected_alert_hook,
            Some(OnUnexpectedAlertDebugWrapper(Box::new(callback))),
        )
        .map(|wrapper| wrapper.0)
    }

    /// Clear any callback registered via [`on_unexpected_alert`](Port::on_unexpected_alert) and return it.
    pub fn clear_on_unexpected_alert(&mut self) -> Option<OnUnexpectedAlertCallback<'a>> {
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

/// Given an optional checker, generate a new one with appropriate lifetime
/// constraints and types.
//
// Inlining helps ensure that the extra function call boundaries will be
// optimized away.
#[inline(always)]
fn gen_new_checker<'a, 'b: 'a, R: Response>(
    checker: &'b Option<impl check::Check<R>>,
) -> Option<impl check::Check<R> + 'a> {
    checker
        .as_ref()
        .map(|checker| move |response| checker.check(response))
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

#[cfg(test)]
mod test {
    use std::cell::Cell;

    use crate::{
        ascii::{
            check::{self, unchecked},
            Alert, AnyResponse, Port, Reply,
        },
        backend::Mock,
        error::*,
    };

    #[test]
    fn command_reply_ok() {
        let mut port = Port::open_mock();
        port.backend.append_data(b"@01 0 OK IDLE -- 0\r\n");
        let reply = port.command_reply("").unwrap();
        assert_eq!(reply.target(), (1, 0).into());

        // Multi-packet Reply
        {
            let backend = &mut port.backend;
            backend.append_data(b"@01 0 OK IDLE -- part1\\\r\n");
            backend.append_data(b"#01 0 cont part2a part2b\\\r\n");
            backend.append_data(b"#01 0 cont part3\r\n");
        }
        let reply = port.command_reply("").unwrap();
        assert_eq!(reply.data(), "part1 part2a part2b part3");
    }

    #[test]
    fn command_reply_fail() {
        let mut port = Port::open_mock();

        // Incorrect kind
        port.backend.append_data(b"!01 0 IDLE FF 0\r\n");
        let err = port.command_reply("").unwrap_err();
        assert!(matches!(err, AsciiError::UnexpectedResponse(_)), "{err:?}");

        // Incorrect target
        port.backend.append_data(b"!02 0 IDLE FF 0\r\n");
        let err = port.command_reply((1, "")).unwrap_err();
        assert!(matches!(err, AsciiError::UnexpectedResponse(_)));

        // Unexpected Alert interleaved in reply packets.
        {
            let backend = &mut port.backend;
            backend.append_data(b"@01 0 OK IDLE -- part1\\\r\n");
            backend.append_data(b"!02 0 IDLE -- 0\r\n");
            backend.append_data(b"#01 0 cont part2\r\n");
        }
        let err = port.command_reply((1, "")).unwrap_err();
        assert!(matches!(err, AsciiError::UnexpectedResponse(_)));

        // Unexpected and incomplete Alert interleaved in reply packets.
        {
            let backend = &mut port.backend;
            backend.append_data(b"@01 0 OK IDLE -- part1\\\r\n");
            backend.append_data(b"!02 0 IDLE -- something\\\r\n");
            backend.append_data(b"#01 0 cont part2\r\n");
        }
        let err = port.command_reply((1, "")).unwrap_err();
        assert!(matches!(err, AsciiError::UnexpectedPacket(_)));
    }

    #[test]
    fn command_reply_unexpected_alert() {
        let alert_count = Cell::new(0);

        let mut port = Port::open_mock();
        port.on_unexpected_alert(|_alert| {
            alert_count.set(alert_count.get() + 1);
            Ok(()) // Consume any alert
        });

        port.backend.append_data(b"!01 0 IDLE --\r\n");
        port.backend.append_data(b"@01 0 OK IDLE -- 0\r\n");
        let reply = port.command_reply("").unwrap();
        assert_eq!(reply.target(), (1, 0).into());
        assert_eq!(alert_count.get(), 1);

        // Multi-packet Reply
        {
            let backend = &mut port.backend;
            backend.append_data(b"@01 0 OK IDLE -- part1\\\r\n");
            backend.append_data(b"#01 0 cont part2a part2b\\\r\n");
            backend.append_data(b"!02 1 IDLE --\r\n");
            backend.append_data(b"!02 1 IDLE --\r\n");
            backend.append_data(b"#01 0 cont part3\r\n");
        }
        let reply = port.command_reply("").unwrap();
        assert_eq!(reply.data(), "part1 part2a part2b part3");
        assert_eq!(alert_count.get(), 3);
    }

    #[test]
    fn command_reply_n_ok() {
        let mut port = Port::open_mock();
        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- 0\r\n");
            buf.append_data(b"@02 0 OK IDLE -- 0\r\n");
        }
        let _ = port.command_reply_n("", 2).unwrap();

        // Interleaved multi-packet replies
        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- 1part1\\\r\n");
            buf.append_data(b"@02 0 OK IDLE -- 2part1\\\r\n");
            buf.append_data(b"#02 0 cont 2part2\r\n");
            buf.append_data(b"#01 0 cont 1part2\r\n");
        }
        let replies = port.command_reply_n("", 2).unwrap();
        let reply_data: Vec<_> = replies.iter().map(|r| r.data()).collect();
        assert_eq!(reply_data, &["1part1 1part2", "2part1 2part2"]);
    }

    #[test]
    fn command_reply_n_fail() {
        let mut port = Port::open_mock();

        // Timeout waiting for non-existent message.
        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- 0\r\n");
            buf.append_data(b"@02 0 OK IDLE -- 0\r\n");
        }
        let err = port.command_reply_n("", 3).unwrap_err();
        assert!(err.is_timeout());

        // Timeout waiting for non-existent packet.
        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- 0\\\r\n");
            buf.append_data(b"@02 0 OK IDLE -- 0\r\n");
        }
        let err = port.command_reply_n("", 2).unwrap_err();
        assert!(err.is_timeout());
    }

    #[test]
    fn command_reply_n_unexpected_alert() {
        let alert_count = Cell::new(0);

        let mut port = Port::open_mock();
        port.on_unexpected_alert(|_alert| {
            alert_count.set(alert_count.get() + 1);
            Ok(()) // Consume any alert
        });

        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- 0\r\n");
            buf.append_data(b"!03 0 IDLE --\r\n");
            buf.append_data(b"@02 0 OK IDLE -- 0\r\n");
        }
        let _ = port.command_reply_n("", 2).unwrap();
        assert_eq!(alert_count.get(), 1);

        // Interleaved multi-packet replies
        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- 1part1\\\r\n");
            buf.append_data(b"!03 0 IDLE --\r\n");
            buf.append_data(b"@02 0 OK IDLE -- 2part1\\\r\n");
            buf.append_data(b"!04 0 IDLE --\r\n");
            buf.append_data(b"#02 0 cont 2part2\r\n");
            buf.append_data(b"#01 0 cont 1part2\r\n");
            buf.append_data(b"!05 0 IDLE --\r\n"); // Shouldn't be read
        }
        let replies = port.command_reply_n("", 2).unwrap();
        let reply_data: Vec<_> = replies.iter().map(|r| r.data()).collect();
        assert_eq!(reply_data, &["1part1 1part2", "2part1 2part2"]);
        assert_eq!(alert_count.get(), 3);
    }

    #[test]
    fn command_replies_until_timeout_ok() {
        let mut port = Port::open_mock();
        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- 0\r\n");
            buf.append_data(b"@02 0 OK IDLE -- 0\r\n");
        }
        let replies = port.command_replies_until_timeout("").unwrap();
        assert_eq!(replies.len(), 2);
    }

    #[test]
    fn command_replies_mixed_cont_until_timeout_ok() {
        let expected = &["part 1a part 1b", "part 2a part 2b"];

        let mut port = Port::open_mock();
        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- part 1a\\\r\n");
            buf.append_data(b"@02 0 OK IDLE -- part 2a\\\r\n");
            buf.append_data(b"#01 0 cont part 1b\r\n");
            buf.append_data(b"#02 0 cont part 2b\r\n");
        }
        let replies = port.command_replies_until_timeout("").unwrap();
        let reply_data: Vec<_> = replies.iter().map(|r| r.data()).collect();
        assert_eq!(reply_data, expected);

        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- part 1a\\\r\n");
            buf.append_data(b"@02 0 OK IDLE -- part 2a\\\r\n");
            buf.append_data(b"#02 0 cont part 2b\r\n");
            buf.append_data(b"#01 0 cont part 1b\r\n");
        }
        let replies = port.command_replies_until_timeout("").unwrap();
        let reply_data: Vec<_> = replies.iter().map(|r| r.data()).collect();
        // When the continuations come shouldn't change the response order.
        assert_eq!(reply_data, expected);

        {
            let buf = &mut port.backend;
            buf.append_data(b"@02 0 OK IDLE -- part 2a\\\r\n");
            buf.append_data(b"@01 0 OK IDLE -- part 1a\\\r\n");
            buf.append_data(b"#02 0 cont part 2b\r\n");
            buf.append_data(b"#01 0 cont part 1b\r\n");
        }
        let replies = port.command_replies_until_timeout("").unwrap();
        let reply_data: Vec<_> = replies.iter().map(|r| r.data()).collect();
        // The initial packet order should change the response order.
        assert_eq!(
            reply_data,
            expected.iter().copied().rev().collect::<Vec<_>>()
        );
    }

    #[test]
    fn command_replies_until_timeout_fail() {
        let mut port = Port::open_mock();
        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 1 OK IDLE -- 0\r\n");
            buf.append_data(b"@02 2 OK IDLE -- 0\r\n"); // Wrong axis number
            buf.append_data(b"!03 1 IDLE -- 0\r\n"); // Wrong kind
        }
        let err = port
            .command_replies_until_timeout(((0, 1), "get pos")) // To all first axes
            .unwrap_err();
        assert!(matches!(err, AsciiError::UnexpectedResponse(_)));
    }

    #[test]
    fn command_replies_until_timeout_unexpected_alert() {
        let alert_count = Cell::new(0);

        let mut port = Port::open_mock();
        port.on_unexpected_alert(|_alert| {
            alert_count.set(alert_count.get() + 1);
            Ok(()) // Consume any alert
        });

        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- 0\r\n");
            buf.append_data(b"!03 0 IDLE --\r\n");
            buf.append_data(b"@02 0 OK IDLE -- 0\r\n");
            buf.append_data(b"!04 0 IDLE --\r\n");
        }
        let replies = port.command_replies_until_timeout("").unwrap();
        assert_eq!(replies.len(), 2);
        assert_eq!(alert_count.get(), 2);
    }

    #[test]
    fn command_replies_mixed_cont_until_timeout_unexpected_alert() {
        let expected = &["part 1a part 1b", "part 2a part 2b"];

        let alert_count = Cell::new(0);

        let mut port = Port::open_mock();
        port.on_unexpected_alert(|_alert| {
            alert_count.set(alert_count.get() + 1);
            Ok(()) // Consume any alert
        });

        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- part 1a\\\r\n");
            buf.append_data(b"!03 0 IDLE --\r\n");
            buf.append_data(b"@02 0 OK IDLE -- part 2a\\\r\n");
            buf.append_data(b"#01 0 cont part 1b\r\n");
            buf.append_data(b"!04 0 IDLE --\r\n");
            buf.append_data(b"#02 0 cont part 2b\r\n");
            buf.append_data(b"!05 0 IDLE --\r\n");
        }
        let replies = port.command_replies_until_timeout("").unwrap();
        let reply_data: Vec<_> = replies.iter().map(|r| r.data()).collect();
        assert_eq!(reply_data, expected);
        assert_eq!(alert_count.get(), 3);
    }

    #[test]
    fn response_until_timeout_ok() {
        let mut port = Port::open_mock();

        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- 0\r\n");
            buf.append_data(b"@02 0 OK IDLE -- 0\r\n");
        }
        let replies: Vec<AnyResponse> = port.responses_until_timeout().unwrap();
        let reply_data: Vec<_> = replies.iter().map(|r| r.data()).collect();
        assert_eq!(reply_data, &["0", "0"]);

        // Multi-packet info messages
        {
            let buf = &mut port.backend;
            buf.append_data(b"#01 0 part 1a\\\r\n");
            buf.append_data(b"#02 0 part 2a\\\r\n");
            buf.append_data(b"#01 0 cont part 1b\r\n");
            buf.append_data(b"#02 0 cont part 2b\r\n");
        }
        let replies: Vec<AnyResponse> = port.responses_until_timeout().unwrap();
        let reply_data: Vec<_> = replies.iter().map(|r| r.data()).collect();
        assert_eq!(reply_data, &["part 1a part 1b", "part 2a part 2b"]);
    }

    #[test]
    fn response_until_timeout_fail() {
        let mut port = Port::open_mock();

        // Received an alert part way should not read following messages.
        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- 0\r\n");
            buf.append_data(b"!02 1 IDLE -- \r\n");
            buf.append_data(b"@02 0 OK IDLE -- 0\r\n");
        }
        let err = port.responses_until_timeout::<Reply>().unwrap_err();
        assert!(matches!(err, AsciiError::UnexpectedResponse(_)));
        // Can read the final reply
        let _ = port.response::<Reply>().unwrap();

        // Invalid continuation packet
        {
            let buf = &mut port.backend;
            buf.append_data(b"@01 0 OK IDLE -- 0\r\n");
            buf.append_data(b"#01 0 cont something\r\n");
        }
        let err = port.responses_until_timeout::<Reply>().unwrap_err();
        assert!(matches!(err, AsciiError::UnexpectedPacket(_)));
    }

    /// Ensure that explicitly reading an alert message while an `on_unexpected_alert`
    /// callback is configured, does not trigger the callback.
    #[test]
    fn explicit_alert_response_does_not_trigger_unexpected_alert_callback() {
        use crate::ascii::Alert;

        let alert_count = Cell::new(0);

        let mut port = Port::open_mock();
        port.on_unexpected_alert(|_alert| {
            alert_count.set(alert_count.get() + 1);
            Ok(()) // Consume any alert
        });

        {
            let buf = &mut port.backend;
            buf.append_data(b"!01 0 IDLE --\r\n");
        }
        let _ = port.response::<Alert>().unwrap();
        assert_eq!(alert_count.get(), 0);
    }

    /// Ensure that setting explicit types is possible for all `*_with_check`
    /// methods. This inferencing was previously forbidden because `arg: impl Bound`
    /// syntax was used. Replacing that syntax with standard `where` bounds
    /// allows for the explicit type
    #[test]
    fn type_inference_regression_test() {
        use super::check::default;

        let mut port = Port::open_mock();
        let _ = port.response_with_check::<AnyResponse, _>(default());
        let _ = port.response_n_with_check::<AnyResponse, _>(2, default());
        let _ = port.responses_until_timeout_with_check::<AnyResponse, _>(default());
    }

    #[test]
    fn set_replace_id() {
        let mut port = Port::open_mock();
        assert_eq!(port.message_ids(), false);
        assert_eq!(port.replace_message_ids(true), false);
        assert_eq!(port.message_ids(), true);
        port.set_message_ids(false);
        assert_eq!(port.replace_message_ids(true), false);
        assert_eq!(port.message_ids(), true);
    }

    #[test]
    fn set_replace_checksums() {
        let mut port = Port::open_mock();
        assert_eq!(port.checksums(), false);
        assert_eq!(port.replace_checksums(true), false);
        assert_eq!(port.checksums(), true);
        port.set_checksums(false);
        assert_eq!(port.replace_checksums(true), false);
        assert_eq!(port.checksums(), true);
    }

    #[test]
    fn read_packet_bytes() {
        struct Case<'a> {
            input: &'a [&'a [u8]],
            expected: Result<&'a [u8], AsciiError>,
        }
        let test_cases: &[Case] = &[
            Case {
                input: &[b"/\r\n"],
                expected: Ok(b"/\r\n"),
            },
            Case {
                input: &[b"/\n"],
                expected: Ok(b"/\n"),
            },
            Case {
                input: &[b"/\r"],
                expected: Err(AsciiPacketMissingEndError::new(b"/\r").into()),
            },
            Case {
                input: &[b"!\r\n"],
                expected: Ok(b"!\r\n"),
            },
            Case {
                input: &[b"#\r\n"],
                expected: Ok(b"#\r\n"),
            },
            Case {
                input: &[b"@\r\n"],
                expected: Ok(b"@\r\n"),
            },
            Case {
                input: &[b"\0\t\r@01 1 OK IDLE --\r\n"],
                expected: Ok(b"@01 1 OK IDLE --\r\n"),
            },
            Case {
                input: &[b"  /1 1 tools echo / this\n"],
                expected: Err(AsciiPacketMissingEndError::new(b"/1 1 tools echo ").into()),
            },
            Case {
                input: &[b"  /1 1 tools echo\nextra"],
                expected: Ok(b"/1 1 tools echo\n"),
            },
            Case {
                input: &[b"\r\n"],
                expected: Err(AsciiPacketMissingStartError::new(b"").into()),
            },
            Case {
                input: &[b"/anything here"],
                expected: Err(AsciiPacketMissingEndError::new(b"/anything here").into()),
            },
        ];
        let mut port = Port::open_mock();
        for case in test_cases {
            port.backend.clear_buffer();
            for packet in case.input {
                port.backend.append_data(packet);
            }
            let actual = port.read_packet_bytes();
            match &case.expected {
                Ok(expected) => {
                    assert_eq!(*expected, actual.expect("expected OK"));
                }
                Err(AsciiError::PacketMissingStart(packet)) => {
                    match &actual.expect_err("expected Err") {
                        AsciiError::PacketMissingStart(actual) => assert_eq!(packet, actual),
                        e => panic!("unexpected error: {e:?}"),
                    }
                }
                Err(AsciiError::PacketMissingEnd(packet)) => {
                    match &actual.expect_err("expected Err") {
                        AsciiError::PacketMissingEnd(actual) => assert_eq!(packet, actual),
                        e => panic!("unexpected error: {e:?}"),
                    }
                }
                Err(_) => panic!("unsupported test case"),
            }
        }
    }

    #[test]
    fn on_packet() {
        use std::cell::RefCell;

        let responses = [
            b"@01 0 OK IDLE -- 0\r\n".as_ref(),
            b"!02 1 IDLE -- \r\n".as_ref(),
            b"@02 0 OK IDLE -- 0\r\n".as_ref(),
        ];
        // `captured` must be declared before the port so that it is dropped after it.
        // This is necessary because the port holds a reference to `captured`
        // via the on_packet closure.
        let captured = RefCell::new(Vec::new());
        let mut port = Port::open_mock();
        {
            let buf = &mut port.backend;
            for response in &responses {
                buf.append_data(*response);
            }
        }
        port.on_packet(|data, dir| {
            if let Ok(mut buffer) = captured.try_borrow_mut() {
                buffer.push((data.to_vec(), dir));
            }
        });

        port.command((1, 3, "get pos")).unwrap();
        let _ = port.response_n::<AnyResponse>(3).unwrap();

        let mut expected = Vec::with_capacity(4);
        expected.push((b"/1 3 get pos\n".to_vec(), super::Direction::Tx));
        for response in responses {
            expected.push((response.to_vec(), super::Direction::Recv));
        }
        assert_eq!(captured.borrow().as_slice(), expected.as_slice());
    }

    // Poison a port
    fn poison_port(port: &mut Port<Mock>) {
        use std::{io, time::Duration};
        let mut guard = port.timeout_guard(Some(Duration::from_secs(1))).unwrap();
        guard
            .backend
            .set_read_timeout_error(Some(io::Error::new(io::ErrorKind::Other, "OOPS!").into()));
    }

    /// Assert that a result contains a poisoning error.
    fn assert_poisoned<T: std::fmt::Debug>(result: Result<T, AsciiError>) {
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(is_poisoning_error(&err), "{} is not a poisoning error", err);
    }

    /// Assert that the result does not contain a poisoning error.
    fn assert_not_poisoned<T: std::fmt::Debug>(result: Result<T, AsciiError>) {
        match result {
            Err(ref err) => {
                assert!(!is_poisoning_error(err), "{} is a poisoning error", err);
            }
            _ => {}
        }
    }

    /// Return true if the error is a poisoning error.
    fn is_poisoning_error(err: &AsciiError) -> bool {
        use std::io;

        let mut poisoning = false;
        if let AsciiError::Io(e) = err {
            if e.kind() == io::ErrorKind::Other {
                let message = format!("{}", e);
                poisoning = message.starts_with("failed to reset") && message.contains("OOPS!");
            }
        }
        poisoning
    }

    /// Generate a test with the given Port `$method`, which ensures that calling
    /// `$method` with the specified `$args` surfaces the unhandled error created
    /// in the [`TimeoutGuard`]'s drop implementation.
    macro_rules! make_poison_test {
		// Case: the Port method to call includes types parameters
		(
			$method:ident::< $($types:ty),+ > $(,)? $($args:expr),*
		) => {
			make_poison_test!(@create $method, $($types),+ ; $($args),* );
		};

		// Case: the Port method to call does not include type parameters
		(
			$method:ident $(,)? $($args:expr),*
		) => {
			make_poison_test!(@create $method, ; $($args),* );
		};

		// Internal case: create the test function.
		(@create $method:ident, $($types:ty),* ; $($args:expr),* ) => {
			paste::paste! { // For generating new identifiers
				#[test]
				fn [<poisoned_ $method>]() {
					// Create a poisoned port and check that $method surfaces the
					// poisoning error.
					let mut port = Port::open_mock();
					poison_port(&mut port);
					let result = port.$method::< $($types),* >( $($args),* );
					assert_poisoned(result.map_err(Into::into));

					// Subsequent calls should not surface the poisoning error.
					let result = port.$method::< $($types),* >( $($args),* );
					assert_not_poisoned(result.map_err(Into::into));
				}
			}
		};
	}

    make_poison_test!(command, "");
    make_poison_test!(command_reply, "");
    make_poison_test!(command_reply_infos, "");
    make_poison_test!(command_reply_infos_with_check, "", unchecked());
    make_poison_test!(command_reply_n, "", 1);
    make_poison_test!(command_reply_n_with_check, "", 1, unchecked());
    make_poison_test!(command_reply_with_check, "", unchecked());
    make_poison_test!(poll_until, "", |_| true);
    make_poison_test!(poll_until_idle, 1);
    make_poison_test!(response::<AnyResponse>);
    make_poison_test!(response_n::<AnyResponse>, 1);
    make_poison_test!(response_n_with_check, 1, unchecked::<AnyResponse>());
    make_poison_test!(response_with_check, unchecked::<AnyResponse>());
    make_poison_test!(responses_until_timeout::<AnyResponse>);
    make_poison_test!(
        responses_until_timeout_with_check,
        unchecked::<AnyResponse>()
    );
    make_poison_test!(timeout_guard, None);
}
