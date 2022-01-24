//! Types for opening and using a serial port with the ASCII protocol.

#[cfg(doc)]
use crate::ascii::Alert;
#[cfg(test)]
use crate::backend::Mock;
use crate::backend::{Backend, Serial, UNKNOWN_BACKEND_NAME};
use crate::{
    ascii::check,
    ascii::checksum::Lrc,
    ascii::id,
    ascii::{AnyResponse, Command, Id, Info, Packet, Reply, Response, Status, Target},
    error::*,
    timeout_guard::TimeoutGuard,
};
use serialport as sp;
use std::convert::TryFrom;
use std::io::{self, BufRead, BufReader};
use std::net::{TcpStream, ToSocketAddrs};
use std::time::Duration;

/// Options for configuring and opening a serial port.
///
/// ## Example
///
/// ```rust
/// # use zaber_protocol::ascii::OpenSerialOptions;
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
    /// The default for how message IDs should be created (or not) when sending commands.
    default_id: Id,
    /// The default for whether commands should include a checksum or not.
    default_checksum: bool,
}

impl OpenSerialOptions {
    /// The default baud rate for the ASCII protocol.
    const DEFAULT_BAUD_RATE: u32 = 115_200;

    /// Create a blank set of options ready for configuration.
    ///
    /// The default baud rate and read timeout are 115,200 and 5 seconds, respectively. Message IDs and checksums are also enabled by default.
    ///
    /// Equivalent to [`default`](OpenSerialOptions::default).
    pub fn new() -> Self {
        OpenSerialOptions {
            baud_rate: OpenSerialOptions::DEFAULT_BAUD_RATE,
            timeout: Some(Duration::from_secs(5)),
            default_id: Id::Generate,
            default_checksum: true,
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

    /// Set whether commands sent on the port should include a checksum by default or not.
    pub fn checksum(&mut self, checksum: bool) -> &mut Self {
        self.default_checksum = checksum;
        self
    }

    /// Set whether commands sent on the port should include a message ID by default or not.
    pub fn id(&mut self, id: bool) -> &mut Self {
        self.default_id = if id { Id::Generate } else { Id::None };
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
    pub fn open(&self, path: &str) -> Result<Port<Serial>, AsciiError> {
        Ok(Port::from_backend(
            self.open_serial_port(path)?,
            self.default_id,
            self.default_checksum,
        ))
    }

    /// Open the port at the specified path with the custom options.
    ///
    /// The type of the underlying backend is erased via dynamic dispatch,
    /// which does have runtime overhead. [`OpenSerialOptions::open`] should
    /// generally be used instead, except when the type of the underlying
    /// backend may not be known at compile time.
    pub fn open_dyn(&self, path: &str) -> Result<Port<Box<dyn Backend>>, AsciiError> {
        Ok(Port::from_backend(
            Box::new(self.open_serial_port(path)?),
            self.default_id,
            self.default_checksum,
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
/// # use zaber_protocol::ascii::OpenTcpOptions;
/// # use std::time::Duration;
/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
/// let mut port = OpenTcpOptions::new()
///     .timeout(Some(Duration::from_millis(50)))
///     .open("192.168.0.1:7770")?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug)]
pub struct OpenTcpOptions {
    /// The custom timeout
    timeout: Option<Duration>,
    /// The default for how message IDs should be created (or not) when sending commands.
    default_id: Id,
    /// The default for whether commands should include a checksum or not.
    default_checksum: bool,
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
            default_id: Id::Generate,
            default_checksum: true,
        }
    }

    /// Set a custom read timeout.
    ///
    /// If duration is `None`, reads will block indefinitely.
    pub fn timeout(&mut self, duration: Option<Duration>) -> &mut Self {
        self.timeout = duration;
        self
    }

    /// Set whether commands sent on the port should include a checksum by default or not.
    pub fn checksum(&mut self, checksum: bool) -> &mut Self {
        self.default_checksum = checksum;
        self
    }

    /// Set whether commands sent on the port should include a message ID by default or not.
    pub fn id(&mut self, id: bool) -> &mut Self {
        self.default_id = if id { Id::Generate } else { Id::None };
        self
    }

    /// Open a [`TcpStream`] configured for the ASCII protocol at the specified address.
    fn open_tcp_stream<A: ToSocketAddrs>(&self, address: A) -> io::Result<TcpStream> {
        let stream = TcpStream::connect(address)?;
        stream.set_read_timeout(self.timeout)?;
        Ok(stream)
    }

    /// Open the port at the specified path with the custom options.
    pub fn open<A: ToSocketAddrs>(&self, address: A) -> io::Result<Port<TcpStream>> {
        Ok(Port::from_backend(
            self.open_tcp_stream(address)?,
            self.default_id,
            self.default_checksum,
        ))
    }

    /// Open the port at the specified path with the custom options.
    ///
    /// The type of the underlying backend is erased via dynamic dispatch,
    /// which does have runtime overhead. [`OpenTcpOptions::open`] should
    /// generally be used instead, except when the type of the underlying
    /// backend may not be known at compile time.
    pub fn open_dyn<A: ToSocketAddrs>(&self, address: A) -> io::Result<Port<Box<dyn Backend>>> {
        Ok(Port::from_backend(
            Box::new(self.open_tcp_stream(address)?),
            self.default_id,
            self.default_checksum,
        ))
    }
}

impl Default for OpenTcpOptions {
    fn default() -> Self {
        OpenTcpOptions::new()
    }
}

/// A port configured to use the ASCII protocol.
///
/// A port is parameterized by some [`Backend`] type, `B`. Use the convenience
/// methods [`Port::open_serial`] and [`Port::open_tcp`] to construct a serial
/// port (`Port<Serial>`) or a TCP port (`Port<TcpStream>`). To customize the
/// construction of these types, or to construct a port with a dynamic backend,
/// use the [`OpenSerialOptions`] and [`OpenTcpOptions`] builder types.
#[derive(Debug)]
pub struct Port<B> {
    /// The underlying backend
    backend: BufReader<B>,
    /// The message ID generator
    ids: id::Counter,
    /// The default for how message IDs should be created (or not) when sending commands.
    default_id: Id,
    /// The default for whether commands should include a checksum or not.
    default_checksum: bool,
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
    /// # use zaber_protocol::ascii::Port;
    /// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut port = Port::open_serial("/dev/ttyUSB0")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn open_serial(path: &str) -> Result<Port<Serial>, AsciiError> {
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
    /// # use zaber_protocol::ascii::Port;
    /// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut port = Port::open_tcp("198.168.0.1:7770")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn open_tcp<A: ToSocketAddrs>(address: A) -> Result<Port<TcpStream>, io::Error> {
        OpenTcpOptions::new().open(address)
    }
}

#[cfg(test)]
impl Port<Mock> {
    /// Open a mock Port. Message Id and checksums are disabled by default for easier testing.
    pub fn open_mock() -> Port<Mock> {
        Port::from_backend(Mock::new(), Id::None, false)
    }
}

impl<B: Backend> Port<B> {
    /// The number of bytes to buffer when reading from the serial port.
    const BUFFER_SIZE: usize = 1024;

    /// Extract the bytes for a response from the specified buffer.
    /// Returns the number of bytes to consume, and either the slice containing
    /// the response or an error if a response could not be extracted.
    ///
    /// Regardless of whether the response bytes were successfully extracted,
    /// the specified number of bytes should *always* be consumed.
    fn extract_response_bytes(buf: &[u8]) -> (usize, Result<&[u8], AsciiProtocolError>) {
        use crate::ascii::parse::AsciiExt as _;

        let start = buf.iter().position(|b| b.is_packet_start());
        match start {
            Some(start) => {
                // We found the start of the packet
                let len = &buf[start + 1..]
                    .iter()
                    .position(|b| b.is_packet_end() || b.is_packet_start());
                match len {
                    Some(len) => {
                        let mut end = start + len + 1;
                        if buf[end].is_packet_end() {
                            if let Some(next) = buf.get(end + 1) {
                                if next.is_packet_end() {
                                    end += 1;
                                }
                            }
                            // We found the end of the packet. Everything is good.
                            // Return the buffer including the terminating byte(s).
                            (end + 1, Ok(&buf[start..end + 1]))
                        } else {
                            // We found the start of a new packet without the end of the first. Something is wrong.
                            (
                                end,
                                Err(AsciiPacketMissingEndError::new(&buf[start..end]).into()),
                            )
                        }
                    }
                    // We didn't find the end of this packet or the start of another one. Something is wrong.
                    None => (
                        buf.len(),
                        Err(AsciiPacketMissingEndError::new(&buf[start..]).into()),
                    ),
                }
            }
            // We couldn't find the start of a packet. Something is wrong.
            None => (
                buf.len(),
                Err(AsciiPacketMissingStartError::new(buf).into()),
            ),
        }
    }

    /// Create a `Port` from a [`Backend`] type.
    fn from_backend(backend: B, default_id: Id, default_checksum: bool) -> Self {
        Port {
            backend: BufReader::with_capacity(Self::BUFFER_SIZE, backend),
            ids: id::Counter::default(),
            default_id,
            default_checksum,
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

    /// Send a command. A reply is not read.
    ///
    /// On success, the generated message ID (if any) is returned.
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zaber_protocol::{ascii::{Command, Port}, backend::Backend};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// port.command(Command::empty())?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn command<C: AsRef<Command>>(&mut self, cmd: C) -> Result<Option<u8>, AsciiError> {
        self.check_poisoned()?;

        let mut buffer = Vec::new();
        let instance = cmd
            .as_ref()
            .instance(&mut self.ids, self.default_id, self.default_checksum);
        instance.write_into(&mut buffer)?;
        log::debug!(
            "{} TX: {}",
            self.backend
                .get_ref()
                .name()
                .unwrap_or_else(|| UNKNOWN_BACKEND_NAME.to_string()),
            String::from_utf8_lossy(buffer.as_slice()).trim_end()
        );
        self.backend.get_mut().write_all(buffer.as_slice())?;
        Ok(instance.id.into())
    }

    /// Transmit a command, receive a reply, and check it with the [`default`](check::default) checks.
    ///
    /// If the reply is split across multiple packets, the continuation messages will automatically be read.
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zaber_protocol::{ascii::Port, backend::Backend};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zaber_protocol::ascii::IntoCommand as _;
    ///
    /// let reply = port.command_reply("get maxspeed".to(1))?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn command_reply<C: AsRef<Command>>(&mut self, cmd: C) -> Result<Reply, AsciiError> {
        self.command_reply_with_check(cmd, check::default())
    }

    /// Same as [`Port::command_reply`] except that the reply is validated with the custom [`Check`](check::Check).
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zaber_protocol::{ascii::Port, backend::Backend};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zaber_protocol::ascii::{check::flag_ok, IntoCommand as _};
    ///
    /// let reply = port.command_reply_with_check("home".to(1), flag_ok())?;  // Home, but ignore any warning flags that might be present
    /// # Ok(())
    /// # }
    /// ```
    pub fn command_reply_with_check<C: AsRef<Command>>(
        &mut self,
        cmd: C,
        checker: impl check::Check<Reply>,
    ) -> Result<Reply, AsciiError> {
        self.check_poisoned()?;

        let cmd = cmd.as_ref();
        let id = self.command(cmd)?;
        let reply = self.response_with_check(checker)?;
        if !reply.target().elicited_by_command_to(cmd.target()) {
            Err(AsciiUnexpectedTargetError::new(reply).into())
        } else if reply.id() != id {
            Err(AsciiUnexpectedIdError::new(reply).into())
        } else {
            Ok(reply)
        }
    }

    /// Transmit a command and then receive a reply and all subsequent info messages.
    ///
    /// The reply and info messages are checked with the [`default`](check::default) checks.
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
    /// If any response other than the single reply and info messages elicited
    /// by the command are received, an [`AsciiUnexpected*`](AsciiUnexpectedError)
    /// is returned. If a reply to the final empty command is never received, an
    /// [`Error`] indicating a timeout is returned.
    ///
    /// ## Example
    /// ```rust
    /// # use zaber_protocol::{ascii::Port, backend::Backend, error::AsciiError};
    /// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), AsciiError> {
    /// use zaber_protocol::ascii::IntoCommand as _;
    ///
    /// let (reply, info_messages) = port.command_reply_infos("stream buffer 1 print".to(1))?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn command_reply_infos<C: AsRef<Command>>(
        &mut self,
        cmd: C,
    ) -> Result<(Reply, Vec<Info>), AsciiError> {
        self.command_reply_infos_with_check(cmd, check::default())
    }

    /// Same as [`Port::command_reply_infos`] except that the messages are validated with the custom [`Check`](check::Check).
    ///
    /// ## Example
    /// ```rust
    /// # use zaber_protocol::{
    /// #     ascii::{Port, AnyResponse},
    /// #     backend::Backend,
    /// #     error::{AsciiCheckError, AsciiError}
    /// # };
    /// # fn wrapper<B: Backend>(port: &mut Port<B>) -> Result<(), AsciiError> {
    /// use zaber_protocol::ascii::IntoCommand as _;
    ///
    /// let (reply, info_messages) = port.command_reply_infos_with_check(
    ///    "stream buffer 1 print".to(1),
    ///    |response| match response {
    ///        // Don't check replies or info messages, but error on alerts
    ///        AnyResponse::Reply(_) | AnyResponse::Info(_) => Ok(response),
    ///        AnyResponse::Alert(_) => Err(AsciiCheckError::custom("Alerts are not allowed!", response))
    ///    }
    /// )?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn command_reply_infos_with_check<C: AsRef<Command>>(
        &mut self,
        cmd: C,
        checker: impl check::Check<AnyResponse>,
    ) -> Result<(Reply, Vec<Info>), AsciiError> {
        #[inline]
        fn gen_new_checker<'a, 'b: 'a>(
            checker: &'b impl check::Check<AnyResponse>,
        ) -> impl check::Check<AnyResponse> + 'a {
            move |response| checker.check(response)
        }

        let cmd = cmd.as_ref();
        let target = cmd.target();
        let reply = self.command_reply(cmd)?;
        let id = self.command(Command::empty().to(target).id(Id::Generate))?;
        let mut infos = Vec::new();
        loop {
            match self.response_with_check(gen_new_checker(&checker))? {
                AnyResponse::Info(info) => {
                    if !info.target().elicited_by_command_to(target) {
                        return Err(AsciiUnexpectedTargetError::new(info).into());
                    } else if info.id() != reply.id() {
                        return Err(AsciiUnexpectedIdError::new(info).into());
                    } else {
                        infos.push(info);
                    }
                }
                AnyResponse::Reply(reply)
                    if reply.target().elicited_by_command_to(target) && reply.id() == id =>
                {
                    // This is the reply we've been waiting for. Stop.
                    break;
                }
                response => return Err(AsciiUnexpectedKindError::new(response).into()),
            }
        }
        Ok((reply, infos))
    }

    /// Transmit a command, receive n replies, and check each reply with the [`default`](check::default) checks.
    ///
    /// If any of the replies are split across multiple packets, the continuation messages will automatically be read.
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zaber_protocol::{ascii::Port, backend::Backend};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zaber_protocol::ascii::IntoCommand as _;
    ///
    /// let replies = port.command_reply_n("get system.serial".to_all(), 5)?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn command_reply_n<C: AsRef<Command>>(
        &mut self,
        cmd: C,
        n: usize,
    ) -> Result<Vec<Reply>, AsciiError> {
        self.command_reply_n_with_check(cmd, n, check::default())
    }

    /// Same as [`Port::command_reply_n`] except that the replies are validated with the custom [`Check`](check::Check).
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zaber_protocol::{ascii::Port, backend::Backend};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zaber_protocol::ascii::{check::unchecked, IntoCommand as _};
    ///
    /// let replies = port.command_reply_n_with_check("get system.serial".to_all(), 5, unchecked())?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn command_reply_n_with_check<C>(
        &mut self,
        cmd: C,
        n: usize,
        checker: impl check::Check<Reply>,
    ) -> Result<Vec<Reply>, AsciiError>
    where
        C: AsRef<Command>,
    {
        let cmd = cmd.as_ref();
        let id = self.command(cmd)?;
        let replies = self.response_n_with_check(n, checker)?;
        for reply in &replies {
            if !reply.target().elicited_by_command_to(cmd.target()) {
                return Err(AsciiUnexpectedTargetError::new(reply.clone()).into());
            } else if reply.id() != id {
                return Err(AsciiUnexpectedIdError::new(reply.clone()).into());
            }
        }
        Ok(replies)
    }

    /// Receive a response [`Packet`]
    ///
    /// The packet's LRC is verified and it is converted to the appropriate
    /// kind of response, but is otherwise unchecked.
    fn response_packet<R>(&mut self) -> Result<Packet<R>, AsciiError>
    where
        R: Response,
        AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
    {
        let backend_name = self
            .backend
            .get_ref()
            .name()
            .unwrap_or_else(|| UNKNOWN_BACKEND_NAME.to_string());

        let buf = self.backend.fill_buf()?;
        let (consumed, result) = Self::extract_response_bytes(buf);
        let result = result
            .map_err(From::from)
            // Log the packet
            .map(|packet| {
                log::debug!(
                    "{} RX: {}",
                    &backend_name,
                    String::from_utf8_lossy(packet).trim_end()
                );
                packet
            })
            // Parse the packet. Accept any response even if it isn't the one we're expecting.
            .and_then(|packet| Ok((packet, Packet::<AnyResponse>::try_from(packet)?)))
            // Verify the checksum, if one exists
            .and_then(|(raw_packet, packet)| {
                if let Some(checksum) = packet.response.checksum() {
                    if !Lrc::verify_packet(raw_packet, checksum) {
                        return Err(AsciiInvalidChecksumError::new(packet.response).into());
                    }
                }
                Ok(packet)
            })
            // Try to convert to the message kind we want.
            .and_then(|packet| {
                Ok(Packet {
                    complete: packet.complete,
                    response: R::try_from(packet.response)
                        .map_err(AsciiUnexpectedKindError::new)?,
                })
            });
        self.backend.consume(consumed);
        result
    }

    /// Receive a response and check it with the [`default`](check::default) checks.
    ///
    /// The type of response must be specified: [`Reply`], [`Info`], [`Alert`], or [`AnyResponse`].
    ///
    /// If the response is split across multiple packets, the continuation messages will automatically be read.
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zaber_protocol::{ascii::{Reply, Info, Port}, backend::Backend};
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
        self.response_with_check(check::default())
    }

    /// Same as [`Port::response`] except that the response is validated with the custom [`Check`](check::Check).
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zaber_protocol::{ascii::{Alert, Port}, backend::Backend};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zaber_protocol::ascii::check::warning_below_fault;
    /// let reply: Alert = port.response_with_check(warning_below_fault())?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn response_with_check<R>(&mut self, checker: impl check::Check<R>) -> Result<R, AsciiError>
    where
        R: Response,
        AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
        AsciiError: From<AsciiCheckError<R>>,
    {
        self.check_poisoned()?;

        let mut packet = self.response_packet::<R>()?;
        while !packet.complete {
            // Read all subsequent Info message packets until they are complete
            let continuation = self.response_packet::<Info>()?;
            if !continuation
                .response
                .target()
                .elicited_by_command_to(packet.response.target())
            {
                return Err(AsciiUnexpectedTargetError::new(continuation.response).into());
            } else if continuation.response.id() != packet.response.id() {
                return Err(AsciiUnexpectedIdError::new(continuation.response).into());
            } else {
                let cont_data = continuation.response.data();
                if let Some(rest) = cont_data.strip_prefix("cont ") {
                    let data = packet.response.data_mut();
                    if !data.is_empty() {
                        data.push(' ');
                    }
                    data.push_str(rest);
                    packet.complete = continuation.complete;
                } else {
                    return Err(AsciiUnexpectedContinuationError::new(continuation.response).into());
                }
            }
        }
        // Check the contents of the message
        checker.check(packet.response).map_err(Into::into)
    }

    /// Receive `n` responses and check each one with the [`default`](check::default) checks.
    ///
    /// The type of response must be specified: [`Reply`], [`Info`], [`Alert`], or [`AnyResponse`].
    ///
    /// If the response is split across multiple packets, the continuation messages will automatically be read.
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zaber_protocol::{ascii::{Info, Port}, backend::Backend};
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
        self.response_n_with_check(n, check::default())
    }

    /// Same as [`Port::response_n`] except that the responses are validated with the custom [`Check`](check::Check).
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zaber_protocol::{ascii::{Reply, Port}, backend::Backend};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zaber_protocol::ascii::check::{flag_ok_and, warning_below_fault};
    /// let reply: Vec<Reply> = port.response_n_with_check(3, flag_ok_and(warning_below_fault()))?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn response_n_with_check<R>(
        &mut self,
        n: usize,
        checker: impl check::Check<R>,
    ) -> Result<Vec<R>, AsciiError>
    where
        R: Response,
        AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
        AsciiError: From<AsciiCheckError<R>>,
    {
        #[inline]
        fn gen_new_checker<'a, 'b: 'a, R: Response>(
            checker: &'b impl check::Check<R>,
        ) -> impl check::Check<R> + 'a {
            move |response| checker.check(response)
        }
        let mut responses = Vec::new();
        for _ in 0..n {
            responses.push(self.response_with_check(gen_new_checker(&checker))?);
        }
        Ok(responses)
    }

    /// Receive responses until the port times out and check each one with the [`default`](check::default) checks.
    ///
    /// The type of response must be specified: [`Reply`], [`Info`], [`Alert`], or [`AnyResponse`].
    ///
    /// If any of the responses are split across multiple packets, the continuation messages will automatically be read.
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zaber_protocol::ascii::{AnyResponse, Port};
    /// # use zaber_protocol::backend::Backend;
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
        self.responses_until_timeout_with_check(check::default())
    }

    /// Same as [`Port::responses_until_timeout`] except that the responses are validated with the custom [`Check`](check::Check).
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use zaber_protocol::ascii::{AnyResponse, Port};
    /// # use zaber_protocol::backend::Backend;
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zaber_protocol::ascii::check::unchecked;
    /// let reply: Vec<AnyResponse> = port.responses_until_timeout_with_check(unchecked())?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn responses_until_timeout_with_check<R>(
        &mut self,
        check: impl check::Check<R>,
    ) -> Result<Vec<R>, AsciiError>
    where
        R: Response,
        AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
        AsciiError: From<AsciiCheckError<R>>,
    {
        #[inline]
        fn gen_new_checker<'a, 'b: 'a, R: Response>(
            checker: &'b impl check::Check<R>,
        ) -> impl check::Check<R> + 'a {
            move |response| checker.check(response)
        }
        let mut responses = Vec::new();
        loop {
            match self.response_with_check(gen_new_checker(&check)) {
                Ok(r) => responses.push(r),
                Err(e) if e.is_timeout() => break,
                Err(e) => return Err(e),
            }
        }
        Ok(responses)
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
    /// # use zaber_protocol::{ascii::Port, backend::Backend};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// use zaber_protocol::ascii::IntoCommand as _;
    ///
    /// port.poll_until("".to((1,1)), |reply| { reply.warning() != "FZ" })?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn poll_until<C, F>(&mut self, cmd: C, mut predicate: F) -> Result<Reply, AsciiError>
    where
        C: AsRef<Command>,
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
    /// # use zaber_protocol::{ascii::Port, backend::Backend};
    /// # fn wrapper<B: Backend>(mut port: Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    /// port.poll_until_idle((1,1))?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn poll_until_idle<T: Into<Target>>(&mut self, target: T) -> Result<Reply, AsciiError> {
        self.poll_until(Command::empty().to(target.into()), |reply| {
            reply.status() == Status::Idle
        })
    }

    /// Set the port timeout and return a "scope guard" that will reset the timeout when it goes out of scope.
    ///
    /// While the guard is in scope, the port can only be accessed through the guard.
    /// However, because the guard implements [`Deref`](std::ops::Deref) and [`DerefMut`](std::ops::DerefMut) callers can treat the guard as the port.
    ///
    /// ## Example
    /// ```rust
    /// # use zaber_protocol::{error::AsciiError, ascii::{Port, Reply, IntoCommand as _}, backend::Backend};
    /// # use std::time::Duration;
    /// # fn helper<B: Backend>(mut port: Port<B>) -> Result<Reply, AsciiError> {
    /// {
    ///     let mut guard = port.timeout_guard(Some(Duration::from_secs(3)))?;
    ///     // All commands within this scope will use a 3 second timeout
    ///     guard.command_reply("system reset".to(1));
    ///
    /// }  // The guard is dropped and the timeout is reset.
    ///
    /// // This command-reply uses the original timeout
    /// port.command_reply("get device.id".to(1))
    /// # }
    /// ```
    pub fn timeout_guard(
        &mut self,
        timeout: Option<Duration>,
    ) -> Result<TimeoutGuard<B, Self>, io::Error> {
        self.check_poisoned()?;

        TimeoutGuard::new(self, timeout)
    }

    /// Set whether commands sent on this port should include a checksum by default or not.
    ///
    /// This default is ignored for any command that explicitly overrides it.
    pub fn default_checksum(&mut self, value: bool) -> &mut Self {
        self.default_checksum = value;
        self
    }

    /// Set whether commands sent on this port should include an automatically generated message ID by default or not.
    ///
    /// This default is ignored for any command that explicitly overrides it.
    pub fn default_id(&mut self, value: bool) -> &mut Self {
        self.default_id = if value { Id::Generate } else { Id::None };
        self
    }
}

impl<B: Backend> io::Write for Port<B> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.check_poisoned()?;
        self.backend.get_mut().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.check_poisoned()?;
        self.backend.get_mut().flush()
    }
}

impl<B: Backend> io::Read for Port<B> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.check_poisoned()?;
        self.backend.read(buf)
    }
}

impl<B: Backend> crate::timeout_guard::Port<B> for Port<B> {
    fn backend_mut(&mut self) -> &mut B {
        self.backend.get_mut()
    }
    fn poison(&mut self, e: io::Error) {
        self.poison = Some(e)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ascii::{check::unchecked, AnyResponse, IntoCommand as _, Port},
        backend::{Mock, Serial},
        error::*,
    };

    #[test]
    fn extract_response_bytes() {
        struct Case<'a> {
            input: &'a [u8],
            expected: (usize, Result<&'a [u8], AsciiProtocolError>),
        }
        let test_cases: &[Case] = &[
            Case {
                input: b"/\r\n",
                expected: (3, Ok(b"/\r\n")),
            },
            Case {
                input: b"/\n",
                expected: (2, Ok(b"/\n")),
            },
            Case {
                input: b"/\r",
                expected: (2, Ok(b"/\r")),
            },
            Case {
                input: b"!\r\n",
                expected: (3, Ok(b"!\r\n")),
            },
            Case {
                input: b"#\r\n",
                expected: (3, Ok(b"#\r\n")),
            },
            Case {
                input: b"@\r\n",
                expected: (3, Ok(b"@\r\n")),
            },
            Case {
                input: b"\0\t\r@01 1 OK IDLE --\r\n",
                expected: (21, Ok(b"@01 1 OK IDLE --\r\n")),
            },
            Case {
                input: b"  /1 1 tools echo / this\n",
                expected: (
                    18,
                    Err(AsciiPacketMissingEndError::new(b"/1 1 tools echo ").into()),
                ),
            },
            Case {
                input: b"\r\n",
                expected: (2, Err(AsciiPacketMissingStartError::new(b"\r\n").into())),
            },
            Case {
                input: b"/anything here",
                expected: (
                    14,
                    Err(AsciiPacketMissingEndError::new(b"/anything here").into()),
                ),
            },
        ];

        for (i, case) in test_cases.into_iter().enumerate() {
            println!("Case {}", i);
            let actual = Port::<Serial>::extract_response_bytes(case.input);

            assert_eq!(
                actual,
                case.expected,
                "Case {}: {}",
                i,
                std::str::from_utf8(case.input).unwrap()
            );
        }
    }

    // Poison a port
    fn poison_port(port: &mut Port<Mock>) {
        use std::{io, time::Duration};
        let mut guard = port.timeout_guard(Some(Duration::from_secs(1))).unwrap();
        guard
            .backend
            .get_mut()
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

    make_poison_test!(command, "".to_all());
    make_poison_test!(command_reply, "".to_all());
    make_poison_test!(command_reply_infos, "".to_all());
    make_poison_test!(command_reply_infos_with_check, "".to_all(), unchecked());
    make_poison_test!(command_reply_n, "".to_all(), 1);
    make_poison_test!(command_reply_n_with_check, "".to_all(), 1, unchecked());
    make_poison_test!(command_reply_with_check, "".to_all(), unchecked());
    make_poison_test!(poll_until, "".to_all(), |_| true);
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
