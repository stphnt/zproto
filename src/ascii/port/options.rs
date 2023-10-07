//! Types defining the different options when opening a port.

use super::Port;
use crate::{
    ascii::MaxPacketSize,
    backend::{Backend, Serial},
    error::AsciiError,
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
    /// The maximum command packet size.
    max_packet_size: MaxPacketSize,
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
            max_packet_size: MaxPacketSize::default(),
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

    /// Set the maximum command packet size.
    ///
    /// The default is [`MaxPacketSize::default`].
    pub fn max_packet_size(&mut self, max_packet_size: MaxPacketSize) -> &mut Self {
        self.max_packet_size = max_packet_size;
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
            self.max_packet_size,
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
            self.max_packet_size,
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
    /// The maximum command packet size.
    max_packet_size: MaxPacketSize,
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
            max_packet_size: MaxPacketSize::default(),
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

    /// Set the maximum command packet size.
    ///
    /// The default is [`MaxPacketSize::default`].
    pub fn max_packet_size(&mut self, max_packet_size: MaxPacketSize) -> &mut Self {
        self.max_packet_size = max_packet_size;
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
            self.max_packet_size,
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
            self.max_packet_size,
        ))
    }
}

impl Default for OpenTcpOptions {
    fn default() -> Self {
        OpenTcpOptions::new()
    }
}
