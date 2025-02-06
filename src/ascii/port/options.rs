//! Types defining the different options when opening a port.

use super::Port;
#[cfg(any(test, doc, feature = "mock"))]
use crate::backend::Mock;
use crate::{
	ascii::command::MaxPacketSize,
	backend::{Backend, Serial},
	error::AsciiError,
};
use serialport as sp;
use std::{
	io,
	net::{TcpStream, ToSocketAddrs},
	time::Duration,
};

/// Options for configuring and opening a [`Port`].
///
/// There are a few flavors:
///
/// 1. [`OpenGeneralOptions`] or `OpenOptions<General>`: Opens a [`Port`] with any [`Backend`].
///
/// ```
/// # use zproto::ascii::port::OpenGeneralOptions;
/// # use zproto::backend::Backend;
/// # fn wrapper<B: Backend>(my_backend: B) {
/// let mut port = OpenGeneralOptions::new().open(my_backend);
/// # }
/// ```
///
/// 2. [`OpenSerialOptions`] or `OpenOptions<Serial>`: Opens a [`Port`] over a serial
///    connection.
/// ```
/// # use zproto::ascii::port::OpenSerialOptions;
/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
/// let mut port = OpenSerialOptions::new().open("/dev/ttyUSB0")?;
/// # Ok(())
/// # }
/// ```
///
/// 3. [`OpenTcpOptions`] or `OpenOptions<TcpStream>`: Opens a [`Port`] over a TCP connection.
///
/// ```
/// # use zproto::ascii::port::OpenTcpOptions;
/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
/// let mut port = OpenTcpOptions::new().open("192.168.0.1:55550")?;
/// # Ok(())
/// # }
/// ```
///
/// 4. [`OpenMockOptions`] or `OpenOptions<Mock>`: Opens a [`Port`] with a [`Mock`] backend.
///
/// ```
/// # #[cfg(feature="mock")] // Only test the code if the "mock" feature is enabled
/// # {
/// #
/// # use zproto::ascii::port::OpenMockOptions;
/// let mut port = OpenMockOptions::new().open();
/// #
/// # }
/// ```
///
#[derive(Debug)]
pub struct OpenOptions<T> {
	/// The custom timeout
	timeout: Option<Duration>,
	/// Whether commands should include message IDs or not.
	generate_id: bool,
	/// Whether commands should include a checksum or not.
	generate_checksum: bool,
	/// The maximum command packet size.
	max_packet_size: MaxPacketSize,
	/// Other parameters used to open a particular backend.
	extra: T,
}

impl<T> OpenOptions<T> {
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

	/// Open a [`Port`] with the configured options and the specified `backend`.
	fn with_backend<'a, B: Backend>(&self, backend: B) -> Port<'a, B> {
		Port::from_backend(
			backend,
			self.generate_id,
			self.generate_checksum,
			self.max_packet_size,
		)
	}
}

/// Implement the standard `OpenOptions::new` method.
macro_rules! impl_new {
	($T:ty) => {
		/// Create a blank set of options ready for configuration.
		///
		/// By default the read timeout is 3 seconds, message IDs and checksums are enabled, and the max
		/// packet size is [`MaxPacketSize::default`].
		///
		/// Equivalent to [`default`](OpenOptions::default).
		pub fn new() -> Self {
			Self {
				timeout: Some(Duration::from_secs(3)),
				generate_id: true,
				generate_checksum: true,
				max_packet_size: MaxPacketSize::default(),
				extra: <$T>::default(),
			}
		}
	};
}

impl OpenOptions<open_data::Serial> {
	/// The default baud rate for the ASCII protocol: 115,200.
	pub const DEFAULT_BAUD_RATE: u32 = 115_200;

	impl_new!(open_data::Serial);

	/// Set a custom baud rate.
	///
	/// The default is 115,200.
	pub fn baud_rate(&mut self, baud_rate: u32) -> &mut Self {
		self.extra.baud_rate = baud_rate;
		self
	}

	/// Open a [`Serial`] port configured for the ASCII protocol at the specified path.
	fn open_serial_port(&self, path: &str) -> Result<Serial, AsciiError> {
		// Due to https://gitlab.com/susurrus/serialport-rs/-/issues/102, the
		// baud rate passed to new is ignored. It must be defined using the
		// baud_rate method below. Use the default baud_rate as it should be a
		// valid baud rate.
		sp::new(path, Self::DEFAULT_BAUD_RATE)
			.data_bits(sp::DataBits::Eight)
			.parity(sp::Parity::None)
			.flow_control(sp::FlowControl::None)
			.stop_bits(sp::StopBits::One)
			// The serialport API does not support infinite timeouts, so simply
			// set the timeout to the largest possible duration if `timeout` is
			// `None`, which is practically infinite.
			.timeout(self.timeout.unwrap_or(Duration::MAX))
			.baud_rate(self.extra.baud_rate)
			.open_native()
			.map(Serial)
			.map_err(Into::into)
	}

	/// Open the serial port at the specified path with the custom options.
	pub fn open<'a>(&self, path: &str) -> Result<Port<'a, Serial>, AsciiError> {
		Ok(self.with_backend(self.open_serial_port(path)?))
	}

	/// Open the serial port at the specified path with the custom options.
	///
	/// The type of the underlying backend is erased via dynamic dispatch,
	/// which does have runtime overhead. [`OpenSerialOptions::open`] should
	/// generally be used instead, except when the type of the underlying
	/// backend may not be known at compile time.
	pub fn open_dyn<'a>(&self, path: &str) -> Result<Port<'a, Box<dyn Backend>>, AsciiError> {
		Ok(self.with_backend(Box::new(self.open_serial_port(path)?)))
	}
}

impl OpenOptions<open_data::TcpStream> {
	impl_new!(open_data::TcpStream);

	/// Open a [`TcpStream`] configured for the ASCII protocol at the specified address.
	fn open_tcp_stream<A: ToSocketAddrs>(&self, address: A) -> io::Result<TcpStream> {
		let stream = TcpStream::connect(address)?;
		stream.set_read_timeout(self.timeout)?;
		Ok(stream)
	}

	/// Open the TCP port at the specified path with the custom options.
	pub fn open<'a, A: ToSocketAddrs>(&self, address: A) -> io::Result<Port<'a, TcpStream>> {
		Ok(self.with_backend(self.open_tcp_stream(address)?))
	}

	/// Open the TCP port at the specified path with the custom options.
	///
	/// The type of the underlying backend is erased via dynamic dispatch,
	/// which does have runtime overhead. [`OpenTcpOptions::open`] should
	/// generally be used instead, except when the type of the underlying
	/// backend may not be known at compile time.
	pub fn open_dyn<'a, A: ToSocketAddrs>(
		&self,
		address: A,
	) -> io::Result<Port<'a, Box<dyn Backend>>> {
		Ok(self.with_backend(Box::new(self.open_tcp_stream(address)?)))
	}
}

impl OpenOptions<open_data::General> {
	impl_new!(open_data::General);

	/// Open a [`Port`] using the specified `backend`.
	pub fn open<'a, B: Backend>(&self, backend: B) -> Port<'a, B> {
		self.with_backend(backend)
	}
}

#[cfg(any(test, doc, feature = "mock"))]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "mock")))]
impl OpenOptions<open_data::Mock> {
	/// Create a blank set of options ready for configuration.
	///
	/// By default the read timeout is 3 seconds and the max packet size is
	/// [`MaxPacketSize::default`], but message IDs and checksums are disabled.
	///
	/// Equivalent to [`default`](OpenOptions::default).
	pub fn new() -> Self {
		Self {
			timeout: Some(Duration::from_secs(3)),
			generate_id: false,
			generate_checksum: false,
			max_packet_size: MaxPacketSize::default(),
			extra: open_data::Mock,
		}
	}

	/// Open a [`Port`] using the specified `backend`.
	pub fn open<'a>(&self) -> Port<'a, Mock> {
		self.with_backend(Mock::new())
	}
}

impl Default for OpenOptions<open_data::Serial> {
	fn default() -> Self {
		OpenOptions::<open_data::Serial>::new()
	}
}

impl Default for OpenOptions<open_data::TcpStream> {
	fn default() -> Self {
		OpenOptions::<open_data::TcpStream>::new()
	}
}

impl Default for OpenOptions<open_data::General> {
	fn default() -> Self {
		OpenOptions::<open_data::General>::new()
	}
}

#[cfg(any(test, doc, feature = "mock"))]
impl Default for OpenOptions<open_data::Mock> {
	fn default() -> Self {
		OpenOptions::<open_data::Mock>::new()
	}
}

/// Types defining data required to open different backends.
///
/// The types are named after the Backend they are used for opening, where possible.
///
/// The module is intentionally private.
mod open_data {
	/// Extra data needed to open a [`crate::backend::Serial`] backend.
	#[derive(Debug)]
	pub struct Serial {
		pub(super) baud_rate: u32,
	}

	impl Default for Serial {
		fn default() -> Self {
			Self {
				baud_rate: super::OpenOptions::<Serial>::DEFAULT_BAUD_RATE,
			}
		}
	}

	/// Extra data a [`std::net::TcpStream`] backend.
	#[derive(Debug, Default)]
	pub struct TcpStream;

	/// Extra data needed to open a general backend.
	#[derive(Debug, Default)]
	pub struct General;

	/// Extra data needed to open a mock backend.
	#[derive(Debug, Default)]
	pub struct Mock;
}

/// Options for configuring and opening a serial port.
///
/// ## Example
///
/// ```rust
/// # use zproto::ascii::port::OpenSerialOptions;
/// # use std::time::Duration;
/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
/// let mut port = OpenSerialOptions::new()
///     .timeout(Some(Duration::from_millis(50)))
///     .open("/dev/ttyUSB0")?;
/// # Ok(())
/// # }
/// ```
pub type OpenSerialOptions = OpenOptions<open_data::Serial>;

/// Options for configuring and opening a TCP port.
///
/// ## Example
///
/// ```rust
/// # use zproto::ascii::port::OpenTcpOptions;
/// # use std::time::Duration;
/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
/// let mut port = OpenTcpOptions::new()
///     .timeout(Some(Duration::from_millis(50)))
///     .open("192.168.0.1:55550")?;
/// # Ok(())
/// # }
/// ```
pub type OpenTcpOptions = OpenOptions<open_data::TcpStream>;

/// Options for configuring and opening a port with any [`Backend`].
///
/// When the backend is either [`Serial`] or [`TcpStream`], it is better to use the options
/// specific to them: [`OpenSerialOptions`] or [`OpenTcpOptions`], respectively. They provide a
/// more ergonomic/correct way of opening the port.
///
/// ## Example
///
/// ```rust
/// # use zproto::ascii::port::OpenGeneralOptions;
/// # use zproto::backend::Backend;
/// # use std::time::Duration;
/// # fn wrapper(my_backend: impl Backend) -> Result<(), Box<dyn std::error::Error>> {
/// let mut port = OpenGeneralOptions::new()
///     .timeout(Some(Duration::from_millis(50)))
///     .open(my_backend);
/// # Ok(())
/// # }
/// ```
pub type OpenGeneralOptions = OpenOptions<open_data::General>;

/// Options for configuring and opening a port with a [`Mock`] backend.
///
/// ## Example
///
/// ```
/// # #[cfg(feature = "mock")] // Only test the code if the "mock" feature is enabled.
/// # {
/// #
/// # use zproto::ascii::port::OpenMockOptions;
/// # use zproto::backend::Mock;
/// # use std::time::Duration;
/// let mut port = OpenMockOptions::new()
///     .timeout(Some(Duration::from_millis(50)))
///     .open();
/// #
/// # }
/// ```
#[cfg(any(test, doc, feature = "mock"))]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "mock")))]
pub type OpenMockOptions = OpenOptions<open_data::Mock>;
