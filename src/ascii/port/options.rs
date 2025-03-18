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

const DEFAULT_TIMEOUT: Duration = Duration::from_secs(3);
const DEFAULT_GENERATE_ID: bool = true;
const DEFAULT_GENERATE_CHECKSUM: bool = true;

/// Defines an `Open*Options` type with the common members and all other specified members.
///
/// It is expected that each type has a `new()` method.
macro_rules! define_option {
	(
		$(#[$attr:meta])*
		pub struct $name:ident {
			$(
				$member:ident: $type:ty,
			)*
			...
		}
	) => {
		$(#[$attr])*
		#[derive(Debug)]
		pub struct $name {
			$(
				$member: $type,
			)*
			/// Whether commands should include message IDs or not.
			generate_id: bool,
			/// Whether commands should include a checksum or not.
			generate_checksum: bool,
			/// The maximum command packet size.
			max_packet_size: MaxPacketSize,
		}

		$(#[$attr])*
		impl Default for $name {
			fn default() -> Self {
				Self::new()
			}
		}
	};
}

/// Create implementations for the common builder methods for open
macro_rules! impl_builder_methods {
	($($token:ident),+) => {
		$(
			impl_builder_methods!{@ $token}
		)+
	};
	(@ timeout) => {
		/// Set a custom read timeout.
		///
		/// If duration is `None`, reads will block indefinitely.
		pub fn timeout(&mut self, duration: Option<Duration>) -> &mut Self {
			self.timeout = duration;
			self
		}
	};
	(@ baud_rate) => {
		/// Set a custom baud rate.
		///
		/// The default is 115,200.
		pub fn baud_rate(&mut self, baud_rate: u32) -> &mut Self {
			self.baud_rate = baud_rate;
			self
		}
	};
	(@ common) => {
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
	};
}

define_option! {
	/// Options for configuring and opening a serial port.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::ascii::port::OpenSerialOptions;
	/// # use std::time::Duration;
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// let mut port = OpenSerialOptions::new()
	///     .timeout(Some(Duration::from_millis(50)))
	///     .open("/dev/ttyUSB0")?;
	/// # Ok(())
	/// # }
	/// ```
	pub struct OpenSerialOptions {
		timeout: Option<Duration>,
		baud_rate: u32,
		...
	}
}

impl OpenSerialOptions {
	/// The default baud rate for the ASCII protocol: 115,200.
	pub const DEFAULT_BAUD_RATE: u32 = 115_200;

	/// Create a blank set of options ready for configuration.
	///
	/// By default the read timeout is 3 seconds, the baud rate is 115,200, message IDs and checksums are enabled, and the max
	/// packet size is [`MaxPacketSize::default`].
	///
	/// Equivalent to [`default`](OpenSerialOptions::default).
	pub fn new() -> Self {
		Self {
			generate_id: DEFAULT_GENERATE_ID,
			generate_checksum: DEFAULT_GENERATE_CHECKSUM,
			max_packet_size: MaxPacketSize::default(),
			timeout: Some(DEFAULT_TIMEOUT),
			baud_rate: Self::DEFAULT_BAUD_RATE,
		}
	}

	/// Open a [`Serial`] port configured for the ASCII protocol at the specified path.
	fn open_serial_port(&self, path: &str) -> Result<Serial, AsciiError> {
		// Due to https://github.com/serialport/serialport-rs/issues/20, the
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
			.baud_rate(self.baud_rate)
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
	pub fn open_dyn<'a>(
		&self,
		path: &str,
	) -> Result<Port<'a, Box<dyn Backend + Send>>, AsciiError> {
		Ok(self.with_backend(Box::new(self.open_serial_port(path)?)))
	}

	impl_builder_methods! {common, baud_rate, timeout}
}

define_option! {
	/// Options for configuring and opening a TCP port.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::ascii::port::OpenTcpOptions;
	/// # use std::time::Duration;
	/// # fn wrapper() -> Result<(), Box<dyn std::error::Error>> {
	/// let mut port = OpenTcpOptions::new()
	///     .timeout(Some(Duration::from_millis(50)))
	///     .open("192.168.0.1:55550")?;
	/// # Ok(())
	/// # }
	/// ```
	pub struct OpenTcpOptions {
		timeout: Option<Duration>,
		...
	}
}

impl OpenTcpOptions {
	/// Create a blank set of options ready for configuration.
	///
	/// By default the read timeout is 3 seconds, message IDs and checksums are enabled, and the max
	/// packet size is [`MaxPacketSize::default`].
	///
	/// Equivalent to [`default`](OpenTcpOptions::default).
	pub fn new() -> Self {
		Self {
			generate_id: DEFAULT_GENERATE_ID,
			generate_checksum: DEFAULT_GENERATE_CHECKSUM,
			max_packet_size: MaxPacketSize::default(),
			timeout: Some(DEFAULT_TIMEOUT),
		}
	}

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
	) -> io::Result<Port<'a, Box<dyn Backend + Send>>> {
		Ok(self.with_backend(Box::new(self.open_tcp_stream(address)?)))
	}

	impl_builder_methods! {common, timeout}
}

define_option! {
	/// Options for configuring and opening a port with any [`Backend`].
	///
	/// When the backend is either [`Serial`] or [`TcpStream`], it is better to use the options
	/// specific to them: [`OpenSerialOptions`] or [`OpenTcpOptions`], respectively. They provide a
	/// more ergonomic/correct way of opening the port.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::ascii::port::OpenGeneralOptions;
	/// # use zproto::backend::Backend;
	/// # fn wrapper(my_backend: impl Backend) -> Result<(), Box<dyn std::error::Error>> {
	/// let mut port = OpenGeneralOptions::new().message_ids(false).open(my_backend);
	/// # Ok(())
	/// # }
	/// ```
	pub struct OpenGeneralOptions {
		...
	}
}

impl OpenGeneralOptions {
	/// Create a blank set of options ready for configuration.
	///
	/// By message IDs and checksums are enabled, and the max packet size is [`MaxPacketSize::default`].
	///
	/// Equivalent to [`default`](OpenGeneralOptions::default).
	pub fn new() -> Self {
		Self {
			generate_id: DEFAULT_GENERATE_ID,
			generate_checksum: DEFAULT_GENERATE_CHECKSUM,
			max_packet_size: MaxPacketSize::default(),
		}
	}

	/// Open a [`Port`] using the specified `backend`.
	pub fn open<'a, B: Backend>(&self, backend: B) -> Port<'a, B> {
		self.with_backend(backend)
	}

	impl_builder_methods! {common}
}

define_option! {
	/// Options for configuring and opening a port with a [`Mock`] backend.
	///
	/// ## Example
	///
	/// ```
	/// # #[cfg(feature = "mock")] // Only test the code if the "mock" feature is enabled.
	/// # {
	/// # use zproto::ascii::port::OpenMockOptions;
	/// # use zproto::backend::Mock;
	/// let mut port = OpenMockOptions::new().message_ids(true).open();
	/// # }
	/// ```
	#[cfg(any(test, doc, feature = "mock"))]
	#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "mock")))]
	pub struct OpenMockOptions {
		...
	}
}

#[cfg(any(test, doc, feature = "mock"))]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "mock")))]
impl OpenMockOptions {
	/// Create a blank set of options ready for configuration.
	///
	/// By default the max packet size is [`MaxPacketSize::default`], but
	/// message IDs and checksums are disabled.
	///
	/// Equivalent to [`default`](OpenMockOptions::default).
	pub fn new() -> Self {
		Self {
			generate_id: false,
			generate_checksum: false,
			max_packet_size: MaxPacketSize::default(),
		}
	}

	/// Open a [`Port`] using the [`Mock`] backend.
	pub fn open<'a>(&self) -> Port<'a, Mock> {
		self.with_backend(Mock::new())
	}

	impl_builder_methods! {common}
}
