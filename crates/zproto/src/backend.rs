//! Types that can exchange (read/write) bytes with a connected device.
//!
//! The [`Backend`] trait represents all such types.

use std::io;
use std::time::Duration;

use serialport as sp;

#[cfg(windows)]
use sp::COMPort as ExternSerial;
use sp::SerialPort;
#[cfg(unix)]
use sp::TTYPort as ExternSerial;

/// The placeholder name for a backend that doesn't have a name.
pub(crate) const UNKNOWN_BACKEND_NAME: &str = "<unknown backend>";

/// Types that allow reading and writing bytes with a connected device.
pub trait Backend: io::Read + io::Write + private::Sealed {
	/// Set the read timeout.
	///
	/// If timeout is `None`, reads will block indefinitely.
	fn set_read_timeout(&mut self, timeout: Option<Duration>) -> Result<(), io::Error>;

	/// Get the read timeout.
	///
	/// If timeout is `None`, reads will block indefinitely.
	fn read_timeout(&self) -> Result<Option<Duration>, io::Error>;

	/// Get the "name" of the backend.
	///
	/// This can be in any format, but should uniquely identify the backend
	/// instance.
	fn name(&self) -> Option<String>;
}

impl<C: Backend + ?Sized> Backend for Box<C> {
	fn set_read_timeout(&mut self, timeout: Option<Duration>) -> Result<(), io::Error> {
		(**self).set_read_timeout(timeout)
	}
	fn read_timeout(&self) -> Result<Option<Duration>, io::Error> {
		(**self).read_timeout()
	}
	fn name(&self) -> Option<String> {
		(**self).name()
	}
}

impl<C: Backend + ?Sized> Backend for &mut C {
	fn set_read_timeout(&mut self, timeout: Option<Duration>) -> Result<(), io::Error> {
		(**self).set_read_timeout(timeout)
	}
	fn read_timeout(&self) -> Result<Option<Duration>, io::Error> {
		(**self).read_timeout()
	}
	fn name(&self) -> Option<String> {
		(**self).name()
	}
}

impl Backend for std::net::TcpStream {
	fn set_read_timeout(&mut self, timeout: Option<Duration>) -> Result<(), io::Error> {
		std::net::TcpStream::set_read_timeout(self, timeout)
	}
	fn read_timeout(&self) -> Result<Option<Duration>, io::Error> {
		std::net::TcpStream::read_timeout(self)
	}
	fn name(&self) -> Option<String> {
		self.local_addr().map(|addr| format!("{addr}")).ok()
	}
}

/// A platform agnostic serial port backend.
//
// The `serialport` crate exposes two platform specific serial ports, `COMPort`
// and `TTYPort` for windows and unix, respectively. These can be used directly,
// but in order for code to be platform agnostic it must either rely on dynamic
// dispatch or be generic over the serial port. Dynamic dispatch brings
// unnecessary runtime overhead and generics add an unnecessary type parameter
// -- in both cases a platform will only ever use one of the types. We get
// around that here by wrapping either a `COMPort` or a `TTYPort`, which is
// decided at compile time, in a new type [`Serial`]. This way all platform
// specific types are abstracted away and all consumers can simply use the
// [`Serial`].
#[derive(Debug)]
pub struct Serial(pub(crate) ExternSerial);

impl io::Read for Serial {
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		self.0.read(buf)
	}
}

impl io::Write for Serial {
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		self.0.write(buf)
	}

	fn flush(&mut self) -> io::Result<()> {
		self.0.flush()
	}
}

impl Backend for Serial {
	fn set_read_timeout(&mut self, timeout: Option<Duration>) -> Result<(), io::Error> {
		// The serialport API does not support infinite timeouts, so simply set
		// the timeout to the largest possible duration if `timeout` is `None`,
		// which is practically infinite.
		Ok(self.0.set_timeout(timeout.unwrap_or(Duration::MAX))?)
	}
	fn read_timeout(&self) -> Result<Option<Duration>, io::Error> {
		Ok(Some(self.0.timeout()))
	}
	fn name(&self) -> Option<String> {
		self.0.name()
	}
}

/// A mock backend for use in testing.
///
/// It has the following features:
///   * It ignores all data written to it.
///   * It can be filled with data for reading.
///   * Specific errors can be inserted for calls to `read`, `write`, `flush`,
///     and `set_read_timeout`.
#[cfg(test)]
#[derive(Debug)]
pub struct Mock {
	/// The buffer data is read from
	buffer: io::Cursor<Vec<u8>>,
	/// The error to surface on the next read, if any. It is only surfaced once.
	read_error: Option<io::Error>,
	/// The error to surface on the next write, if any. It is only surfaced once.
	write_error: Option<io::Error>,
	/// The error to surface on the next flush, if any. It is only surfaced once.
	flush_error: Option<io::Error>,
	/// The error to surface on the next set_read_timeout, if any. It is only surfaced once.
	set_read_timeout_error: Option<io::Error>,
	/// The read timeout, which is ignored.
	ignored_read_timeout: Option<Duration>,
}

#[cfg(test)]
impl Mock {
	/// Create a new Mock backend.
	pub fn new() -> Self {
		Mock {
			buffer: io::Cursor::new(Vec::new()),
			read_error: None,
			write_error: None,
			flush_error: None,
			set_read_timeout_error: None,
			ignored_read_timeout: Some(Duration::ZERO),
		}
	}
	/// Append data to the read buffer.
	///
	/// The data is not validated in any way.
	pub fn append_data<T: AsRef<[u8]>>(&mut self, bytes: T) {
		self.buffer.get_mut().extend_from_slice(bytes.as_ref());
	}
	/// Clear the read buffer.
	pub fn clear_buffer(&mut self) {
		self.buffer.get_mut().clear();
		self.buffer.set_position(0);
	}
	/// Whether the mock has any data available or not
	pub fn is_empty(&self) -> bool {
		self.buffer.position() as usize >= self.buffer.get_ref().len()
	}
	/// Set the error for the next `read`, if any.
	pub fn read_error(&mut self, err: Option<io::Error>) {
		self.read_error = err;
	}
	/// Set the error for the next `write`, if any.
	pub fn write_error(&mut self, err: Option<io::Error>) {
		self.write_error = err;
	}
	/// Set the error for the next `flush`, if any.
	pub fn flush_error(&mut self, err: Option<io::Error>) {
		self.flush_error = err;
	}
	/// Set the error for the next `set_read_timeout`, if any.
	pub fn set_read_timeout_error(&mut self, err: Option<io::Error>) {
		self.set_read_timeout_error = err;
	}
}

#[cfg(test)]
impl Default for Mock {
	fn default() -> Self {
		Self::new()
	}
}

#[cfg(test)]
impl Backend for Mock {
	fn set_read_timeout(&mut self, timeout: Option<Duration>) -> Result<(), io::Error> {
		if let Some(err) = self.set_read_timeout_error.take() {
			Err(err)
		} else {
			self.ignored_read_timeout = timeout;
			Ok(())
		}
	}

	fn read_timeout(&self) -> Result<Option<Duration>, io::Error> {
		Ok(self.ignored_read_timeout)
	}

	fn name(&self) -> Option<String> {
		Some(format!("<mock 0x{:x}>", self as *const Mock as usize))
	}
}

#[cfg(test)]
impl io::Read for Mock {
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		if let Some(err) = self.read_error.take() {
			Err(err)
		} else if self.is_empty() {
			// For a real device, having no data ready would result in a wait
			// and then eventual timeout error. However, as our data is in
			// memory that does not happen here. So simulate that behaviour by
			// returning a timeout error immediately.
			Err(io::Error::new(
				io::ErrorKind::TimedOut,
				"Simulated timeout error",
			))
		} else {
			self.buffer.read(buf)
		}
	}
}

#[cfg(test)]
impl io::Write for Mock {
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		if let Some(err) = self.write_error.take() {
			Err(err)
		} else {
			Ok(buf.len())
		}
	}

	fn flush(&mut self) -> io::Result<()> {
		if let Some(err) = self.flush_error.take() {
			Err(err)
		} else {
			Ok(())
		}
	}
}

mod private {
	pub trait Sealed {}

	impl Sealed for super::Serial {}
	impl Sealed for std::net::TcpStream {}
	#[cfg(test)]
	impl Sealed for super::Mock {}
	impl<C: super::Backend + ?Sized> Sealed for Box<C> {}
	impl<C: super::Backend + ?Sized> Sealed for &mut C {}
}
