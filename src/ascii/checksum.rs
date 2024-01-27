//! Types for producing and verifying ASCII packet checksums

use std::io;

/// A Longitudinal Redundancy Check hasher.
#[derive(Debug, Default)]
pub(crate) struct Lrc {
	/// The hash value
	sum: u32,
}

impl Lrc {
	/// Update the Lrc with the specified byte.
	pub fn update(&mut self, byte: u8) {
		self.sum = (self.sum + byte as u32) & 0xFF;
	}

	/// Clear the hasher's state. This returns the hasher to the state after
	/// initially calling `Lrc::default()`.
	pub fn reset(&mut self) {
		self.sum = 0;
	}

	/// Finish calculating the Lrc hash.
	///
	/// The hasher's state is reset.
	pub fn finish(&mut self) -> u32 {
		let sum = ((self.sum ^ 0xFF) + 1) & 0xFF;
		self.reset();
		sum
	}

	/// Verif if the hash matches the input.
	pub fn verify(input: &[u8], hash: u32) -> bool {
		let sum: u32 = input.iter().fold(0u32, |sum, b| *b as u32 + sum);
		0 == ((sum + hash) & 0xFF)
	}
}

/// A type that implement [`io::Write`] and calculates the LRC of the data
/// written to it.
#[derive(Debug)]
pub(crate) struct LrcWriter<W> {
	hasher: Lrc,
	writer: W,
}

impl<W> LrcWriter<W> {
	/// Create a new `LrcWriter` wrapping `writer`.
	pub fn new(writer: W) -> LrcWriter<W> {
		LrcWriter {
			hasher: Lrc::default(),
			writer,
		}
	}

	/// Reset the hash state.
	pub fn reset_hash(&mut self) {
		self.hasher.reset();
	}

	/// Calculate the hash for the data received so far and reset the hash state.
	pub fn finish_hash(&mut self) -> u32 {
		self.hasher.finish()
	}

	/// Consume this writer and return the inner one.
	#[cfg(test)]
	pub fn into_inner(self) -> W {
		self.writer
	}
}

impl<W> io::Write for LrcWriter<W>
where
	W: io::Write,
{
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		for byte in buf {
			self.hasher.update(*byte)
		}
		self.writer.write(buf)
	}
	fn flush(&mut self) -> io::Result<()> {
		self.writer.flush()
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use std::io::Write as _;

	#[test]
	fn test_lrc() {
		assert!(Lrc::verify(b"01 tools echo", 143));
		assert!(!Lrc::verify(b"01 tools echo", 142));
	}

	/// Writing to an LrcWriter should both write the data and correctly update the hash.
	#[test]
	fn test_lrc_writer() {
		let buf = Vec::with_capacity(80);
		let writer = std::io::Cursor::new(buf);

		let mut lrc_writer = LrcWriter::new(writer);
		write!(&mut lrc_writer, "hello world").unwrap();
		let hash = lrc_writer.finish_hash();
		assert!(Lrc::verify(b"hello world", hash));
		assert_eq!(
			lrc_writer.into_inner().into_inner().as_slice(),
			b"hello world"
		);
	}

	/// Writing to an LrcWriter should both write the data and correctly update the hash.
	#[test]
	fn test_lrc_writer_reset_hash() {
		let buf = Vec::with_capacity(80);
		let writer = std::io::Cursor::new(buf);

		let mut lrc_writer = LrcWriter::new(writer);
		write!(&mut lrc_writer, "hello ").unwrap();
		lrc_writer.reset_hash();
		write!(&mut lrc_writer, "world").unwrap();
		let hash = lrc_writer.finish_hash();
		assert!(Lrc::verify(b"world", hash));
		assert_eq!(
			lrc_writer.into_inner().into_inner().as_slice(),
			b"hello world"
		);
	}
}
