//! Types and traits for generating ASCII commands.

use crate::{
	ascii::{checksum::LrcWriter, id},
	error::{AsciiCommandSplitError, AsciiError, AsciiReservedCharacterError},
};
use std::borrow::Cow;
use std::io;

mod private {
	use super::*;
	pub trait Sealed {}

	impl Sealed for str {}
	impl Sealed for String {}
	impl Sealed for [u8] {}
	impl<const N: usize> Sealed for [u8; N] {}
	impl Sealed for Vec<u8> {}
	impl<T, D> Sealed for (T, D)
	where
		T: Into<Target> + Copy,
		D: AsRef<[u8]>,
	{
	}
	impl<D: AsRef<[u8]>> Sealed for (u8, u8, D) {}
	impl<T> Sealed for &T where T: Sealed + ?Sized {}
	impl<T> Sealed for &mut T where T: Sealed + ?Sized {}
	impl<T> Sealed for Box<T> where T: Sealed + ?Sized {}
}

/// A trait that is implemented by all ASCII commands.
///
/// Multiple types implement `Command` including:
///
/// * `str`,
/// * `String`,
/// * `[u8; N]`,
/// * `[u8]`,
/// * `Vec<u8>`,
/// * and any `&T`, `&mut T`, or `Box<T>` where `T` implements `Command`.
///
/// When used as a command the bytes in these types will be interpreted as the
/// data for a command and targeted to all devices and axes in the chain. For
/// instance, the following are examples of how the above types are converted
/// into ASCII commands:
///
/// * `"home"` → `"/home\n"`
/// * `format!("move abs {}", 12345)` → `"/move abs 12345\n"`
///
/// To target specific devices and/or axes, prepend any type convertible to a
/// [`Target`] to the data as part of a tuple:
///
/// * `(1, "home")` → `"/1 home\n"`
/// * `((1, 2), format!("move abs {}", 12345))` → `"/1 2 move abs 12345\n"`
///
/// For convenience, tuples of the form `(u8, u8, data)` are also supported:
///
/// * `(1, 2, "home")` → `"/1 2 home\n"`
///
/// Note, whether message IDs and/or checksums are generated for each command
/// is defined by the [`Port`](crate::ascii::Port). By default they are both
/// enabled.
pub trait Command: private::Sealed {
	/// Get the command's target.
	fn target(&self) -> Target;
	/// Get the command's data.
	fn data(&self) -> Cow<'_, [u8]>;
}

impl Command for str {
	fn target(&self) -> Target {
		Target::for_all()
	}
	fn data(&self) -> Cow<'_, [u8]> {
		self.as_bytes().into()
	}
}

impl Command for [u8] {
	fn target(&self) -> Target {
		Target::for_all()
	}
	fn data(&self) -> Cow<'_, [u8]> {
		self.into()
	}
}

impl<const N: usize> Command for [u8; N] {
	fn target(&self) -> Target {
		Target::for_all()
	}
	fn data(&self) -> Cow<'_, [u8]> {
		self.as_slice().into()
	}
}

impl Command for String {
	fn target(&self) -> Target {
		Target::for_all()
	}
	fn data(&self) -> Cow<'_, [u8]> {
		self.as_bytes().into()
	}
}

impl Command for Vec<u8> {
	fn target(&self) -> Target {
		Target::for_all()
	}
	fn data(&self) -> Cow<'_, [u8]> {
		self.as_slice().into()
	}
}

impl<T, D> Command for (T, D)
where
	T: Into<Target> + Copy,
	D: AsRef<[u8]>,
{
	fn target(&self) -> Target {
		self.0.into()
	}
	fn data(&self) -> Cow<'_, [u8]> {
		self.1.as_ref().into()
	}
}

impl<D> Command for (u8, u8, D)
where
	D: AsRef<[u8]>,
{
	fn target(&self) -> Target {
		Target::for_device(self.0).with_axis(self.1)
	}
	fn data(&self) -> Cow<'_, [u8]> {
		self.2.as_ref().into()
	}
}

impl<T> Command for &T
where
	T: Command + ?Sized,
{
	fn target(&self) -> Target {
		(**self).target()
	}
	fn data(&self) -> Cow<'_, [u8]> {
		(**self).data()
	}
}

impl<T> Command for &mut T
where
	T: Command + ?Sized,
{
	fn target(&self) -> Target {
		(**self).target()
	}
	fn data(&self) -> Cow<'_, [u8]> {
		(**self).data()
	}
}

impl<T> Command for Box<T>
where
	T: Command + ?Sized,
{
	fn target(&self) -> Target {
		(**self).target()
	}
	fn data(&self) -> Cow<'_, [u8]> {
		(**self).data()
	}
}

/// The maximum number of bytes to put in a command packet.
///
/// It must be 80 bytes or larger. The [`default`](MaxPacketSize::default) is 80 bytes.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct MaxPacketSize(usize);

impl MaxPacketSize {
	const MIN_PACKET_SIZE: usize = 80;

	/// Create a new `MaxPacketSize` if the value is >= 80.
	pub const fn new(value: usize) -> Option<Self> {
		if value >= Self::MIN_PACKET_SIZE {
			Some(MaxPacketSize(value))
		} else {
			None
		}
	}

	/// The default maximum command packet size is 80 bytes.
	pub const fn default() -> MaxPacketSize {
		MaxPacketSize(Self::MIN_PACKET_SIZE)
	}

	/// Get the number of bytes as a `usize`.
	pub const fn as_usize(self) -> usize {
		self.0
	}
}

impl Default for MaxPacketSize {
	/// The default maximum command packet size is 80 bytes.
	fn default() -> Self {
		MaxPacketSize::default()
	}
}

/// A type for writing commands to the serial port.
pub(crate) struct CommandWriter<'a> {
	/// The targeted device/axis
	pub target: Target,
	/// The message ID
	pub id: Option<u8>,
	/// The command data
	pub data: Cow<'a, [u8]>,
	/// The current offset into the data
	pub offset: usize,
	/// Whether to generate a checksum
	pub checksum: bool,
	/// The maximum number of bytes to put in a packet
	pub max_packet_size: MaxPacketSize,
	/// The index of the next packet to send. 0 is the first packet.
	pub packet_index: usize,
}

impl<'a> CommandWriter<'a> {
	/// Create a command writer.
	///
	/// Returns an error if the command data contains any [reserved characters].
	///
	/// [reserved characters]: https://www.zaber.com/protocol-manual?protocol=ASCII#topic_message_format__reserved_characters
	pub fn new<C, G>(
		command: &'a C,
		mut generator: G,
		generate_id: bool,
		generate_checksum: bool,
		max_packet_size: MaxPacketSize,
	) -> Result<CommandWriter<'a>, AsciiReservedCharacterError>
	where
		C: Command,
		G: id::Generator,
	{
		let data = command.data();
		// Check that the command data doesn't contain any reserved ASCII protocol
		// characters
		if let Some(reserved) = data
			.iter()
			.find(|byte| **byte > 127 || b"/@#!\r\n:\\".contains(byte))
		{
			return Err(AsciiReservedCharacterError::new(command, *reserved));
		}
		Ok(CommandWriter {
			target: command.target(),
			id: if generate_id {
				Some(generator.next_id())
			} else {
				None
			},
			data,
			offset: 0,
			checksum: generate_checksum,
			max_packet_size,
			packet_index: 0,
		})
	}

	/// Returns `true` if the command has been completely written out.
	fn is_complete(&self) -> bool {
		self.packet_index > 0
			&& (self.offset >= self.data.len() || self.data.iter().all(u8::is_ascii_whitespace))
	}

	/// Write the packet header and return the number of bytes written.
	///
	/// Only the minimum number of bytes is used and no trailing whitespace is added.
	fn write_packet_header<W: io::Write>(
		&mut self,
		writer: &mut LrcWriter<W>,
	) -> io::Result<usize> {
		use std::io::Write as _;

		let device_char_count = ascii_char_count(self.target.device() as usize);
		let axis_char_count = ascii_char_count(self.target.axis() as usize);
		write!(writer, "/")?;
		let mut bytes_written = 1; // '/'

		// Do not include the leading slash in the checksum
		writer.reset_hash();

		// Only output the address, axis or message ID if it is necessary.
		match self.id {
			Some(id) => {
				write!(
					writer,
					"{} {} {}",
					self.target.device(),
					self.target.axis(),
					id
				)?;
				bytes_written +=
					device_char_count + axis_char_count + ascii_char_count(id as usize) + 2;
				// 2 spaces
			}
			None => {
				if self.target.axis() != 0 {
					write!(writer, "{} {}", self.target.device(), self.target.axis())?;
					bytes_written += device_char_count + axis_char_count + 1; // 1 space
				} else if self.target.device() != 0 {
					write!(writer, "{}", self.target.device())?;
					bytes_written += device_char_count;
				}
			}
		};
		Ok(bytes_written)
	}

	/// Write a packet for this command to `writer`. Returns `true` if more packets
	/// need to be written, otherwise returns `false`.
	pub fn write_packet<W: io::Write + ?Sized>(
		&mut self,
		writer: &mut W,
	) -> Result<bool, AsciiError> {
		use std::io::Write as _;

		if self.is_complete() {
			return Ok(false);
		}

		let writer = &mut LrcWriter::new(writer);
		let mut bytes_written = self.write_packet_header(writer)?;

		let data = &self.data[self.offset..];
		let mut words = data
			.split(u8::is_ascii_whitespace)
			.filter(|word| !word.is_empty()) // Remove multiple adjacent whitespace
			.enumerate()
			.peekable();
		if words.peek().is_some() {
			if bytes_written > 1 {
				// We've written numbers in the header, add a delimiting whitespace
				// before we write the data portion.
				write!(writer, " ")?;
				bytes_written += 1;
			}

			if self.packet_index != 0 {
				// This is a continuation packet so add the preamble
				write!(writer, "cont {} ", self.packet_index)?;
				bytes_written += 6 + ascii_char_count(self.packet_index);
			}

			// Only add the data that will fit in the packet
			let mut remaining = self.max_packet_size.as_usize()
                - bytes_written // The header we've already written
                - if self.checksum { 3 } else { 0 } // save space for the checksum
                - 1; // save space for the '\n' terminator
			let mut wrote_word = false;
			while let Some((index, word)) = words.next() {
				let mut needed_bytes = word.len();
				if index != 0 {
					needed_bytes += 1; // Leading delimiting space
				}
				if needed_bytes <= remaining {
					// This word fits
					if words.peek().is_some() && needed_bytes == remaining {
						// We cannot add the necessary trailing space (if the
						// next word fits in the packet) or the `\` (if the next
						// word does not). So we must split the packet before
						// this word.
						self.offset = word.as_ptr() as usize - self.data.as_ptr() as usize;
						break;
					}

					if index != 0 {
						writer.write_all(b" ")?;
					}
					writer.write_all(word)?;
					wrote_word = true;
					remaining -= needed_bytes;

					// Move the offset past this word
					self.offset = words.peek().map_or_else(
						|| self.data.len(), // default
						|(_, word)| word.as_ptr() as usize - self.data.as_ptr() as usize,
					);
				} else {
					// The word doesn't fit, split here
					self.offset = word.as_ptr() as usize - self.data.as_ptr() as usize;
					break;
				}
			}
			if !wrote_word {
				// Could not find a whitespace to split the command at
				return Err(AsciiCommandSplitError::new((self.target, self.data.to_vec())).into());
			}
			if self.offset != self.data.len() {
				writer.write_all(b"\\")?;
			}
		}

		if self.checksum {
			let checksum = writer.finish_hash();
			write!(writer, ":{checksum:02X}")?;
		}
		writer.write_all(b"\n")?;
		self.packet_index += 1;
		Ok(!self.is_complete())
	}
}

/// Calculates the number of ASCII digits that are required to print `num`.
fn ascii_char_count(mut num: usize) -> usize {
	let mut count = 1;
	while num >= 10 {
		count += 1;
		num /= 10;
	}
	count
}

/// The device address and axis number a command/response was sent to/from.
///
/// `Target` has multiple builder methods that can be chained to construct the
/// desired target.
///
/// ```rust
/// # use zproto::ascii::command::Target;
/// let target = Target::for_device(1).with_axis(2);
/// ```
///
/// Or you can create a target from a `u8` or `tuple` of `u8`s:
///
/// ```rust
/// # use zproto::ascii::command::Target;
/// assert_eq!(Target::for_device(1), Target::from(1));
/// assert_eq!(Target::for_device(1).with_axis(2), Target::from((1, 2)));
/// ```
///
/// The [`Default`](Target::default) target is all devices and axes.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Target(u8, u8);

impl Target {
	/// Create a new target with the specified `device` address and `axis` number.
	pub const fn new(device: u8, axis: u8) -> Target {
		Target(device, axis)
	}
	/// Create a target for all devices and axes (i.e. device address 0 and axis number 0)
	///
	/// ```
	/// # use zproto::ascii::command::Target;
	/// assert_eq!(Target::for_all(), Target::new(0, 0));
	/// ```
	pub const fn for_all() -> Target {
		Target(0, 0)
	}
	/// Create a target for a specific device.
	pub const fn for_device(address: u8) -> Target {
		Target(address, 0)
	}
	/// Create a target for the all axes on this device.
	///
	/// The device address of the current target is copied to the new target.
	///
	/// ```
	/// # use zproto::ascii::command::Target;
	/// assert_eq!(Target::new(2, 1).with_all_axes(), Target::new(2, 0));
	/// ```
	pub const fn with_all_axes(self) -> Target {
		Target(self.0, 0)
	}
	/// Create a target for the specified axis on the device.
	///
	/// The device address of the current target is copied to the new target.
	///
	/// ```
	/// # use zproto::ascii::command::Target;
	/// assert_eq!(Target::new(2, 1).with_axis(3), Target::new(2, 3));
	/// ```
	pub const fn with_axis(self, axis: u8) -> Target {
		Target(self.0, axis)
	}
	/// Get the address of the targeted device.
	///
	/// ```
	/// # use zproto::ascii::command::Target;
	/// assert_eq!(Target::new(2, 1).device(), 2);
	/// ```
	pub const fn device(self) -> u8 {
		self.0
	}
	/// Get the number of the targeted axis.
	///
	/// ```
	/// # use zproto::ascii::command::Target;
	/// assert_eq!(Target::new(2, 1).axis(), 1);
	/// ```
	pub const fn axis(self) -> u8 {
		self.1
	}
	/// Assuming this target is that of a response, determine if the response
	/// could have been elicited by a command to the specified target.
	///
	/// This is true if
	///  * the command's device address was 0 or matches this target's address, and
	///  * the command's axis number was 0 or matches this target's axis number
	pub(crate) const fn elicited_by_command_to(self, target: Target) -> bool {
		if target.0 == 0 || self.0 == target.0 {
			target.1 == 0 || self.1 == target.1
		} else {
			false
		}
	}
}

impl Default for Target {
	/// Get the default target, which is all devices and axes in the chain.
	fn default() -> Target {
		Target::for_all()
	}
}

impl From<u8> for Target {
	fn from(other: u8) -> Target {
		Target(other, 0)
	}
}

impl From<(u8, u8)> for Target {
	fn from(other: (u8, u8)) -> Target {
		Target(other.0, other.1)
	}
}

#[cfg(test)]
mod test {
	use super::*;

	struct ConstId {}
	impl id::Generator for ConstId {
		fn next_id(&mut self) -> u8 {
			5
		}
	}

	#[test]
	fn target_default_is_all() {
		assert_eq!(Target::default(), Target::for_all());
	}

	#[test]
	fn test_target() {
		assert_eq!(Target::default(), Target::for_all());
		assert_eq!(Target::for_device(1), Target(1, 0));
		assert_eq!(Target::for_device(1).with_axis(1), Target(1, 1));
		assert_eq!(Target::new(5, 9).with_all_axes(), Target(5, 0));
	}

	#[test]
	fn test_command_writer() {
		let mut buf = Vec::with_capacity(500);
		struct Case {
			command: &'static (u8, u8, &'static str),
			generate_id: bool,
			generate_checksum: bool,
			expected: &'static [u8],
		}
		let cases = [
            Case {
                command: &(0, 0, ""),
                generate_id: false,
                generate_checksum: false,
                expected: b"/\n",
            },
            Case {
                command: &(0, 0, " \t"),
                generate_id: false,
                generate_checksum: false,
                expected: b"/\n",
            },
            Case {
                command: &(0, 0, ""),
                generate_id: false,
                generate_checksum: true,
                expected: b"/:00\n",
            },
            Case {
                command: &(0, 0, " \t"),
                generate_id: false,
                generate_checksum: true,
                expected: b"/:00\n",
            },
            Case {
                command: &(0, 0, ""),
                generate_id: true,
                generate_checksum: true,
                expected: b"/0 0 5:2B\n",
            },
            Case {
                command: &(0, 0, " \t"),
                generate_id: true,
                generate_checksum: true,
                expected: b"/0 0 5:2B\n",
            },
            Case {
                command: &(1, 0, ""),
                generate_id: false,
                generate_checksum: true,
                expected: b"/1:CF\n",
            },
            Case {
                command: &(0, 1, ""),
                generate_id: false,
                generate_checksum: true,
                expected: b"/0 1:7F\n",
            },
            Case {
                command: &(2, 1, ""),
                generate_id: false,
                generate_checksum: true,
                expected: b"/2 1:7D\n",
            },
            Case {
                command: &(1, 0, "tools echo"),
                generate_id: false,
                generate_checksum: true,
                expected: b"/1 tools echo:BF\n",
            },
            Case {
                command: &(0, 0, "get maxspeed"),
                generate_id: false,
                generate_checksum: true,
                expected: b"/get maxspeed:49\n",
            },
            Case {
                command: &(2, 0, "get maxspeed"),
                generate_id: true,
                generate_checksum: true,
                expected: b"/2 0 5 get maxspeed:52\n",
            },
            Case {
                command: &(1, 0, "tools echo aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff gggggggggg hhhhhhhhhh iiiiiiiiii jjjjjjjjj"),
                generate_id: false,
                generate_checksum: true,
                expected: b"/1 tools echo aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee\\:D0\n/1 cont 1 ffffffffff gggggggggg hhhhhhhhhh iiiiiiiiii jjjjjjjjj:24\n",
            },
            Case {
                command: &(0, 0, "aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff gggggggggg"),  // Should just fit into one packet
                generate_id: false,
                generate_checksum: true,
                expected: b"/aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff gggggggggg:4B\n",
            },
            Case {
                command: &(0, 0, "aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff gggggggggg h"), // Extra h should pull the gg... into next packet to make room for `\`
                generate_id: false,
                generate_checksum: true,
                expected: b"/aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff\\:15\n/cont 1 gggggggggg h:4D\n",
            },
            Case {
                command: &(0, 0, "aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff ggggggggg h"), // Extra h should _not_ pull the gg... into next packet as there is one fewer g.
                generate_id: false,
                generate_checksum: true,
                expected: b"/aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff ggggggggg\\:56\n/cont 1 h:73\n",
            },
            Case {
                command: &(1, 0, "aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff gggggggggg"),  // Larger header should push gg... into next packet.
                generate_id: false,
                generate_checksum: true,
                expected: b"/1 aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff\\:C4\n/1 cont 1 gggggggggg:84\n",
            },
            Case {
                command: &(0, 0, "aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff gggggggggg"),  // Larger header should push gg... into next packet.
                generate_id: true,
                generate_checksum: true,
                expected: b"/0 0 5 aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff\\:20\n/0 0 5 cont 1 gggggggggg:E0\n",
            },
            Case {
                command: &(0, 0, "aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd     \teeeeeeeeee ffffffffff gggggggggg h"), // Extra space should be squashed
                generate_id: false,
                generate_checksum: true,
                expected: b"/aaaaaaaaaa bbbbbbbbbb ccccccccc dddddddddd eeeeeeeeee ffffffffff\\:15\n/cont 1 gggggggggg h:4D\n",
            },
        ];

		for (case_index, case) in cases.into_iter().enumerate() {
			eprintln!("cases[{}] = {:?}", case_index, case.command);

			buf.clear();
			let mut writer = CommandWriter::new(
				&case.command,
				ConstId {},
				case.generate_id,
				case.generate_checksum,
				MaxPacketSize::default(),
			)
			.unwrap();
			let num_expected_packets = case.expected.iter().filter(|byte| **byte == b'\n').count();
			for index in 0..num_expected_packets {
				let more = writer.write_packet(&mut buf).unwrap();
				assert_eq!(
					more,
					index + 1 != num_expected_packets,
					"packet {index}: unexpected write_packet result ({more}): {}",
					std::str::from_utf8(&buf).unwrap()
				);
			}

			assert_eq!(
				buf,
				case.expected,
				"unexpected output: {}",
				String::from_utf8_lossy(&buf)
			);
		}
	}

	#[test]
	fn test_command_writer_custom_packet_size() {
		let mut buf = vec![];
		let _79_bytes =
			"1234567891123456789212345678931234567894123456789512345678961234567897123456789";
		{
			// Should be not be able to fit data plus the leading `/` and trailing `\n`
			let mut writer = CommandWriter::new(
				&_79_bytes,
				ConstId {},
				false,
				false,
				MaxPacketSize::default(),
			)
			.unwrap();
			writer.write_packet(&mut buf).unwrap_err();
		}
		{
			// Should have just enough room for leading `/` and trailing `\n`
			let mut writer = CommandWriter::new(
				&_79_bytes,
				ConstId {},
				false,
				false,
				MaxPacketSize::new(81).unwrap(),
			)
			.unwrap();
			assert!(!writer.write_packet(&mut buf).unwrap());
		}
	}

	#[test]
	fn test_command_writer_cannot_split() {
		let mut buf = vec![];
		let mut writer = CommandWriter::new(&(1, "tools echo aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"), ConstId {}, false, true, MaxPacketSize::default()).unwrap();
		assert!(writer.write_packet(&mut buf).unwrap());
		let _: AsciiCommandSplitError = writer
			.write_packet(&mut buf)
			.unwrap_err()
			.try_into()
			.unwrap();
	}

	#[test]
	fn test_max_packet_size() {
		assert_eq!(MaxPacketSize::default().as_usize(), 80);
		assert!(MaxPacketSize::new(79).is_none());
		assert!(MaxPacketSize::new(80).is_some());
	}

	#[test]
	fn ascii_char_count() {
		let cases = [
			// (input, expected count)
			(0, 1),
			(1, 1),
			(2, 1),
			(3, 1),
			(4, 1),
			(5, 1),
			(6, 1),
			(7, 1),
			(8, 1),
			(9, 1),
			(10, 2),
			(99, 2),
			(100, 3),
			(999, 3),
			(1000, 4),
			(9999, 4),
			(10000, 5),
			(100000, 6),
			(1000000, 7),
			(10000000, 8),
			(100000000, 9),
			(usize::MAX, 20),
		];
		for (input, expected_count) in cases {
			eprintln!("case {input}");
			let actual_count = super::ascii_char_count(input);
			assert_eq!(actual_count, expected_count);
		}
	}
}
