//! Types for parsing ASCII packets from bytes.
//!
//! There are two main types for parsing packets: [`Packet`] and [`Tokens`].
//! A `Packet` parses the bytes into usable types, such as [`Target`]
//! or [`Status`], while [`Tokens`] parse the packet into a
//! stream of [`Token`]s. [`Tokens`] does not parse the bytes into other data
//! types, but merely associates subslices of the packet's bytes with a
//! particular meaning, such as [`Token::Kind`] or [`Token::DataWord`].
//!
//! See each type's documentation for more information.
//!
//! [`Status`]: crate::ascii::response::Status
//! [`Target`]: crate::ascii::command::Target

#[allow(clippy::module_inception)]
mod packet;
#[cfg(test)]
mod test;
mod token;
mod visitor;

pub use packet::{Packet, RefPacket};
pub use token::{RefTokens, Token, TokenIter, Tokens};
pub use visitor::PacketKind;

/// The byte preceding a checksum.
pub(crate) const CHECKSUM_MARKER: u8 = b':';

/// The byte indicating the message is incomplete and continued in a subsequent info message.
pub(crate) const MORE_PACKETS_MARKER: u8 = b'\\';

/// The first byte of a command message.
pub(crate) const COMMAND_MARKER: u8 = b'/';

/// The first byte of a reply message.
pub(crate) const REPLY_MARKER: u8 = b'@';

/// The first byte of an info message.
pub(crate) const INFO_MARKER: u8 = b'#';

/// The first byte of an alert message.
pub(crate) const ALERT_MARKER: u8 = b'!';

/// The carriage return byte.
pub(crate) const CARRIAGE_RETURN: u8 = b'\r';

/// The line feed byte.
pub(crate) const LINE_FEED: u8 = b'\n';

/// A trait for identifying a byte as special characters in Zaber's ASCII protocol.
pub(crate) trait AsciiExt {
	/// The character is a reserved character
	fn is_reserved(&self) -> bool;
	/// The character is a word separator
	fn is_separator(&self) -> bool;
	/// The character is the end of a packet
	fn is_packet_end(&self) -> bool;
	/// The character is the start of a packet
	fn is_packet_start(&self) -> bool;
}

impl AsciiExt for u8 {
	fn is_reserved(&self) -> bool {
		matches!(
			*self,
			CHECKSUM_MARKER
				| MORE_PACKETS_MARKER
				| COMMAND_MARKER
				| REPLY_MARKER
				| INFO_MARKER
				| ALERT_MARKER
				| CARRIAGE_RETURN
				| LINE_FEED
		)
	}

	fn is_separator(&self) -> bool {
		matches! {
			*self,
			b' ' | b'\t'
		}
	}

	fn is_packet_end(&self) -> bool {
		matches!(*self, CARRIAGE_RETURN | LINE_FEED)
	}

	fn is_packet_start(&self) -> bool {
		matches!(
			*self,
			COMMAND_MARKER | REPLY_MARKER | ALERT_MARKER | INFO_MARKER
		)
	}
}
