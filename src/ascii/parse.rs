//! Utilities for parsing ASCII messages from bytes.

use nom::{bytes::complete::take_till, IResult};

/// The byte preceding a checksum.
pub const CHECKSUM_MARKER: u8 = b':';

/// The byte indicating the message is incomplete and continued in a subsequent info message.
pub const MORE_PACKETS_MARKER: u8 = b'\\';

/// The first byte of a command message.
pub const COMMAND_MARKER: u8 = b'/';

/// The first byte of a reply message.
pub const REPLY_MARKER: u8 = b'@';

/// The first byte of an info message.
pub const INFO_MARKER: u8 = b'#';

/// The first byte of an alert message.
pub const ALERT_MARKER: u8 = b'!';

/// The carriage return byte.
pub const CARRIAGE_RETURN: u8 = b'\r';

/// The line feed byte.
pub const LINE_FEED: u8 = b'\n';

/// A trait for identifying a byte as special characters in Zaber's ASCII protocol.
pub trait AsciiExt {
    /// The character is a reserved character
    fn is_reserved(&self) -> bool;
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

/// Get the contents of an ASCII message.
///
/// The contents of a message those after the packet start marker but before
/// the checksum marker (`:`) or packet end marker;
pub fn get_packet_contents(input: &[u8]) -> &[u8] {
    let start = input
        .iter()
        .position(|c| c.is_packet_start())
        .map(|i| i + 1)
        .unwrap_or(input.len());
    let len = input[start..]
        .iter()
        .position(|c| *c == CHECKSUM_MARKER || c.is_packet_end())
        .unwrap_or_else(|| input[start..].len());
    &input[start..start + len]
}

/// A trait for parsing a type from a byte slice using `nom`.
pub trait Nom: Sized {
    /// Parse an instance of `Self` from the `input` bytes.
    ///
    /// This conforms to `nom`'s parser interface and can be used with other
    /// `nom` parsers.
    fn nom(input: &[u8]) -> IResult<&[u8], Self>;
}

/// Parse a `u8` from some bytes, assuming that it a number in base 10 and utf8.
pub fn u8_from_base_10(input: &[u8]) -> Result<u8, ()> {
    std::str::from_utf8(input)
        .map_err(|_| ())
        .and_then(|string| string.parse::<u8>().map_err(|_| ()))
}

/// Consume `input` until a reserved character is found
pub fn take_till_reserved(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_till(|byte: u8| byte.is_reserved())(input)
}

/// Consume `input` until a tab, space, or reserved character is found.
pub fn take_till_tab_space_reserved(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_till(|byte: u8| byte == b' ' || byte == b'\t' || byte.is_reserved())(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_get_packet_contents() {
        let expected = b"1 2 OK IDLE --";
        let cases = &[
            b"  /1 2 OK IDLE --\r    ",
            b"  /1 2 OK IDLE --\n    ",
            b"  /1 2 OK IDLE --\r\n   ",
            b"  /1 2 OK IDLE --:12\r\n",
            b"  @1 2 OK IDLE --\r\n   ",
            b"  !1 2 OK IDLE --\r\n   ",
            b"  #1 2 OK IDLE --\r\n   ",
        ];

        for (i, case) in cases.iter().enumerate() {
            assert_eq!(get_packet_contents(*case), expected, "Case {} failed", i);
        }
    }
}
