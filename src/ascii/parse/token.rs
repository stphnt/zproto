use crate::ascii::{
    parse::visitor::{Client, PacketKind, Visitor},
    Flag, Status, Warning,
};
use crate::error::AsciiPacketMalformedError;

/// A token in an ASCII packet.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Token {
    /// The kind (type) of the packet
    Kind,
    /// The device address
    DeviceAddress,
    /// The axis number
    AxisNumber,
    /// The message ID
    MessageId,
    /// The reply flag
    Flag,
    /// The device status
    Status,
    /// The warning
    Warning,
    /// The `cont` keyword
    Cont,
    /// The count following `cont` (not used by all packet types)
    ContCount,
    /// A word in the data field
    DataWord,
    /// The `\` character
    MorePacketsMarker,
    /// The prefix `:` of a checksum
    ChecksumMarker,
    /// The checksum
    Checksum,
    /// The termination sequence (`\n` or `\r\n`)
    Terminator,
    /// Whitespace separating words (not including termination characters)
    Separator,
}

/// The tokens in a packet.
///
/// There are two different "flavours" of `Tokens`. The default one that stores
/// an owned copy of the packet bytes when parsing, constructed via the
/// [`new`](Tokens::new) constructor. And another that stores a reference to the
/// packet bytes when parsing, constructed via the [`new_ref`](Tokens::new_ref)
/// constructor.
///
/// Access to the underlying tokens and their associated bytes are exposed via
/// a [`TokenIter`], created with the [`iter`](Tokens::iter) method.
///
/// ## Examples
///
/// ```
/// # use zproto::ascii::parse::{Token, Tokens};
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let packet = b"@01 2 OK IDLE -- 0\r\n";
/// let tokens = Tokens::new_ref(packet)?;
/// let pairs: Vec<(Token, &[u8])> = tokens.iter().collect();
/// assert_eq!(pairs, &[
///     (Token::Kind, b"@".as_slice()),
///     (Token::DeviceAddress, b"01"),
///     (Token::Separator, b" "),
///     (Token::AxisNumber, b"2"),
///     (Token::Separator, b" "),
///     (Token::Flag, b"OK"),
///     (Token::Separator, b" "),
///     (Token::Status, b"IDLE"),
///     (Token::Separator, b" "),
///     (Token::Warning, b"--"),
///     (Token::Separator, b" "),
///     (Token::DataWord, b"0"),
///     (Token::Terminator, b"\r\n"),
/// ]);
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Tokens<T = Box<[u8]>> {
    pub(crate) packet: T,
    pub(crate) items: Vec<(Token, u8)>,
}

/// A type alias for [`Tokens`] that takes a reference to its data.
///
/// Returned by [`Tokens::new_ref`].
pub type RefTokens<'a> = Tokens<&'a [u8]>;

impl Tokens<Box<[u8]>> {
    /// Try to parse the packet's bytes and create an instance of [`Tokens`].
    ///
    /// The `Tokens` will store a copy of the bytes.
    pub fn new(packet: &[u8]) -> Result<Self, AsciiPacketMalformedError> {
        Tokens::try_from(packet)
    }
}

impl<'a> Tokens<&'a [u8]> {
    /// Try to parse the packet's bytes and create an instance of `Packet` that references `packet`.
    ///
    /// This avoids an extra memory allocation but means the `Packet` cannot outlive the bytes it references.
    pub fn new_ref(packet: &'a [u8]) -> Result<Self, AsciiPacketMalformedError> {
        Tokens::try_from(packet)
    }
}

impl<T> Tokens<T>
where
    T: Default,
{
    /// Create a new `Tokens` but with "garbage" default values.
    ///
    /// It must be initialized via a [`Client`].
    pub(crate) fn new_default() -> Self {
        Tokens {
            packet: T::default(),
            items: Vec::new(),
        }
    }
}

impl<T> Tokens<T>
where
    T: AsRef<[u8]>,
{
    /// Returns an iterator over the tokens in the packet.
    pub fn iter(&self) -> TokenIter<'_, T> {
        TokenIter {
            tokens: self,
            token_index: 0,
            packet_index: 0,
        }
    }
}

impl<'a, T> Visitor<'a> for Tokens<T>
where
    T: From<&'a [u8]>,
{
    type Output = Self;

    fn separator(&mut self, bytes: &[u8]) {
        self.items.push((Token::Separator, bytes.len() as u8));
    }
    fn kind(&mut self, _: PacketKind, bytes: &[u8]) {
        self.items.push((Token::Kind, bytes.len() as u8));
    }
    fn device_address(&mut self, _: u8, bytes: &[u8]) {
        self.items.push((Token::DeviceAddress, bytes.len() as u8));
    }
    fn axis_number(&mut self, _: u8, bytes: &[u8]) {
        self.items.push((Token::AxisNumber, bytes.len() as u8));
    }
    fn message_id(&mut self, _: u8, bytes: &[u8]) {
        self.items.push((Token::MessageId, bytes.len() as u8));
    }
    fn flag(&mut self, _: Flag, bytes: &[u8]) {
        self.items.push((Token::Flag, bytes.len() as u8));
    }
    fn status(&mut self, _: Status, bytes: &[u8]) {
        self.items.push((Token::Status, bytes.len() as u8));
    }
    fn warning(&mut self, _: Warning, bytes: &[u8]) {
        self.items.push((Token::Warning, bytes.len() as u8));
    }
    fn cont(&mut self, bytes: &[u8]) {
        self.items.push((Token::Cont, bytes.len() as u8));
    }
    fn cont_count(&mut self, _: u8, bytes: &[u8]) {
        self.items.push((Token::ContCount, bytes.len() as u8));
    }
    fn data_word(&mut self, bytes: &[u8]) {
        self.items.push((Token::DataWord, bytes.len() as u8));
    }
    fn data(&mut self, _: &'a [u8]) {
        // Skipped because it overlaps with `data_word` and `separator`
    }
    fn content(&mut self, _: &'a [u8]) {
        // Skipped because it overlaps with other methods.
    }
    fn more_packets_marker(&mut self, bytes: &[u8]) {
        self.items
            .push((Token::MorePacketsMarker, bytes.len() as u8));
    }
    fn checksum_marker(&mut self, bytes: &[u8]) {
        self.items.push((Token::ChecksumMarker, bytes.len() as u8));
    }
    fn checksum(&mut self, _: u32, bytes: &[u8]) {
        self.items.push((Token::Checksum, bytes.len() as u8));
    }
    fn termination(&mut self, bytes: &[u8]) {
        self.items.push((Token::Terminator, bytes.len() as u8));
    }

    fn start_visit(&mut self, bytes: &'a [u8]) {
        self.packet = bytes.into();
    }
    fn finish_visit(self) -> Self::Output {
        self
    }
}

impl<'a> TryFrom<&'a [u8]> for Tokens<Box<[u8]>> {
    type Error = AsciiPacketMalformedError;

    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        let client = Client::new(Tokens::new_default(), value);
        client
            .parse()
            .map_err(|_| AsciiPacketMalformedError::new(value))
    }
}

impl<'a> TryFrom<&'a [u8]> for Tokens<&'a [u8]> {
    type Error = AsciiPacketMalformedError;

    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        let client = Client::new(Tokens::new_default(), value);
        client
            .parse()
            .map_err(|_| AsciiPacketMalformedError::new(value))
    }
}

/// An iterator over the tokens in a packet.
// Support packets of at most 256 characters (which is fine because packets are
// at most 80 characters).
#[derive(Debug)]
pub struct TokenIter<'a, T> {
    tokens: &'a Tokens<T>,
    token_index: u8,
    packet_index: u8,
}

impl<'a, T> Iterator for TokenIter<'a, T>
where
    T: AsRef<[u8]>,
{
    type Item = (Token, &'a [u8]);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((token, len)) = self.tokens.items.get(self.token_index as usize) {
            let start = self.packet_index as usize;
            self.packet_index += *len as u8;
            self.token_index += 1;
            Some((
                *token,
                &self.tokens.packet.as_ref()[start..start + (*len as usize)],
            ))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn type_alias() {
        let _: Result<RefTokens<'_>, AsciiPacketMalformedError> = Tokens::new_ref(b"");
    }

    #[test]
    fn iter() {
        struct Case {
            tokens: Tokens<&'static [u8]>,
            expected: &'static [(Token, &'static [u8])],
        }

        let cases = &[Case {
            tokens: Tokens::new_ref(b"@01 2 3 OK BUSY WR 0.123 1.234 NA\\:55\r\n").unwrap(),
            expected: &[
                (Token::Kind, b"@"),
                (Token::DeviceAddress, b"01"),
                (Token::Separator, b" "),
                (Token::AxisNumber, b"2"),
                (Token::Separator, b" "),
                (Token::MessageId, b"3"),
                (Token::Separator, b" "),
                (Token::Flag, b"OK"),
                (Token::Separator, b" "),
                (Token::Status, b"BUSY"),
                (Token::Separator, b" "),
                (Token::Warning, b"WR"),
                (Token::Separator, b" "),
                (Token::DataWord, b"0.123"),
                (Token::Separator, b" "),
                (Token::DataWord, b"1.234"),
                (Token::Separator, b" "),
                (Token::DataWord, b"NA"),
                (Token::MorePacketsMarker, b"\\"),
                (Token::ChecksumMarker, b":"),
                (Token::Checksum, b"55"),
                (Token::Terminator, b"\r\n"),
            ],
        }];

        for case in cases {
            assert_eq!(case.tokens.iter().collect::<Vec<_>>(), case.expected);
        }
    }
}