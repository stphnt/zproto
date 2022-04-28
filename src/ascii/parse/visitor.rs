//! A [`Client`] defines the algorithm for parsing an ASCII packet, but does not
//! define any particular output. The output type is defined by a separate
//! [`Visitor`] types. In this way the parsing algorithm is only defined once
//! but multiple output types can be defined with minimal work.
//!
//! The [`Client`] and [`Visitor`] traits are not intended for the public
//! interface, but used by dedicated parsing functions for each type.
use super::*;
use crate::ascii::{Flag, Status, Warning};

/// The `cont` keyword.
const CONT: &[u8] = b"cont";

/// The kind of packet
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PacketKind {
    /// A command (`/`) packet.
    Command,
    /// A reply (`@`) packet.
    Reply,
    /// An info (`#`) packet.
    Info,
    /// An alert (`!`) packet.
    Alert,
}

impl PacketKind {
    /// Whether the packet is a Reply, Info, or Alert packet.
    fn is_response(self) -> bool {
        matches!(
            self,
            PacketKind::Reply | PacketKind::Info | PacketKind::Alert
        )
    }
}

/// The kinds of tokens parsed from a packet by a client.
///
/// This is never exposed by the client.
#[derive(Debug, Copy, Clone)]
enum ClientToken<'a> {
    /// A reserved character sequence.
    Reserved(&'a [u8]),
    /// A whitespace/reserved character delimited word.
    Word(&'a [u8]),
}

impl<'a> ClientToken<'a> {
    /// Convert a token to it's bytes.
    fn into_bytes(self) -> &'a [u8] {
        match self {
            ClientToken::Reserved(bytes) | ClientToken::Word(bytes) => bytes,
        }
    }
}

/// A visitor for parsing a packet.
///
/// Types that implement Visitor enable the [`Client`] to parse a packet into
/// the `Output` type.
pub(crate) trait Visitor<'a> {
    /// The type that is generated after successfully parsing a packet.
    type Output;

    /// Called with the contents of token separating whitespace.
    fn separator(&mut self, bytes: &'a [u8]);
    /// Called with the contents of the packet kind field.
    fn kind(&mut self, kind: PacketKind, bytes: &'a [u8]);
    /// Called with the contents of the device address field.
    fn device_address(&mut self, value: u8, bytes: &'a [u8]);
    /// Called with the contents of the axis number field.
    fn axis_number(&mut self, value: u8, bytes: &'a [u8]);
    /// Called with the contents of the message ID field.
    fn message_id(&mut self, value: u8, bytes: &'a [u8]);
    /// Called with the contents of the reply flag field.
    fn flag(&mut self, flag: Flag, bytes: &'a [u8]);
    /// Called with the contents of the status field.
    fn status(&mut self, status: Status, bytes: &'a [u8]);
    /// Called with the contents of the warning field.
    fn warning(&mut self, warning: Warning, bytes: &'a [u8]);
    /// Called with the contents of the `cont` word.
    fn cont(&mut self, bytes: &'a [u8]);
    /// Called with the contents of the count field following `cont` word.
    fn cont_count(&mut self, value: u8, bytes: &'a [u8]);
    /// Called with the contents of the a single data word.
    fn data_word(&mut self, bytes: &'a [u8]);
    /// Called with the contents of all the data words, including separating whitespace (but not leading/trailing whitespace).
    ///
    /// This callback reports data already passed to [`data_word`] and [`separator`].
    fn data(&mut self, bytes: &'a [u8]);
    /// Called with the hashed contents of the entire packet (everything between the packet kind marker and the checksum/termination).
    ///
    /// This callback reports data already passed to most other methods in this trait.
    fn hashed_content(&mut self, bytes: &'a [u8]);
    /// Called with the `\` character.
    fn more_packets_marker(&mut self, bytes: &'a [u8]);
    /// Called with the `:` character.
    fn checksum_marker(&mut self, bytes: &'a [u8]);
    /// Called with the contents of the checksum field.
    fn checksum(&mut self, value: u32, bytes: &'a [u8]);
    /// Called with the contents of the termination field.
    fn termination(&mut self, bytes: &'a [u8]);

    /// Called when parsing begins with the entire contents of the packet being parsed.
    ///
    /// The contents of the packet may not be a valid packet as it has not been parsed yet.
    fn start_visit(&mut self, bytes: &'a [u8]);
    /// Called when parsing has successfully completed.
    fn finish_visit(self) -> Self::Output;
}

/// A client for parsing a packet into the output type of the specified visitor `V`
#[derive(Debug)]
pub(crate) struct Client<'a, V> {
    visitor: V,
    packet: &'a [u8],
    index: usize,
}

impl<'a, V: Visitor<'a>> Client<'a, V> {
    /// Create a new `Client` to parse a packet.
    pub fn new(visitor: V, packet: &'a [u8]) -> Self {
        Client {
            visitor,
            packet,
            index: 0,
        }
    }

    /// Parse the packet and return the output of the visitor.
    pub fn parse(mut self) -> Result<V::Output, ParseError> {
        use AsciiExt as _;
        use PacketKind as PK;

        // Start parsing
        self.visitor.start_visit(self.packet);

        // Error on any leading whitespace, to guarantee that the subsequent
        // calls to `reserved` are in fact the first characters in the packet
        let sep = self.count_separator();
        if sep > 0 {
            return Err(ParseError::MissingOrInvalidKind);
        }

        // Parse the kind of packet
        let kind;
        if let Some(bytes) = self.reserved(&[COMMAND_MARKER])? {
            kind = PK::Command;
            self.visitor.kind(kind, bytes);
        } else if let Some(bytes) = self.reserved(&[REPLY_MARKER])? {
            kind = PK::Reply;
            self.visitor.kind(kind, bytes);
        } else if let Some(bytes) = self.reserved(&[INFO_MARKER])? {
            kind = PK::Info;
            self.visitor.kind(kind, bytes);
        } else if let Some(bytes) = self.reserved(&[ALERT_MARKER])? {
            kind = PK::Alert;
            self.visitor.kind(kind, bytes);
        } else {
            return Err(ParseError::MissingOrInvalidKind);
        }
        let content_start_index = self.index;

        // Parse the header
        if let Some(digits) = self.digits()? {
            let address = std::str::from_utf8(digits).unwrap().parse().unwrap();
            self.visitor.device_address(address, digits);

            if let Some(digits) = self.digits()? {
                let axis = std::str::from_utf8(digits).unwrap().parse().unwrap();
                self.visitor.axis_number(axis, digits);

                if let Some(digits) = self.digits()? {
                    let id = std::str::from_utf8(digits).unwrap().parse().unwrap();
                    self.visitor.message_id(id, digits);
                }
            } else if kind.is_response() {
                return Err(ParseError::MissingOrInvalidAxis);
            }
        } else if kind.is_response() {
            return Err(ParseError::MissingOrInvalidAddress);
        }

        // Parse the body, which is different for each packet type.
        match kind {
            PK::Command => {
                if let Some(word) = self.word(CONT)? {
                    self.visitor.cont(word);

                    let word = self
                        .digits()?
                        .ok_or(ParseError::MissingOrInvalidContCount)?;
                    let count = std::str::from_utf8(word).unwrap().parse().unwrap();
                    self.visitor.cont_count(count, word);
                }
                self.parse_data()?;
            }
            PK::Info => {
                if let Some(word) = self.word(CONT)? {
                    self.visitor.cont(word);
                }
                self.parse_data()?;
            }
            PK::Reply | PK::Alert => {
                if kind == PK::Reply {
                    // Parse flag
                    if let Some(word) = self.word(b"OK")? {
                        self.visitor.flag(Flag::Ok, word);
                    } else if let Some(word) = self.word(b"RJ")? {
                        self.visitor.flag(Flag::Rj, word);
                    } else {
                        return Err(ParseError::MissingOrInvalidFlag);
                    }
                }

                // Parse Status
                if let Some(word) = self.word(b"IDLE")? {
                    self.visitor.status(Status::Idle, word);
                } else if let Some(word) = self.word(b"BUSY")? {
                    self.visitor.status(Status::Busy, word);
                } else {
                    return Err(ParseError::MissingOrInvalidStatus);
                }

                // Parse Warning
                let word = self
                    .any_word()?
                    .ok_or(ParseError::MissingOrInvalidWarning)?;
                let warning = Warning::from(
                    TryInto::<[u8; 2]>::try_into(word)
                        .map_err(|_| ParseError::MissingOrInvalidWarning)?,
                );
                self.visitor.warning(warning, word);

                self.parse_data()?;
            }
        }

        // Parse the footer
        let mut content_end_index = None;
        if let Some(word) = self.reserved(&[MORE_PACKETS_MARKER])? {
            self.visitor.more_packets_marker(word);
        }
        if let Some(word) = self.reserved(b":")? {
            content_end_index = Some(self.index - 1);
            self.visitor.checksum_marker(word);
            // Parse the checksum
            let word = self
                .hex_digits()?
                .ok_or(ParseError::MissingOrInvalidChecksum)?;
            let checksum = std::str::from_utf8(word).unwrap();
            let checksum = u32::from_str_radix(checksum, 16)
                .map_err(|_| ParseError::MissingOrInvalidChecksum)?;
            self.visitor.checksum(checksum, word);
        }

        // Parse the termination
        let termination = self
            .token_if(|token| matches!(token, ClientToken::Reserved(bytes) if bytes.iter().all(|c| c.is_packet_end())))?
            .map(ClientToken::into_bytes)
            .ok_or(ParseError::MissingOrInvalidTermination)?;
        self.visitor.termination(termination);

        let content_end_index = content_end_index.unwrap_or(self.index - termination.len());
        self.visitor
            .hashed_content(&self.packet[content_start_index..content_end_index]);

        // Make sure there isn't anything else
        if !self.rest().is_empty() {
            return Err(ParseError::ExtraData);
        }
        Ok(self.visitor.finish_visit())
    }

    /// Consume a token if `predicate` returns `true`.
    ///
    /// Any preceding whitespace is consumed but only if the token is also consumed.
    fn token_if<F>(&mut self, mut predicate: F) -> Result<Option<ClientToken<'a>>, ParseError>
    where
        F: FnMut(ClientToken<'a>) -> bool,
    {
        if self.index == self.packet.len() {
            return Ok(None);
        }
        // Collect any preceding whitespace, but don't consume it just yet.
        // If we match something something else, we'll consume it then.
        let ws_count = self.count_separator();
        let start = self.index + ws_count;

        // First see if we've found a termination sequence.
        let count = (&self.packet[start..])
            .iter()
            .take_while(|c| c.is_packet_end())
            .count();
        if count > 0 {
            let token = ClientToken::Reserved(&self.packet[start..start + count]);
            if (predicate)(token) {
                // Consume the preceding whitespace and then our match.
                self.separator();
                self.index += count;
                return Ok(Some(token));
            } else {
                return Ok(None);
            }
        }

        // Try to take the next single reserved character (terminations are the
        // only sequence of reserved characters)
        if self.packet[start].is_reserved() {
            let token = ClientToken::Reserved(&self.packet[start..start + 1]);
            if (predicate)(token) {
                // Consume the preceding whitespace and then our match.
                self.separator();
                self.index += 1;
                return Ok(Some(token));
            } else {
                return Ok(None);
            }
        }

        // Take the next word, which we know must exist
        let count = (&self.packet[start..])
            .iter()
            .take_while(|c| !c.is_separator() && !c.is_reserved())
            .count();
        assert!(count > 0);
        let valid_ascii = self.packet[start..start + count]
            .iter()
            .all(|c| c.is_ascii());
        if !valid_ascii {
            return Err(ParseError::NonAscii);
        }
        let token = ClientToken::Word(&self.packet[start..start + count]);
        if (predicate)(token) {
            // Consume the preceding whitespace and then our match.
            self.separator();
            self.index += count;
            Ok(Some(token))
        } else {
            Ok(None)
        }
    }

    /// Count the number of whitespace separator characters could be parsed.
    ///
    /// The characters are not consumed.
    fn count_separator(&self) -> usize {
        (&self.packet[self.index..])
            .iter()
            .take_while(|c| c.is_separator())
            .count()
    }

    /// Consume whitespace separator characters if they are present.
    ///
    /// It will consume the number of characters as returned by [`count_separator`].
    fn separator(&mut self) -> Option<&'a [u8]> {
        let count = self.count_separator();
        if count > 0 {
            let slice = &self.packet[self.index..self.index + count];
            self.index += count;
            self.visitor.separator(slice);
            Some(slice)
        } else {
            None
        }
    }

    /// Consume a token containing only digits, along with preceding whitespace.
    fn digits(&mut self) -> Result<Option<&'a [u8]>, ParseError> {
        Ok(self.token_if(
            |token| matches!(token, ClientToken::Word(bytes) if bytes.iter().all(|c| c.is_ascii_digit())),
        )?
        .map(ClientToken::into_bytes))
    }

    /// Consume a token containing only hex digits, along with preceding whitespace.
    fn hex_digits(&mut self) -> Result<Option<&'a [u8]>, ParseError> {
        Ok(self.token_if(|token| matches!(token, ClientToken::Word(bytes) if bytes.iter().all(|c| c.is_ascii_hexdigit())))?
        .map(ClientToken::into_bytes))
    }

    /// Consume any word token, along with preceding whitespace.
    fn any_word(&mut self) -> Result<Option<&'a [u8]>, ParseError> {
        Ok(self
            .token_if(|token| matches!(token, ClientToken::Word(_)))?
            .map(ClientToken::into_bytes))
    }

    /// Consume a specific word token, along with preceding whitespace.
    fn word(&mut self, tag: &[u8]) -> Result<Option<&'a [u8]>, ParseError> {
        Ok(self
            .token_if(|token| matches!(token, ClientToken::Word(bytes) if bytes == tag))?
            .map(ClientToken::into_bytes))
    }

    /// Consume a specific reserved token, along with preceding whitespace.
    fn reserved(&mut self, tag: &[u8]) -> Result<Option<&'a [u8]>, ParseError> {
        Ok(self
            .token_if(|token| matches!(token, ClientToken::Reserved(bytes) if bytes == tag))?
            .map(ClientToken::into_bytes))
    }

    /// Consume all word tokens along with separating whitespace.
    fn parse_data(&mut self) -> Result<(), ParseError> {
        // Consume any leading whitespace so we know where the first data word
        // starts and then consume as many words as we possibly can.
        self.separator();
        let start = self.index;
        while let Some(word) = self.any_word()? {
            self.visitor.data_word(word);
        }
        if self.index > start {
            self.visitor.data(&self.packet[start..self.index]);
        }
        Ok(())
    }

    /// Return the rest of the packet.
    fn rest(&self) -> &'a [u8] {
        &self.packet[self.index..]
    }
}

/// Error parsing packet
#[allow(missing_docs)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ParseError {
    MissingOrInvalidKind,
    MissingOrInvalidAddress,
    MissingOrInvalidAxis,
    MissingOrInvalidContCount,
    MissingOrInvalidFlag,
    MissingOrInvalidStatus,
    MissingOrInvalidWarning,
    MissingOrInvalidChecksum,
    MissingOrInvalidTermination,
    NonAscii,
    ExtraData,
}
