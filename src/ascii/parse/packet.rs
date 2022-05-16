use crate::ascii::{
    parse::visitor::{Client, PacketKind, Visitor},
    Flag, Status, Target, Warning,
};
use crate::error::AsciiPacketMalformedError;

#[derive(Debug, Clone, PartialEq)]
pub(super) struct InnerPacket<T> {
    /// The entire packet,
    pub packet: T,
    /// The kind of packet.
    pub kind: PacketKind,
    /// The packet's target.
    pub target: (u8, u8),
    /// The message id.
    pub id: Option<u8>,
    /// The reply flag, if it is a reply packet.
    pub flag: Option<Flag>,
    /// The status, if it is a reply or alert packet.
    pub status: Option<Status>,
    /// The warning flag, if it is a reply or alert packet.
    pub warning: Option<Warning>,
    /// The data field (may be empty).
    pub data: T,
    /// The hashed contents of the packet.
    pub hashed_content: T,
    /// Whether this packet continues a previous one (i.e., `cont` keyword is present).
    pub cont: bool,
    /// The number of the continuation packet, if specified.
    ///
    /// For example, in the command `/cont 3 last part\n`, this method returns 3.
    pub cont_count: Option<u8>,
    /// Whether there are more packets in the message after this one (i.e., `\` marker is present).
    pub more_packets: bool,
    /// The parsed checksum value.
    ///
    /// The value may or may not be valid for the contents of the packet.
    pub checksum: Option<u32>,
}

/// A parsed ASCII packet.
///
/// It can represent any kind of packet: Command, Reply, Info, or Alert. As such,
/// many methods return an `Option` because not all packet kinds have a
/// particular field (e.g., only Replies have a reply flag field). However,
/// if a particular packet kind requires a field, then for those kinds of
/// packets it is safe to `unwrap` the value for those fields.
///
/// There are two different "flavours" of `Packet`. The default one that stores
/// an owned copy of the packet bytes when parsing, constructed via the
/// [`new`](Packet::new) constructor. And another that stores a reference to the
/// packet bytes when parsing, constructed via the [`new_ref`](Packet::new_ref)
/// constructor.
///
/// ## Examples
///
/// ```
/// # use zproto::ascii::parse::{PacketKind, Packet};
/// # use zproto::ascii::{Flag, Status, Warning};
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let packet = b"@01 2 OK IDLE -- 0\r\n";
/// let packet = Packet::new_ref(packet)?;
/// assert_eq!(packet.kind(), PacketKind::Reply);
/// assert_eq!(packet.target(), (1, 2).into());
/// assert_eq!(packet.flag().unwrap(), Flag::Ok);
/// assert_eq!(packet.status().unwrap(), Status::Idle);
/// assert_eq!(packet.warning().unwrap(), Warning::NONE);
/// assert_eq!(packet.data(), "0");
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Packet<T = Box<[u8]>>(Box<InnerPacket<T>>);

impl<T> From<InnerPacket<T>> for Packet<T> {
    fn from(other: InnerPacket<T>) -> Self {
        Packet(Box::new(other))
    }
}

/// A type alias for a [`Packet`] that takes a reference to its data.
///
/// Returned by [`Packet::new_ref`].
pub type RefPacket<'a> = Packet<&'a [u8]>;

impl Packet<Box<[u8]>> {
    /// Try to parse the packet's bytes and create an instance of [`Packet`].
    ///
    /// The `Packet` will store a copy of the bytes.
    pub fn new(packet: &[u8]) -> Result<Self, AsciiPacketMalformedError> {
        Packet::try_from(packet)
    }
}

impl<'a> Packet<&'a [u8]> {
    /// Try to parse the packet and create an instance of `Packet` that references `packet`.
    ///
    /// This avoids an extra memory allocation but means the `Packet` cannot outlive the bytes it references.
    pub fn new_ref(packet: &'a [u8]) -> Result<Self, AsciiPacketMalformedError> {
        Packet::try_from(packet)
    }
}

impl<'a, T> Packet<T>
where
    T: Default,
{
    /// Create a new `Packet` but with "garbage" default values.
    ///
    /// It must be initialized via a [`Client`].
    pub(super) fn new_default() -> Self {
        Packet::from(InnerPacket {
            packet: Default::default(),
            kind: PacketKind::Command,
            target: (0, 0),
            id: None,
            flag: None,
            status: None,
            warning: None,
            data: Default::default(),
            hashed_content: Default::default(),
            cont: false,
            cont_count: None,
            more_packets: false,
            checksum: None,
        })
    }
}

impl<'a, T> Packet<T>
where
    T: AsRef<[u8]>,
{
    /// Return the original bytes in the packet.
    pub fn as_bytes(&self) -> &[u8] {
        self.0.packet.as_ref()
    }

    /// Return the packet as a `str`.
    pub fn as_str(&self) -> &str {
        // If the packet was successfully parsed and instantiated, then it is
        // guaranteed to be valid ASCII, and therefore also valid utf8. So
        // unwrapping is safe.
        std::str::from_utf8(self.as_bytes()).unwrap()
    }

    /// Return the kind of packet
    pub fn kind(&self) -> PacketKind {
        self.0.kind
    }

    /// Return the packet's target
    pub fn target(&self) -> Target {
        self.0.target.into()
    }

    /// Return the packet's message ID, if there is one.
    pub fn id(&self) -> Option<u8> {
        self.0.id
    }

    /// Whether or not this packet is a continuation of a previous one.
    pub fn cont(&self) -> bool {
        self.0.cont
    }

    /// If the packet is a continuation packet and it has an associated count, returns the count.
    ///
    /// A packet can be a continuation packet and note have a count.
    pub fn cont_count(&self) -> Option<u8> {
        self.0.cont_count
    }

    /// Return the reply flag, if there is one.
    ///
    /// Guaranteed to be [`Some`] if [`kind`](Packet::kind) is [`PacketKind::Reply`].
    pub fn flag(&self) -> Option<Flag> {
        self.0.flag
    }

    /// Return the status, if there is one.
    ///
    /// Guaranteed to be [`Some`] if [`kind`](Packet::kind) is [`PacketKind::Reply`] or [`PacketKind::Alert`].
    pub fn status(&self) -> Option<Status> {
        self.0.status
    }

    /// Return the warning, if there is one.
    ///
    /// Guaranteed to be [`Some`] if [`kind`](Packet::kind) is [`PacketKind::Reply`] or [`PacketKind::Alert`].
    pub fn warning(&self) -> Option<Warning> {
        self.0.warning
    }

    /// Return the data, which may be empty.
    pub fn data(&self) -> &str {
        // It is OK to unwrap because the Client guarantees the data is valid ASCII.
        std::str::from_utf8(self.0.data.as_ref()).unwrap()
    }

    /// Return the hashed contents of the packet (everything between the packet kind marker and the checksum marker/termination).
    ///
    /// This is only useful for validating checksums.
    pub(crate) fn hashed_content(&self) -> &[u8] {
        self.0.hashed_content.as_ref()
    }

    /// Return whether the packet is the last one in a message.
    pub fn more_packets(&self) -> bool {
        self.0.more_packets
    }

    /// Return the value of the checksum, if there is one.
    pub fn checksum(&self) -> Option<u32> {
        self.0.checksum
    }
}

impl<T> std::fmt::Display for Packet<T>
where
    T: AsRef<[u8]>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(self.as_bytes()))
    }
}

impl<'a, T> Visitor<'a> for Packet<T>
where
    T: From<&'a [u8]>,
{
    type Output = Self;

    fn separator(&mut self, _: &[u8]) {}
    fn kind(&mut self, kind: PacketKind, _: &[u8]) {
        self.0.kind = kind;
    }
    fn device_address(&mut self, address: u8, _: &[u8]) {
        self.0.target.0 = address;
    }
    fn axis_number(&mut self, axis: u8, _: &[u8]) {
        self.0.target.1 = axis;
    }
    fn message_id(&mut self, id: u8, _: &[u8]) {
        self.0.id = Some(id);
    }
    fn flag(&mut self, flag: Flag, _: &[u8]) {
        self.0.flag = Some(flag);
    }
    fn status(&mut self, status: Status, _: &[u8]) {
        self.0.status = Some(status);
    }
    fn warning(&mut self, warning: Warning, _: &[u8]) {
        self.0.warning = Some(warning);
    }
    fn cont(&mut self, _: &[u8]) {
        self.0.cont = true;
    }
    fn cont_count(&mut self, count: u8, _: &[u8]) {
        self.0.cont_count = Some(count);
    }
    fn data_word(&mut self, _: &'a [u8]) {}
    fn data(&mut self, bytes: &'a [u8]) {
        self.0.data = bytes.into();
    }
    fn hashed_content(&mut self, bytes: &'a [u8]) {
        self.0.hashed_content = bytes.into();
    }
    fn more_packets_marker(&mut self, _: &[u8]) {
        self.0.more_packets = true;
    }
    fn checksum_marker(&mut self, _: &[u8]) {}
    fn checksum(&mut self, checksum: u32, _: &[u8]) {
        self.0.checksum = Some(checksum);
    }
    fn termination(&mut self, _: &[u8]) {}

    fn start_visit(&mut self, bytes: &'a [u8]) {
        self.0.packet = bytes.into();
    }
    fn finish_visit(self) -> Self::Output {
        self
    }
}

impl<'a> TryFrom<&'a [u8]> for Packet<Box<[u8]>> {
    type Error = AsciiPacketMalformedError;

    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        let client = Client::new(Packet::new_default(), value);
        client
            .parse()
            .map_err(|_| AsciiPacketMalformedError::new(value))
    }
}

impl<'a> TryFrom<&'a [u8]> for Packet<&'a [u8]> {
    type Error = AsciiPacketMalformedError;

    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        let client = Client::new(Packet::new_default(), value);
        client
            .parse()
            .map_err(|_| AsciiPacketMalformedError::new(value))
    }
}

#[cfg(test)]
mod type_test {
    use super::*;

    #[test]
    fn type_alias() {
        let _: Result<RefPacket<'_>, AsciiPacketMalformedError> = Packet::new_ref(b"");
    }
}
