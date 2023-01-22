//! The ASCII reply message type.

use crate::ascii::{
    response::{parse, AnyResponse, Header, Response, SpecificResponse, Status, Warning},
    Target,
};

/// A reply flag.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Flag {
    /// The `OK` flag, indicating the previous command was accepted.
    Ok,
    /// The `RJ` flag, indicating the previous command was rejected.
    Rj,
}

impl Flag {
    pub(crate) const OK_STR: &'static str = "OK";
    pub(crate) const RJ_STR: &'static str = "RJ";
}

impl std::fmt::Display for Flag {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Flag::Ok => write!(f, "{}", Flag::OK_STR),
            Flag::Rj => write!(f, "{}", Flag::RJ_STR),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ReplyInner {
    pub target: Target,
    pub id: Option<u8>,
    pub flag: Flag,
    pub status: Status,
    pub warning: Warning,
    pub data: String,
}

/// A decoded ASCII Reply message.
#[derive(Debug, Clone, PartialEq)]
pub struct Reply(pub(super) Box<ReplyInner>);

impl Reply {
    /// Try to convert a packet into an Reply message.
    ///
    /// The conversion will fail if the packet is the wrong kind or if the packet
    /// is not the start of a message. The packet does not need to complete the
    /// message.
    pub(crate) fn try_from_packet<T>(packet: &parse::Packet<T>) -> Result<Self, &parse::Packet<T>>
    where
        T: AsRef<[u8]>,
    {
        if packet.kind() != parse::PacketKind::Reply || packet.cont() {
            return Err(packet);
        }
        Ok(ReplyInner {
            target: packet.target(),
            id: packet.id(),
            flag: packet.flag().ok_or(packet)?,
            status: packet.status().ok_or(packet)?,
            warning: packet.warning().ok_or(packet)?,
            data: packet.data().to_string(),
        }
        .into())
    }
    /// The device and axis number the Reply came from.
    pub fn target(&self) -> Target {
        self.0.target
    }
    /// The message ID, if any.
    pub fn id(&self) -> Option<u8> {
        self.0.id
    }
    /// The reply flag.
    pub fn flag(&self) -> Flag {
        self.0.flag
    }
    /// The device's or axis's status.
    pub fn status(&self) -> Status {
        self.0.status
    }
    /// The highest priority warning on the device or axis.
    pub fn warning(&self) -> Warning {
        self.0.warning
    }
    /// The message's data.
    pub fn data(&self) -> &str {
        self.0.data.as_str()
    }
}

impl From<ReplyInner> for Reply {
    fn from(inner: ReplyInner) -> Self {
        Reply(Box::new(inner))
    }
}

impl std::convert::TryFrom<AnyResponse> for Reply {
    type Error = AnyResponse;
    fn try_from(response: AnyResponse) -> Result<Self, Self::Error> {
        if let AnyResponse::Reply(reply) = response {
            Ok(reply)
        } else {
            Err(response)
        }
    }
}

impl std::fmt::Display for Reply {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", char::from(parse::REPLY_MARKER))?;
        Header {
            address: self.target().device(),
            axis: self.target().axis(),
            id: self.id(),
        }
        .fmt(f)?;
        write!(f, " ")?;
        self.flag().fmt(f)?;
        write!(f, " ")?;
        self.status().fmt(f)?;
        write!(f, " ")?;
        self.warning().fmt(f)?;
        if !self.data().is_empty() {
            write!(f, " {}", self.data())?;
        }
        Ok(())
    }
}

impl Response for Reply {
    fn target(&self) -> Target {
        self.target()
    }
    fn id(&self) -> Option<u8> {
        self.id()
    }
    fn data(&self) -> &str {
        self.data()
    }
}

impl SpecificResponse for Reply {}
