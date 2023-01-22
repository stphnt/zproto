//! The ASCII Alert message type.

use crate::ascii::{
    response::{parse, AnyResponse, Header, Response, SpecificResponse, Status, Warning},
    Target,
};

/// The contents of an [`Alert`] message
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AlertInner {
    pub target: Target,
    pub status: Status,
    pub warning: Warning,
    pub data: String,
}

/// A decoded Zaber ASCII Alert message.
#[derive(Debug, Clone, PartialEq)]
pub struct Alert(pub(super) Box<AlertInner>);

impl Alert {
    /// Try to convert a packet into an Alert message.
    ///
    /// The conversion will fail if the packet is the wrong kind or if the packet
    /// is not the start of a message. The packet does not need to complete the
    /// message.
    pub(crate) fn try_from_packet<T>(packet: &parse::Packet<T>) -> Result<Self, &parse::Packet<T>>
    where
        T: AsRef<[u8]>,
    {
        if packet.kind() != parse::PacketKind::Alert || packet.cont() {
            return Err(packet);
        }
        Ok(AlertInner {
            target: packet.target(),
            status: packet.status().ok_or(packet)?,
            warning: packet.warning().ok_or(packet)?,
            data: packet.data().to_string(),
        }
        .into())
    }

    /// The device and axis number the Alert came from.
    pub fn target(&self) -> Target {
        self.0.target
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

impl From<AlertInner> for Alert {
    fn from(inner: AlertInner) -> Self {
        Alert(Box::new(inner))
    }
}

impl std::convert::TryFrom<AnyResponse> for Alert {
    type Error = AnyResponse;
    fn try_from(response: AnyResponse) -> Result<Self, Self::Error> {
        if let AnyResponse::Alert(alert) = response {
            Ok(alert)
        } else {
            Err(response)
        }
    }
}

impl std::fmt::Display for Alert {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", char::from(parse::ALERT_MARKER))?;
        Header {
            address: self.target().device(),
            axis: self.target().axis(),
            id: None,
        }
        .fmt(f)?;
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

impl Response for Alert {
    fn target(&self) -> Target {
        self.target()
    }
    fn id(&self) -> Option<u8> {
        None
    }
    fn data(&self) -> &str {
        self.data()
    }
}

impl SpecificResponse for Alert {}
