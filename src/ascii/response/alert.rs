//! The ASCII Alert message type.

use crate::ascii::{
    response::{
        parse, protocol_error_from_nom_error, AnyResponse, Footer, Header, Packet, Response,
        SpecificResponse, Status, Warning,
    },
    Target,
};
use crate::error::*;
use nom::{
    bytes::complete::tag,
    character::complete::{line_ending, space1},
    combinator::{map_res, opt},
    sequence::{delimited, preceded, tuple},
};

/// The contents of an [`Alert`] message
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AlertInner {
    pub target: Target,
    pub status: Status,
    pub warning: Warning,
    pub data: String,
    pub checksum: Option<u32>,
}

/// A decoded Zaber ASCII Alert message.
#[derive(Debug, Clone, PartialEq)]
pub struct Alert(Box<AlertInner>);

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
            checksum: packet.checksum(),
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
    /// The message's checksum, if any.
    pub fn checksum(&self) -> Option<u32> {
        self.0.checksum
    }
}

impl From<AlertInner> for Alert {
    fn from(inner: AlertInner) -> Self {
        Alert(Box::new(inner))
    }
}

impl parse::Nom for Packet<Alert> {
    fn nom(input: &[u8]) -> nom::IResult<&[u8], Self> {
        map_res(
            delimited(
                tag(&[parse::ALERT_MARKER]),
                tuple((
                    Header::nom,
                    preceded(space1, Status::nom),
                    preceded(space1, Warning::nom),
                    opt(preceded(
                        space1,
                        map_res(
                            parse::take_till_reserved,
                            |bytes: &[u8]| -> Result<&str, std::str::Utf8Error> {
                                std::str::from_utf8(bytes)
                            },
                        ),
                    )),
                    Footer::nom,
                )),
                line_ending,
            ),
            |(header, status, warning, data, footer): (
                Header,
                Status,
                Warning,
                Option<&str>,
                Footer,
            )|
             -> Result<Packet<Alert>, std::convert::Infallible> {
                Ok(Packet {
                    complete: true,
                    response: AlertInner {
                        target: Target(header.address, header.axis),
                        status,
                        warning,
                        data: data.unwrap_or("").to_string(),
                        checksum: footer.checksum,
                    }
                    .into(),
                })
            },
        )(input)
    }
}

impl std::convert::TryFrom<&[u8]> for Packet<Alert> {
    type Error = AsciiProtocolError;
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        use nom::Finish as _;
        <Self as parse::Nom>::nom(bytes)
            .finish()
            .map(|(_, value)| value)
            .map_err(protocol_error_from_nom_error)
    }
}

impl std::convert::TryFrom<&str> for Packet<Alert> {
    type Error = AsciiProtocolError;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::try_from(s.as_bytes())
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
            address: self.target().get_device(),
            axis: self.target().get_axis(),
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
        Footer {
            checksum: self.checksum(),
        }
        .fmt(f)
    }
}

impl Response for Alert {
    fn target(&self) -> Target {
        self.target()
    }
    fn id(&self) -> Option<u8> {
        None
    }
    fn checksum(&self) -> Option<u32> {
        self.checksum()
    }
    fn data(&self) -> &str {
        self.data()
    }
    #[doc(hidden)]
    fn data_mut(&mut self) -> &mut String {
        &mut self.0.data
    }
    // If this logic changes update the documentation for `ascii::check::default`
    #[doc(hidden)]
    fn default_check() -> fn(Self) -> Result<Self, AsciiCheckError<Self>> {
        |alert| {
            if alert.warning() == Warning::NONE {
                Ok(alert)
            } else {
                Err(AsciiCheckWarningError::new(
                    format!("expected {} warning flag", Warning::NONE),
                    alert,
                )
                .into())
            }
        }
    }
}

impl SpecificResponse for Alert {}
