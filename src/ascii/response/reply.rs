//! The ASCII reply message type.

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

/// A reply flag.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Flag {
    /// The `OK` flag, indicating the previous command was accepted.
    Ok,
    /// The `RJ` flag, indicating the previous command was rejected.
    Rj,
}

impl parse::Nom for Flag {
    fn nom(input: &[u8]) -> nom::IResult<&[u8], Self> {
        map_res(
            parse::take_till_tab_space_reserved,
            |bytes: &[u8]| match bytes {
                b"OK" => Ok(Flag::Ok),
                b"RJ" => Ok(Flag::Rj),
                _ => Err(()),
            },
        )(input)
    }
}

impl std::convert::TryFrom<&[u8]> for Flag {
    type Error = AsciiProtocolError;
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        use nom::Finish as _;
        <Self as parse::Nom>::nom(bytes)
            .finish()
            .map(|(_, value)| value)
            .map_err(protocol_error_from_nom_error)
    }
}

impl std::convert::TryFrom<&str> for Flag {
    type Error = AsciiProtocolError;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::try_from(s.as_bytes())
    }
}

impl std::fmt::Display for Flag {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Flag::Ok => write!(f, "OK"),
            Flag::Rj => write!(f, "RJ"),
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
    pub checksum: Option<u32>,
}

/// A decoded ASCII Reply message.
#[derive(Debug, Clone, PartialEq)]
pub struct Reply(Box<ReplyInner>);

impl Reply {
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
    /// The message's checksum, if any.
    pub fn checksum(&self) -> Option<u32> {
        self.0.checksum
    }
}

impl From<ReplyInner> for Reply {
    fn from(inner: ReplyInner) -> Self {
        Reply(Box::new(inner))
    }
}

impl parse::Nom for Packet<Reply> {
    fn nom(input: &[u8]) -> nom::IResult<&[u8], Self> {
        map_res(
            delimited(
                 tag(&[parse::REPLY_MARKER]),
                tuple((
                    Header::nom,
                    preceded(space1, Flag::nom),
                    preceded(space1, Status::nom),
                    preceded(space1, Warning::nom),
                    map_res(
                        preceded(space1, parse::take_till_reserved),
                        |bytes: &[u8]| -> Result<String, std::str::Utf8Error> {Ok(std::str::from_utf8(bytes)?.to_string())},
                    ),
                    opt(tag(&[parse::MORE_PACKETS_MARKER])),
                    Footer::nom,
                )),
                line_ending,
            ),
            |(header, flag, status, warning, data, continuation, footer)| -> Result<Packet<Reply>, std::convert::Infallible>{
                Ok(Packet {
					complete: continuation.is_none(),
					response: ReplyInner {
						target: Target(header.address, header.axis),
						id: header.id,
						flag,
						status,
						warning,
						data,
						checksum: footer.checksum,
					}.into()
				})
            },
        )(input)
    }
}

impl std::convert::TryFrom<&[u8]> for Packet<Reply> {
    type Error = AsciiProtocolError;
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        use nom::Finish as _;
        <Self as parse::Nom>::nom(bytes)
            .finish()
            .map(|(_, value)| value)
            .map_err(protocol_error_from_nom_error)
    }
}

impl std::convert::TryFrom<&str> for Packet<Reply> {
    type Error = AsciiProtocolError;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::try_from(s.as_bytes())
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
            address: self.target().get_device(),
            axis: self.target().get_axis(),
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
        Footer {
            checksum: self.checksum(),
        }
        .fmt(f)
    }
}

impl Response for Reply {
    fn target(&self) -> Target {
        self.target()
    }
    fn id(&self) -> Option<u8> {
        self.id()
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
        |reply| {
            if reply.flag() != Flag::Ok {
                Err(AsciiCheckFlagError::new(Flag::Ok, reply).into())
            } else if reply.warning() != Warning::NONE {
                Err(AsciiCheckWarningError::new(
                    format!("expected {} warning flag", Warning::NONE),
                    reply,
                )
                .into())
            } else {
                Ok(reply)
            }
        }
    }
}

impl SpecificResponse for Reply {}
