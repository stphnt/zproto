//! The ASCII info message type.

use crate::ascii::{
    response::{
        parse, protocol_error_from_nom_error, AnyResponse, Footer, Header, Packet, Response,
        SpecificResponse,
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

/// The contents of an [`Info`] message.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct InfoInner {
    pub target: Target,
    pub id: Option<u8>,
    pub data: String,
    pub checksum: Option<u32>,
}

/// A decoded Zaber ASCII Info message.
#[derive(Debug, Clone, PartialEq)]
pub struct Info(Box<InfoInner>);

impl Info {
    /// The device and axis number the Info message came from.
    pub fn target(&self) -> Target {
        self.0.target
    }
    /// The message ID, if any.
    pub fn id(&self) -> Option<u8> {
        self.0.id
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

impl From<InfoInner> for Info {
    fn from(inner: InfoInner) -> Self {
        Info(Box::new(inner))
    }
}

impl parse::Nom for Packet<Info> {
    fn nom(input: &[u8]) -> nom::IResult<&[u8], Self> {
        map_res(
            delimited(
                tag(&[parse::INFO_MARKER]),
                tuple((
                    Header::nom,
                    map_res(
                        preceded(space1, parse::take_till_reserved),
                        |bytes: &[u8]| -> Result<String, std::str::Utf8Error> {
                            Ok(std::str::from_utf8(bytes)?.to_string())
                        },
                    ),
                    opt(tag(&[parse::CONTINUATION_MARKER])),
                    Footer::nom,
                )),
                line_ending,
            ),
            |(header, data, continuation, footer)| -> Result<Packet<Info>, std::convert::Infallible> {
                Ok(Packet {
                    complete: continuation.is_none(),
					response: InfoInner {
						target: Target(header.address, header.axis),
						id: header.id,
						data,
						checksum: footer.checksum,
					}.into()
				})
            },
        )(input)
    }
}

impl std::convert::TryFrom<&[u8]> for Packet<Info> {
    type Error = AsciiProtocolError;
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        use nom::Finish as _;
        <Self as parse::Nom>::nom(bytes)
            .finish()
            .map(|(_, value)| value)
            .map_err(protocol_error_from_nom_error)
    }
}

impl std::convert::TryFrom<&str> for Packet<Info> {
    type Error = AsciiProtocolError;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::try_from(s.as_bytes())
    }
}

impl std::convert::TryFrom<AnyResponse> for Info {
    type Error = AnyResponse;
    fn try_from(response: AnyResponse) -> Result<Self, Self::Error> {
        if let AnyResponse::Info(info) = response {
            Ok(info)
        } else {
            Err(response)
        }
    }
}

impl std::fmt::Display for Info {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", char::from(parse::INFO_MARKER))?;
        Header {
            address: self.target().get_device(),
            axis: self.target().get_axis(),
            id: self.id(),
        }
        .fmt(f)?;
        if !self.data().is_empty() {
            write!(f, " {}", self.data())?;
        }
        Footer {
            checksum: self.checksum(),
        }
        .fmt(f)
    }
}

impl Response for Info {
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
        Ok
    }
}

impl SpecificResponse for Info {}
