//! Types and traits for parsing ASCII response messages.

mod alert;
pub mod check;
mod info;
mod reply;
pub use alert::*;
pub use info::*;
pub use reply::*;

use crate::error::*;

use crate::ascii::{parse, Target};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, hex_digit1, space1},
    combinator::{map_res, opt},
    sequence::{preceded, tuple},
};

/// Convert a `nom::error::Error` into a `AsciiProtocolError`
fn protocol_error_from_nom_error(nom_err: nom::error::Error<&[u8]>) -> AsciiProtocolError {
    AsciiPacketMalformedError::new(nom_err.input).into()
}

/// A trait that is implemented by ASCII response messages.
pub trait Response:
    std::fmt::Display
    + std::fmt::Debug
    + std::convert::TryFrom<AnyResponse>
    + std::convert::Into<AnyResponse>
    + private::Sealed
{
    /// Return the message's target (e.g. device address)
    fn target(&self) -> Target;
    /// Return the message's ID, if there is one.
    fn id(&self) -> Option<u8>;
    /// Return the message's checksum, if there is one.
    fn checksum(&self) -> Option<u32>;
    /// Return the message's data.
    fn data(&self) -> &str;
    /// Return a mutable reference the message's data
    #[doc(hidden)]
    fn data_mut(&mut self) -> &mut String;
    /// Return the default check for this response.
    ///
    /// This needs to be defined here so we can get the default check based on
    /// the response type, but users shouldn't access it here. Instead users
    /// should use [`crate::ascii::check::default`]
    #[doc(hidden)]
    fn default_check() -> fn(Self) -> Result<Self, AsciiCheckError<Self>>;
}

/// A marker trait for specific ASCII response types, i.e., not [`AnyResponse`].
pub trait SpecificResponse: Response {}

/// A trait for a response that contains a warning.
pub trait ResponseWithWarning: Response {
    /// Get the response's warning
    fn warning(&self) -> Warning;
}

impl ResponseWithWarning for Reply {
    fn warning(&self) -> Warning {
        self.warning()
    }
}

impl ResponseWithWarning for Alert {
    fn warning(&self) -> Warning {
        self.warning()
    }
}

/// A trait for a response that contains a status.
pub trait ResponseWithStatus: Response {
    /// Get the response's status
    fn status(&self) -> Status;
}

impl ResponseWithStatus for Reply {
    fn status(&self) -> Status {
        self.status()
    }
}

impl ResponseWithStatus for Alert {
    fn status(&self) -> Status {
        self.status()
    }
}

/// A trait for a response that contains a reply flag.
pub trait ResponseWithFlag: Response {
    /// Get the response's reply flag
    fn flag(&self) -> Flag;
}

impl ResponseWithFlag for Reply {
    fn flag(&self) -> Flag {
        self.flag()
    }
}

/// The device or axis status.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Status {
    /// The device or axis is busy performing a motion.
    Busy,
    /// The device or axis is not moving.
    Idle,
}

impl parse::Nom for Status {
    fn nom(input: &[u8]) -> nom::IResult<&[u8], Self> {
        map_res(
            parse::take_till_tab_space_reserved,
            |bytes: &[u8]| match bytes {
                b"IDLE" => Ok(Status::Idle),
                b"BUSY" => Ok(Status::Busy),
                _ => Err(()),
            },
        )(input)
    }
}

impl std::convert::TryFrom<&[u8]> for Status {
    type Error = AsciiProtocolError;
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        use nom::Finish as _;
        <Self as parse::Nom>::nom(bytes)
            .finish()
            .map(|(_, value)| value)
            .map_err(protocol_error_from_nom_error)
    }
}

impl std::convert::TryFrom<&str> for Status {
    type Error = AsciiProtocolError;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::try_from(s.as_bytes())
    }
}

impl std::fmt::Display for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Status::Busy => write!(f, "BUSY"),
            Status::Idle => write!(f, "IDLE"),
        }
    }
}

/// A device or axis warning flag.
///
/// To check what the the warning flag is simply do a comparison (e.g. `warning == "WR"`).
#[derive(Debug, Copy, Clone, Eq, Hash)]
pub struct Warning([u8; 2]);

impl Warning {
    /// The warning value indicating there are no warnings on a device/axis, i.e. `--`.
    pub const NONE: Warning = Warning(*b"--");

    /// Whether there is no warning (i.e., `--`).
    ///
    /// The opposite of `is_some()`.
    pub fn is_none(&self) -> bool {
        *self == Warning::NONE
    }
    /// Whether there is a warning or not.
    pub fn is_some(&self) -> bool {
        !self.is_none()
    }
    /// Whether the warning is a fault, i.e., `F*`.
    pub fn is_fault(&self) -> bool {
        self.0[0] == b'F'
    }
    /// Whether the warning is a warning, i.e., `W*`.
    pub fn is_warning(&self) -> bool {
        self.0[0] == b'W'
    }
    /// Whether the warning is a notice, i.e., `N*`.
    pub fn is_notice(&self) -> bool {
        self.0[0] == b'N'
    }
}

impl AsRef<[u8]> for Warning {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl parse::Nom for Warning {
    fn nom(input: &[u8]) -> nom::IResult<&[u8], Self> {
        map_res(parse::take_till_tab_space_reserved, |bytes: &[u8]| {
            use std::convert::TryInto as _;

            if bytes.len() == 2 {
                // It is safe to unwrap here because we have guaranteed
                // above that the slice size matches the array.
                Ok(Warning(bytes.try_into().unwrap()))
            } else {
                Err(())
            }
        })(input)
    }
}

impl std::convert::From<[u8; 2]> for Warning {
    fn from(bytes: [u8; 2]) -> Self {
        Warning(bytes)
    }
}

impl std::convert::From<&[u8; 2]> for Warning {
    fn from(bytes: &[u8; 2]) -> Self {
        Warning(*bytes)
    }
}

impl std::convert::TryFrom<&[u8]> for Warning {
    type Error = AsciiProtocolError;
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        use nom::Finish as _;
        <Self as parse::Nom>::nom(bytes)
            .finish()
            .map(|(_, value)| value)
            .map_err(protocol_error_from_nom_error)
    }
}

impl std::convert::TryFrom<&str> for Warning {
    type Error = AsciiProtocolError;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::try_from(s.as_bytes())
    }
}

impl std::fmt::Display for Warning {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", std::str::from_utf8(&self.0).unwrap())
    }
}

impl PartialEq<[u8]> for Warning {
    fn eq(&self, other: &[u8]) -> bool {
        &self.0[..] == other
    }
}

impl<T> PartialEq<T> for Warning
where
    T: AsRef<[u8]>,
{
    fn eq(&self, other: &T) -> bool {
        &self.0[..] == other.as_ref()
    }
}

/// A Zaber ASCII message header.
#[derive(Debug, Clone, PartialEq)]
struct Header {
    /// The address of the device sending the message
    pub address: u8,
    /// The axis number for which the message is relevant
    pub axis: u8,
    /// The optional message ID
    pub id: Option<u8>,
}

impl parse::Nom for Header {
    fn nom(input: &[u8]) -> nom::IResult<&[u8], Self> {
        map_res(
            tuple((
                // The address
                map_res(digit1, parse::u8_from_base_10),
                // The axis
                map_res(preceded(space1, digit1), parse::u8_from_base_10),
                // The optional
                opt(map_res(preceded(space1, digit1), parse::u8_from_base_10)),
            )),
            |(address, axis, id)| -> Result<Header, ()> { Ok(Header { address, axis, id }) },
        )(input)
    }
}

impl std::convert::TryFrom<&[u8]> for Header {
    type Error = AsciiProtocolError;
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        use nom::Finish as _;
        <Self as parse::Nom>::nom(bytes)
            .finish()
            .map(|(_, value)| value)
            .map_err(protocol_error_from_nom_error)
    }
}

impl std::convert::TryFrom<&str> for Header {
    type Error = AsciiProtocolError;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::try_from(s.as_bytes())
    }
}

impl std::fmt::Display for Header {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:02} {}", self.address, self.axis)?;
        if let Some(id) = self.id {
            write!(f, " {:02}", id)?;
        }
        Ok(())
    }
}

/// A Zaber ASCII message footer.
#[derive(Debug, Clone, PartialEq)]
struct Footer {
    /// The optional message checksum.
    pub checksum: Option<u32>,
}

impl parse::Nom for Footer {
    fn nom(input: &[u8]) -> nom::IResult<&[u8], Self> {
        map_res(
            opt(preceded(tag(&[parse::CHECKSUM_MARKER]), hex_digit1)),
            |bytes: Option<&[u8]>| -> Result<Footer, std::num::ParseIntError> {
                match bytes {
                    Some(bytes) => Ok(Footer {
                        checksum: {
                            let s = std::str::from_utf8(bytes).unwrap_or("");
                            let checksum = u32::from_str_radix(s, 16)?;
                            Some(checksum)
                        },
                    }),
                    None => Ok(Footer { checksum: None }),
                }
            },
        )(input)
    }
}

impl std::convert::TryFrom<&[u8]> for Footer {
    type Error = AsciiProtocolError;
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        use nom::Finish as _;
        <Self as parse::Nom>::nom(bytes)
            .finish()
            .map(|(_, value)| value)
            .map_err(protocol_error_from_nom_error)
    }
}

impl std::convert::TryFrom<&str> for Footer {
    type Error = AsciiProtocolError;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::try_from(s.as_bytes())
    }
}

impl std::fmt::Display for Footer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(checksum) = self.checksum {
            write!(f, ":{:02X}", checksum)?;
        }
        Ok(())
    }
}

/// Any Zaber ASCII response.
#[derive(Debug, Clone, PartialEq)]
pub enum AnyResponse {
    /// A reply message
    Reply(Reply),
    /// An info message
    Info(Info),
    /// An alert message
    Alert(Alert),
}

impl AnyResponse {
    /// The kind of Zaber ASCII response.
    pub fn kind(&self) -> Kind {
        match self {
            AnyResponse::Reply(..) => Kind::Reply,
            AnyResponse::Info(..) => Kind::Info,
            AnyResponse::Alert(..) => Kind::Alert,
        }
    }

    /// The response's data.
    pub fn data(&self) -> &str {
        match self {
            AnyResponse::Reply(reply) => reply.data(),
            AnyResponse::Info(info) => info.data(),
            AnyResponse::Alert(alert) => alert.data(),
        }
    }

    /// The status of the response.
    ///
    /// Not all response's have a status.
    pub fn status(&self) -> Option<Status> {
        match self {
            AnyResponse::Reply(reply) => Some(reply.status()),
            AnyResponse::Info(..) => None,
            AnyResponse::Alert(alert) => Some(alert.status()),
        }
    }
}

impl std::convert::From<std::convert::Infallible> for AnyResponse {
    fn from(_: std::convert::Infallible) -> Self {
        unreachable!();
    }
}

impl std::convert::From<Reply> for AnyResponse {
    fn from(item: Reply) -> Self {
        AnyResponse::Reply(item)
    }
}

impl std::convert::From<Info> for AnyResponse {
    fn from(item: Info) -> Self {
        AnyResponse::Info(item)
    }
}

impl std::convert::From<Alert> for AnyResponse {
    fn from(item: Alert) -> Self {
        AnyResponse::Alert(item)
    }
}

impl parse::Nom for Packet<AnyResponse> {
    fn nom(input: &[u8]) -> nom::IResult<&[u8], Self> {
        alt((
            // Try to parse the most common responses first to avoid doing extra work
            map_res(Packet::<Reply>::nom, Packet::<AnyResponse>::try_from),
            map_res(Packet::<Info>::nom, Packet::<AnyResponse>::try_from),
            map_res(Packet::<Alert>::nom, Packet::<AnyResponse>::try_from),
        ))(input)
    }
}

impl std::convert::TryFrom<&[u8]> for Packet<AnyResponse> {
    type Error = AsciiProtocolError;
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        use nom::Finish as _;
        <Self as parse::Nom>::nom(bytes)
            .finish()
            .map(|(_, value)| value)
            .map_err(protocol_error_from_nom_error)
    }
}

impl std::convert::TryFrom<&str> for Packet<AnyResponse> {
    type Error = AsciiProtocolError;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::try_from(s.as_bytes())
    }
}

impl std::fmt::Display for AnyResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AnyResponse::Reply(reply) => reply.fmt(f),
            AnyResponse::Info(info) => info.fmt(f),
            AnyResponse::Alert(alert) => alert.fmt(f),
        }
    }
}

impl Response for AnyResponse {
    fn target(&self) -> Target {
        match self {
            AnyResponse::Reply(reply) => reply.target(),
            AnyResponse::Info(info) => info.target(),
            AnyResponse::Alert(alert) => alert.target(),
        }
    }
    fn id(&self) -> Option<u8> {
        match self {
            AnyResponse::Reply(reply) => reply.id(),
            AnyResponse::Info(info) => info.id(),
            AnyResponse::Alert(alert) => alert.id(),
        }
    }
    fn checksum(&self) -> Option<u32> {
        match self {
            AnyResponse::Reply(reply) => reply.checksum(),
            AnyResponse::Info(info) => info.checksum(),
            AnyResponse::Alert(alert) => alert.checksum(),
        }
    }
    fn data(&self) -> &str {
        match self {
            AnyResponse::Reply(reply) => reply.data(),
            AnyResponse::Info(info) => info.data(),
            AnyResponse::Alert(alert) => alert.data(),
        }
    }
    #[doc(hidden)]
    fn data_mut(&mut self) -> &mut String {
        match self {
            AnyResponse::Reply(reply) => reply.data_mut(),
            AnyResponse::Info(info) => info.data_mut(),
            AnyResponse::Alert(alert) => alert.data_mut(),
        }
    }
    #[doc(hidden)]
    fn default_check() -> fn(Self) -> Result<Self, AsciiCheckError<Self>> {
        |response| match response {
            AnyResponse::Reply(reply) => Reply::default_check()(reply)
                .map(From::from)
                .map_err(From::from),
            AnyResponse::Info(info) => Info::default_check()(info)
                .map(From::from)
                .map_err(From::from),
            AnyResponse::Alert(alert) => Alert::default_check()(alert)
                .map(From::from)
                .map_err(From::from),
        }
    }
}

/// The different kind of responses.
///
/// The Zaber ASCII protocol calls this the "type" of response but "type" is a
/// keyword in Rust so we use "kind" instead.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    /// A reply message.
    Reply,
    /// An info message.
    Info,
    /// An alert message.
    Alert,
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Kind::Reply => write!(f, "Reply"),
            Kind::Info => write!(f, "Info"),
            Kind::Alert => write!(f, "Alert"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Packet<R> {
    /// Whether this packet completes the message.
    ///
    /// If the message is not complete, further content should be read from
    /// subsequent [`Info`] messages until they are complete.
    pub complete: bool,
    /// The response
    pub response: R,
}

impl std::convert::TryFrom<Packet<Reply>> for Packet<AnyResponse> {
    type Error = <AnyResponse as std::convert::TryFrom<Reply>>::Error;

    fn try_from(other: Packet<Reply>) -> Result<Self, Self::Error> {
        Ok(Packet {
            complete: other.complete,
            response: AnyResponse::try_from(other.response)?,
        })
    }
}

impl std::convert::TryFrom<Packet<Info>> for Packet<AnyResponse> {
    type Error = <AnyResponse as std::convert::TryFrom<Info>>::Error;

    fn try_from(other: Packet<Info>) -> Result<Self, Self::Error> {
        Ok(Packet {
            complete: other.complete,
            response: AnyResponse::try_from(other.response)?,
        })
    }
}

impl std::convert::TryFrom<Packet<Alert>> for Packet<AnyResponse> {
    type Error = <AnyResponse as std::convert::TryFrom<Alert>>::Error;

    fn try_from(other: Packet<Alert>) -> Result<Self, Self::Error> {
        Ok(Packet {
            complete: other.complete,
            response: AnyResponse::try_from(other.response)?,
        })
    }
}

mod private {
    use super::{Alert, AnyResponse, Info, Reply};
    pub trait Sealed {}

    impl Sealed for Reply {}
    impl Sealed for Alert {}
    impl Sealed for Info {}
    impl Sealed for AnyResponse {}
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ascii::parse::Nom as _;

    #[test]
    fn test_response_nom() {
        struct TestCase {
            input: &'static [u8],
            expect: nom::IResult<&'static [u8], Packet<AnyResponse>>,
        }

        let cases = &[
            TestCase {
                input: b"@01 1 OK IDLE -- 0\r\nextra ",
                expect: Ok((
                    b"extra ",
                    Packet {
                        complete: true,
                        response: AnyResponse::Reply(
                            ReplyInner {
                                target: Target(1, 1),
                                id: None,
                                flag: Flag::Ok,
                                status: Status::Idle,
                                warning: Warning([b'-', b'-']),
                                data: "0".to_string(),
                                checksum: None,
                            }
                            .into(),
                        ),
                    },
                )),
            },
            TestCase {
                input: b"@01 1 OK IDLE FF \r\n",
                expect: Ok((
                    b"",
                    Packet {
                        complete: true,
                        response: AnyResponse::Reply(
                            ReplyInner {
                                target: Target(1, 1),
                                id: None,
                                flag: Flag::Ok,
                                status: Status::Idle,
                                warning: Warning([b'F', b'F']),
                                data: "".to_string(),
                                checksum: None,
                            }
                            .into(),
                        ),
                    },
                )),
            },
            TestCase {
                input: b"@01 1 OK IDLE -- 0\\:AB\r\n",
                expect: Ok((
                    b"",
                    Packet {
                        complete: false,
                        response: AnyResponse::Reply(
                            ReplyInner {
                                target: Target(1, 1),
                                id: None,
                                flag: Flag::Ok,
                                status: Status::Idle,
                                warning: Warning([b'-', b'-']),
                                data: "0".to_string(),
                                checksum: Some(171),
                            }
                            .into(),
                        ),
                    },
                )),
            },
            TestCase {
                input: b"@01 1 12 OK IDLE -- 0\\:AB\r\n",
                expect: Ok((
                    b"",
                    Packet {
                        complete: false,
                        response: AnyResponse::Reply(
                            ReplyInner {
                                target: Target(1, 1),
                                id: Some(12),
                                flag: Flag::Ok,
                                status: Status::Idle,
                                warning: Warning([b'-', b'-']),
                                data: "0".to_string(),
                                checksum: Some(171),
                            }
                            .into(),
                        ),
                    },
                )),
            },
            TestCase {
                input: b"@01 OK IDLE -- 0\r\n", // Missing axis number
                expect: Err(nom::Err::Error(nom::error::Error::new(
                    b"@01 OK IDLE -- 0\r\n",
                    nom::error::ErrorKind::Tag,
                ))),
            },
            TestCase {
                input: b"@01 2 ok IDLE -- 0\r\n", // Invalid flag
                expect: Err(nom::Err::Error(nom::error::Error::new(
                    b"@01 2 ok IDLE -- 0\r\n",
                    nom::error::ErrorKind::Tag,
                ))),
            },
            TestCase {
                input: b"@01 2 OK 123 -- 0\r\n", // Invalid Status
                expect: Err(nom::Err::Error(nom::error::Error::new(
                    b"@01 2 OK 123 -- 0\r\n",
                    nom::error::ErrorKind::Tag,
                ))),
            },
            TestCase {
                input: b"#01 1 the body of the info message\r\n",
                expect: Ok((
                    b"",
                    Packet {
                        complete: true,
                        response: AnyResponse::Info(
                            InfoInner {
                                target: Target(1, 1),
                                id: None,
                                data: "the body of the info message".to_string(),
                                checksum: None,
                            }
                            .into(),
                        ),
                    },
                )),
            },
            TestCase {
                input: b"#01 1 34 the body of the info message\\:12\r\n",
                expect: Ok((
                    b"",
                    Packet {
                        complete: false,
                        response: AnyResponse::Info(
                            InfoInner {
                                target: Target(1, 1),
                                id: Some(34),
                                data: "the body of the info message".to_string(),
                                checksum: Some(18),
                            }
                            .into(),
                        ),
                    },
                )),
            },
            TestCase {
                input: b"!01 1 BUSY -- something else:56\r\n",
                expect: Ok((
                    b"",
                    Packet {
                        complete: true,
                        response: AnyResponse::Alert(
                            AlertInner {
                                target: Target(1, 1),
                                status: Status::Busy,
                                warning: Warning([b'-', b'-']),
                                data: "something else".to_string(),
                                checksum: Some(86),
                            }
                            .into(),
                        ),
                    },
                )),
            },
            TestCase {
                input: b"!01 1 BUSY WR\r\n",
                expect: Ok((
                    b"",
                    Packet {
                        complete: true,
                        response: AnyResponse::Alert(
                            AlertInner {
                                target: Target(1, 1),
                                status: Status::Busy,
                                warning: Warning([b'W', b'R']),
                                data: "".to_string(),
                                checksum: None,
                            }
                            .into(),
                        ),
                    },
                )),
            },
        ];
        for (i, case) in cases.into_iter().enumerate() {
            let response = Packet::<AnyResponse>::nom(case.input);
            assert_eq!(
                response,
                case.expect,
                "Case {}: `{}`",
                i,
                std::str::from_utf8(case.input).unwrap()
            );
        }
    }

    #[test]
    fn test_warning_partial_eq() {
        let warning = Warning([b'-', b'-']);
        assert_eq!(warning, b"--");
        assert_eq!(warning, "--");
        assert_eq!(warning, b"--".to_vec());
        assert_eq!(warning, "--".to_string());
    }
}
