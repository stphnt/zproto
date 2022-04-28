//! Types and traits for parsing ASCII response messages.

mod alert;
mod builder;
pub mod check;
mod info;
mod reply;
pub use alert::*;
pub(crate) use builder::ResponseBuilder;
pub use info::*;
pub use reply::*;

use crate::error::*;

use crate::ascii::{parse, Target};

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

impl<'a> std::convert::TryFrom<&'a [u8]> for Warning {
    type Error = &'a [u8];

    fn try_from(bytes: &'a [u8]) -> Result<Self, Self::Error> {
        use std::convert::TryInto as _;

        if bytes.len() == 2 {
            // It is safe to unwrap here because we have guaranteed that the
            // slice size matches the array size.
            Ok(Warning(bytes.try_into().unwrap()))
        } else {
            Err(bytes)
        }
    }
}

impl<'a> std::convert::TryFrom<&'a str> for Warning {
    type Error = &'a str;
    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        TryFrom::<&'a [u8]>::try_from(s.as_bytes()).map_err(|_| s)
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

impl std::fmt::Display for Header {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:02} {}", self.address, self.axis)?;
        if let Some(id) = self.id {
            write!(f, " {:02}", id)?;
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
    /// Try to convert a packet into an AnyResponse message.
    ///
    /// The conversion will fail if the packet is the wrong kind or if the packet
    /// is not the start of a message. The packet does not need to complete the
    /// message.
    pub(crate) fn try_from_packet<T>(packet: &parse::Packet<T>) -> Result<Self, &parse::Packet<T>>
    where
        T: AsRef<[u8]>,
    {
        Reply::try_from_packet(packet)
            .map(From::from)
            .or_else(|packet| Info::try_from_packet(packet).map(From::from))
            .or_else(|packet| Alert::try_from_packet(packet).map(From::from))
    }

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
