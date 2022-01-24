//! Error types.
//!
//! Each error is represented by a unique type that implements [`std::error::Error`].
//! However, most APIs return more than one kind of error and so will return one
//! of the higher level [enums](#enums), such as [`AsciiError`], [`BinaryError`],
//! or [`Error`]. Where appropriate, the error types are convertable to the
//! higher level enums, allowing them to be used with `?`:
//!
//! ```
//! use zaber_protocol::error::{AsciiUnexpectedError, Error};
//!
//! fn foo() -> Result<(), AsciiUnexpectedError> {
//!     // ...
//! # unimplemented!();
//! }
//!
//! fn bar() -> Result<(), Error> {
//!     foo()?;
//!     // ...
//! # Ok(())
//! }
//! ```
//!
//! For error types that indicate errors with a response, use [`AsRef`] and
//! [`From`]/[`Into`] to retrieve the offending response.
//!
//! ```
//! # use zaber_protocol::{ascii, error::AsciiUnexpectedError};
//! #
//! # fn wrapper() {
//! let error: AsciiUnexpectedError = //...
//! # todo!();
//! let response: &ascii::AnyResponse = error.as_ref();
//! // OR
//! let response: ascii::AnyResponse = error.into();
//! # }
//! ```

use crate::ascii::{Alert, AnyResponse, Flag, Info, Reply, Response, SpecificResponse, Status};
use crate::binary::DeviceMessage;

/// Implement Error and Display traits for the specified type.
///
/// If type is generic, define the trait bounds before the type as you normally
/// would, but omitting the impl keyword (for brevity). After the type define
/// the format string and any arguments it should reference after `self =>` (to
/// abide by macro hygiene rules).
macro_rules! impl_error_display {
    (
        $( <$($t:tt : $bound:path),+> )?
        $name:path,
        $self:ident =>
        $display:literal
        $(,
            $($arg:expr),+
        )?
    ) => {
        impl$(<$($t : $bound),+>)? std::error::Error for $name {}

        impl$(<$($t : $bound),+>)? std::fmt::Display for $name {
            fn fmt(&$self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                writeln!(
                    f,
                    $display
                    $(,
                        $($arg),+
                    )?
                )
            }
        }
    };
}

/// Define error enums that contain concrete error types (not other error enums).
///
/// From and TryFrom traits will be implemented for the enum and it's underlying
/// errors. The enum's Display implementation will defer to the underlying errors'
/// Display implementations.
///
/// Simple implementations of From and TryFrom with other error enums can be
/// added by appending a succinct impl block, which assumes that:
///   * it is being implemented for this error enum,
///   * each variant has a single tuple value, and can be converted to the value
///     in this enum with its own From implementation.
///
/// Note, that this has not been implemented for generic enums. If the
/// conversions are more complex, implement From and TryFrom directly.
///
/// ```
/// // This defines the enum and From/TryFrom between ThisError and A and B.
/// #[non_exhaustive]
/// pub enum ThisError {
///     VariantA(A),
///     VariantB(B),
///     // ...
/// }
///
/// // This implements a simple From/TryFrom between ThisError and OtherType.
/// impl From<OtherType> {
///     FromVariantA => VariantA,
///     // ...
/// }
/// ```
///
///
macro_rules! error_enum {
    // The non-generic case
    (
        $(#[$attr:meta])*
        pub enum $name:ident {
            $(
                $variant:ident($inner:path)
            ),+
            $(,)?
        }
        // Additional information for From/TryFrom impl blocks.
        $(
            impl$(<$impl_t:ident $(: $impl_bound:path )? >)? From<$from_t:ident>
            {
                $($from_variant:ident => $to_variant:ident),+
                $(,)?
            }
        )*
    ) => {
        // Define the error enum itself
        $(
            #[$attr]
        )*
        #[allow(missing_docs)]
        pub enum $name {
            $(
                $variant($inner )
            ),+
        }

        impl std::error::Error for $name {}

        // Defer the display to the inner error type
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $(
                        $name::$variant(e) => e.fmt(f)
                    ),+
                }
            }
        }

        // Allow the enum to be convertable from an infallible error
        impl From<std::convert::Infallible> for $name {
            fn from(_: std::convert::Infallible) -> Self {
                unreachable!();
            }
        }

        // Conversions with underlying errors
        $(
            impl From<$inner> for $name {
                fn from(other: $inner) -> Self {
                    $name::$variant(other)
                }
            }

            impl TryFrom<$name> for $inner {
                type Error = $name;
                fn try_from(other: $name) -> Result<Self, Self::Error> {
                    match other {
                        $name::$variant(value) => Ok(value),
                        value => Err(value)
                    }
                }
            }
        )+

        // Conversions from other enum errors
        $(
            impl From<$from_t> for $name {
                fn from(other: $from_t) -> Self {
                    match other {
                        $($from_t::$from_variant(e) => $name::$to_variant(From::from(e))),+
                    }
                }
            }

            impl TryFrom<$name> for $from_t {
                type Error = $name;
                fn try_from(other: $name) -> Result<Self, Self::Error> {
                    match other {
                        $(
                            $name::$to_variant(e) => Ok($from_t::$from_variant(From::from(e)))
                        ),+
                        ,
                        _ => Err(other)
                    }
                }

            }
        )*
    };
    // The generic case
    (
        $(#[$attr:meta])*
        pub enum $name:ident<$t:tt> {
            $(
                $variant:ident($inner:path)
            ),+
            $(,)?
        }
        impl<$type:tt : $bound:tt>
    ) => {
        // Define the error enum itself
        $(
            #[$attr]
        )*
        #[allow(missing_docs)]
        pub enum $name<$t> {
            $(
                $variant($inner)
            ),+
        }

        impl<$type: $bound> std::error::Error for $name<$type> {}

        // Defer the display to the inner error type
        impl<$type: $bound> std::fmt::Display for $name<$type> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $(
                        $name::$variant(e) => e.fmt(f)
                    ),+
                }
            }
        }

        // Allow the enum to be convertable from an infallible error
        impl<$type: $bound> From<std::convert::Infallible> for $name<$type> {
            fn from(_: std::convert::Infallible) -> Self {
                unreachable!();
            }
        }

        // Conversions with underlying errors
        $(
            impl<$type: $bound> From<$inner> for $name<$type> {
                fn from(other: $inner) -> Self {
                    $name::$variant(other)
                }
            }

            impl<$type: $bound> TryFrom<$name<$type>> for $inner {
                type Error = $name<$type>;
                fn try_from(other: $name<$type>) -> Result<Self, Self::Error> {
                    match other {
                        $name::$variant(value) => Ok(value),
                        value => Err(value)
                    }
                }
            }
        )+
    };
}

/// Implement the `new()` and `packet()` methods for `AsciiPacket*` error types.
macro_rules! impl_ascii_packet {
    (
        $name:ident
    ) => {
        impl $name {
            /// Create a instance of the error
            pub(crate) fn new<R: AsRef<[u8]>>(bytes: R) -> Self {
                $name(
                    String::from_utf8_lossy(bytes.as_ref())
                        .into_owned()
                        .into_boxed_str(),
                )
            }

            /// Get the contents of the invalid packet.
            pub fn packet(&self) -> &str {
                &*self.0
            }
        }
    };
}

/// Implement `pub(crate) fn new<R: Into<AnyResponse>>(other: R) -> Self` for the specified type
macro_rules! impl_new_for_into_any_response {
    ($name:ident) => {
        impl $name {
            pub(crate) fn new<R: Into<AnyResponse>>(other: R) -> Self {
                $name(other.into())
            }
        }
    };
}

/// Implement `AsRef<R> for Error` and `From<T> for Error` to give users access
/// to the underlying responses.
macro_rules! impl_traits_to_access_inner_response {
    (
        ($name:path) -> $out:ty { $($path:tt)+ }
    ) => {
        impl_traits_to_access_inner_response!{@as_ref ($name) -> $out { $($path)+ } }
        impl_traits_to_access_inner_response!{@from ($name) -> $out { $($path)+ } }
    };
    (
        @as_ref ($name:path) -> $out:ty { $($path:tt)+ }
    ) => {
        impl AsRef<$out> for $name {
            /// Get access to the response associated with this error.
            fn as_ref(&self) -> &$out {
                &self.$($path)+
            }
        }
    };
    (
        @from ($name:path) -> $out:ty { $($path:tt)+ }
    ) => {
        impl From<$name> for $out {
            /// Consume the error and return the response associated with it.
            fn from(other: $name) -> Self {
                other.$($path)+
            }
        }
    };
    (
        for<R: Response> ($name:ident) -> R { $($path:tt)+ }
    ) => {
        impl<R: Response> AsRef<R> for $name<R> {
            /// Get access to the response associated with this error.
            fn as_ref(&self) -> &R {
                &self.$($path)+
            }
        }

        // Unfortunately we cannot create a blanket impl for `From` for
        // `R: Response` because we cannot implement foreign traits on foreign
        // types (rustc doesn't see that the only types that can implement
        // `Response` are in this crate -- or at least doesn't trust that to
        // always be true).
        impl_traits_to_access_inner_response!{@from ($name<AnyResponse>) -> AnyResponse { $($path)+ } }
        impl_traits_to_access_inner_response!{@from ($name<Reply>) -> Reply { $($path)+ } }
        impl_traits_to_access_inner_response!{@from ($name<Info>) -> Info { $($path)+ } }
        impl_traits_to_access_inner_response!{@from ($name<Alert>) -> Alert { $($path)+ } }
    };
}

/// Implement the conversion from Type<SpecificResponse> to Type<AnyResponse>
/// for error types that can hold either.
macro_rules! impl_from_specific_to_any_response {
    ($name:ident) => {
        impl<R> From<$name<R>> for $name<AnyResponse>
        where
            R: SpecificResponse,
            AnyResponse: From<R>,
        {
            fn from(other: $name<R>) -> Self {
                $name(Box::new((other.0 .0, other.0 .1.into())))
            }
        }
    };
}

macro_rules! impl_binary_error {
    ($name:ident) => {
        impl $name {
            /// Create a new error.
            pub(crate) const fn new(message: DeviceMessage) -> Self {
                $name(message)
            }
        }

        impl AsRef<DeviceMessage> for $name {
            fn as_ref(&self) -> &DeviceMessage {
                &self.0
            }
        }

        impl From<$name> for DeviceMessage {
            fn from(other: $name) -> Self {
                other.0
            }
        }
    };
}

/// The specified device is either disconnected or already in use by another processs.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct SerialDeviceInUseOrDisconnectedError(Box<str>);

impl_error_display! {
    SerialDeviceInUseOrDisconnectedError,
    self =>
    "the specified device is either disconnected or already in use by another process: {}", self.0
}

/// An ASCII packet was missing a start byte.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AsciiPacketMissingStartError(Box<str>);

impl_error_display! {
    AsciiPacketMissingStartError,
    self => "ASCII packet missing a start byte: {}", self.0
}
impl_ascii_packet! { AsciiPacketMissingStartError }

/// An ASCII packet was missing the end byte(s).
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AsciiPacketMissingEndError(Box<str>);

impl_error_display! {
    AsciiPacketMissingEndError,
    self => "ASCII packet missing end byte(s): {}", self.0
}
impl_ascii_packet! { AsciiPacketMissingEndError }

/// An ASCII packet is malformed.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AsciiPacketMalformedError(Box<str>);

impl_error_display! {
    AsciiPacketMalformedError,
    self => "ASCII packet is malformed: {}", self.0
}
impl_ascii_packet! { AsciiPacketMalformedError }

error_enum! {
    /// Received data that did not conform to the ASCII protocol.
    #[derive(Debug, PartialEq, Eq, Hash)]
    #[non_exhaustive]
    pub enum AsciiProtocolError {
        PacketMissingStart(AsciiPacketMissingStartError),
        PacketMissingEnd(AsciiPacketMissingEndError),
        PacketMalformed(AsciiPacketMalformedError),
    }
}

/// A response had an invalid checksum, indicating it was corrupt.
#[derive(Debug, PartialEq)]
pub struct AsciiInvalidChecksumError(AnyResponse);

impl_error_display! {
    AsciiInvalidChecksumError,
    self => "invalid checksum: {}", self.0
}

impl_traits_to_access_inner_response! { (AsciiInvalidChecksumError) -> AnyResponse { 0 } }
impl_new_for_into_any_response! { AsciiInvalidChecksumError }

/// A response came from an unexpected target.
#[derive(Debug, PartialEq)]
pub struct AsciiUnexpectedTargetError(AnyResponse);

impl_error_display! {
    AsciiUnexpectedTargetError,
    self => "unexpected response target: {}", self.0
}

impl_traits_to_access_inner_response! { (AsciiUnexpectedTargetError) -> AnyResponse { 0 } }
impl_new_for_into_any_response! { AsciiUnexpectedTargetError }

/// A response had an unexpected message ID.
#[derive(Debug, PartialEq)]
pub struct AsciiUnexpectedIdError(AnyResponse);

impl_error_display! {
    AsciiUnexpectedIdError,
    self => "unexpected response message ID: {}", self.0
}

impl_traits_to_access_inner_response! { (AsciiUnexpectedIdError) -> AnyResponse { 0 } }
impl_new_for_into_any_response! { AsciiUnexpectedIdError }

/// A response had an unexpected message kind.
#[derive(Debug, PartialEq)]
pub struct AsciiUnexpectedKindError(AnyResponse);

impl_error_display! {
    AsciiUnexpectedKindError,
    self => "unexpected kind of response: {}", self.0
}

impl_traits_to_access_inner_response! { (AsciiUnexpectedKindError) -> AnyResponse { 0 } }
impl_new_for_into_any_response! { AsciiUnexpectedKindError }

/// A response was expected to be a continuation info message but it was not.
#[derive(Debug, PartialEq)]
pub struct AsciiUnexpectedContinuationError(AnyResponse);

impl_error_display! {
    AsciiUnexpectedContinuationError,
    self => "unexpected non-continuation message: {}", self.0
}

impl_traits_to_access_inner_response! { (AsciiUnexpectedContinuationError) -> AnyResponse { 0 } }
impl_new_for_into_any_response! { AsciiUnexpectedContinuationError }

/// A [`Reply`] was received with an unexpected reply flag.
#[derive(Debug, PartialEq)]
pub struct AsciiCheckFlagError(Box<(Flag, Reply)>);

impl_error_display! {
    AsciiCheckFlagError,
    self => "expected reply flag {}: {}", self.0.0, self.0.1
}

impl AsciiCheckFlagError {
    /// Create a new `AsciiCheckFlagError` error.
    ///
    /// `expected` is the expected [`Flag`] and `reply` is the invalid response.
    pub fn new(expected: Flag, reply: Reply) -> Self {
        AsciiCheckFlagError(Box::new((expected, reply)))
    }
}

impl_traits_to_access_inner_response! { (AsciiCheckFlagError) -> Reply { 0.1 } }

/// A response of type `R` was received with an unexpected status.
#[derive(Debug, PartialEq)]
pub struct AsciiCheckStatusError<R>(Box<(Status, R)>);

impl_error_display! {
    <R: Response> AsciiCheckStatusError<R>,
    self => "expected status {}: {}", self.0.0, self.0.1
}

impl<R: Response> AsciiCheckStatusError<R> {
    /// Create a new `AsciiCheckStatusError` error.
    ///
    /// `expected` is the expected [`Status`] and `response` is the invalid response.
    pub fn new(expected: Status, response: R) -> Self {
        AsciiCheckStatusError(Box::new((expected, response)))
    }
}

impl_from_specific_to_any_response! { AsciiCheckStatusError }
impl_traits_to_access_inner_response! { for<R: Response> (AsciiCheckStatusError) -> R { 0.1 } }

/// A response of type `R` was received with an unexpected warning flag.
#[derive(Debug, PartialEq)]
pub struct AsciiCheckWarningError<R>(Box<(String, R)>);

impl_error_display! {
    <R: Response> AsciiCheckWarningError<R>,
    self => "{}: {}", self.0.0, self.0.1
}

impl<R: Response> AsciiCheckWarningError<R> {
    /// Create a new `AsciiCheckWarningError` error.
    ///
    /// `message` is a description of the error and `response` is the invalid response.
    pub fn new(message: impl Into<String>, response: R) -> Self {
        AsciiCheckWarningError(Box::new((message.into(), response)))
    }
}

impl_from_specific_to_any_response! { AsciiCheckWarningError }
impl_traits_to_access_inner_response! { for<R: Response> (AsciiCheckWarningError) -> R { 0.1 } }

/// A response of type `R` was received with unexpected data.
#[derive(Debug, PartialEq)]
pub struct AsciiCheckDataError<R>(Box<(String, R)>);

impl<R: Response> AsciiCheckDataError<R> {
    /// Create a new `AsciiCheckDataError` error.
    ///
    /// `message` is a description of the error and `response` is the invalid response.
    pub fn new(message: impl Into<String>, response: R) -> Self {
        AsciiCheckDataError(Box::new((message.into(), response)))
    }
}

impl_error_display! {
    <R: Response> AsciiCheckDataError<R>,
    self => "{}: {}", self.0.0, self.0.1
}

impl_from_specific_to_any_response! { AsciiCheckDataError }
impl_traits_to_access_inner_response! { for<R: Response> (AsciiCheckDataError) -> R { 0.1 } }

/// A response of type `R` was received and is considered invalid for some custom reason.
///
/// This should be used when [`AsciiCheckFlagError`], [`AsciiCheckStatusError`]. [`AsciiCheckWarningError`], or [`AsciiCheckDataError`] are not appropriate.
#[derive(Debug, PartialEq)]
pub struct AsciiCheckCustomError<R>(Box<(String, R)>);

impl_error_display! {
    <R: Response> AsciiCheckCustomError<R>,
    self => "{}: {}", self.0.0, self.0.1
}

impl<R: Response> AsciiCheckCustomError<R> {
    /// Create a new `AsciiCheckCustomError` error.
    ///
    /// `message` is a description of the error and `response` is the invalid response.
    pub fn new(message: impl Into<String>, response: R) -> Self {
        AsciiCheckCustomError(Box::new((message.into(), response)))
    }

    /// Create a new `AsciiCheckCustomError` indicating a response is invalid for some unknown reason.
    pub fn unknown(response: R) -> Self {
        AsciiCheckCustomError::new("invalid response", response)
    }
}

impl_from_specific_to_any_response! { AsciiCheckCustomError }
impl_traits_to_access_inner_response! { for<R: Response> (AsciiCheckCustomError) -> R { 0.1 } }

error_enum! {
    /// Received an unexpected ASCII response.
    #[derive(Debug, PartialEq)]
    #[non_exhaustive]
    pub enum AsciiUnexpectedError {
        Target(AsciiUnexpectedTargetError),
        Id(AsciiUnexpectedIdError),
        Kind(AsciiUnexpectedKindError),
        Continuation(AsciiUnexpectedContinuationError),
    }
}

impl From<AsciiUnexpectedError> for AnyResponse {
    /// Consume the error and get the underlying response.
    fn from(other: AsciiUnexpectedError) -> Self {
        match other {
            AsciiUnexpectedError::Target(e) => e.into(),
            AsciiUnexpectedError::Id(e) => e.into(),
            AsciiUnexpectedError::Kind(e) => e.into(),
            AsciiUnexpectedError::Continuation(e) => e.into(),
        }
    }
}

impl AsRef<AnyResponse> for AsciiUnexpectedError {
    /// Get access to the response associated with this error.
    fn as_ref(&self) -> &AnyResponse {
        match self {
            AsciiUnexpectedError::Target(e) => e.as_ref(),
            AsciiUnexpectedError::Id(e) => e.as_ref(),
            AsciiUnexpectedError::Kind(e) => e.as_ref(),
            AsciiUnexpectedError::Continuation(e) => e.as_ref(),
        }
    }
}

error_enum! {
/// The contents of a response failed a [`check`](crate::ascii::check).
///
/// This indicates a response did not contain what the caller expected, though
/// the response is well formed.
#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum AsciiCheckError<R> {
    Flag(AsciiCheckFlagError),
    Status(AsciiCheckStatusError<R>),
    Warning(AsciiCheckWarningError<R>),
    Data(AsciiCheckDataError<R>),
    Custom(AsciiCheckCustomError<R>),
}

impl<R: Response>
}

impl<R: Response> AsciiCheckError<R> {
    /// Create an `AsciiCheckError` error indicating the reply flag was unexpected.
    pub fn unexpected_flag(expected: Flag, reply: Reply) -> AsciiCheckError<Reply> {
        AsciiCheckFlagError::new(expected, reply).into()
    }

    /// Create an `AsciiCheckError` error indicating the status was unexpected.
    pub fn unexpected_status(expected: Status, response: R) -> AsciiCheckError<R> {
        AsciiCheckStatusError::new(expected, response).into()
    }

    /// Create an `AsciiCheckError` error indicating the warning was unexpected.
    pub fn unexpected_warning(message: impl Into<String>, response: R) -> AsciiCheckError<R> {
        AsciiCheckWarningError::new(message, response).into()
    }

    /// Create an `AsciiCheckError` error indicating the data was unexpected.
    pub fn unexpected_data(message: impl Into<String>, response: R) -> AsciiCheckError<R> {
        AsciiCheckWarningError::new(message, response).into()
    }

    /// Create an `AsciiCheckError` error indicating the message was invalid for some custom reason.
    pub fn custom(message: impl Into<String>, response: R) -> AsciiCheckError<R> {
        AsciiCheckCustomError::new(message, response).into()
    }

    /// Create an `AsciiCheckError` error indicating the message was invalid for some unknown reason.
    pub fn unknown(response: R) -> AsciiCheckError<R> {
        AsciiCheckCustomError::unknown(response).into()
    }
}

impl AsRef<Reply> for AsciiCheckError<Reply> {
    /// Get a reference to the underlying [`Reply`].
    ///
    /// [`AsRef`] is only implemented for `AsciiCheckError<Reply>` because the
    /// `Flag` variant always holds a `Reply`. So it is the only type that a
    /// reference can always be returned for.
    fn as_ref(&self) -> &Reply {
        match self {
            AsciiCheckError::Flag(e) => e.as_ref(),
            AsciiCheckError::Status(e) => e.as_ref(),
            AsciiCheckError::Warning(e) => e.as_ref(),
            AsciiCheckError::Data(e) => e.as_ref(),
            AsciiCheckError::Custom(e) => e.as_ref(),
        }
    }
}

impl<R> From<AsciiCheckError<R>> for AnyResponse
where
    AnyResponse: From<R>,
    R: From<AsciiCheckStatusError<R>>
        + From<AsciiCheckWarningError<R>>
        + From<AsciiCheckDataError<R>>
        + From<AsciiCheckCustomError<R>>,
{
    fn from(other: AsciiCheckError<R>) -> Self {
        match other {
            AsciiCheckError::Flag(e) => Into::<Reply>::into(e).into(),
            AsciiCheckError::Status(e) => Into::<R>::into(e).into(),
            AsciiCheckError::Warning(e) => Into::<R>::into(e).into(),
            AsciiCheckError::Data(e) => Into::<R>::into(e).into(),
            AsciiCheckError::Custom(e) => Into::<R>::into(e).into(),
        }
    }
}

impl From<AsciiCheckError<Reply>> for Reply {
    fn from(other: AsciiCheckError<Reply>) -> Self {
        match other {
            AsciiCheckError::Flag(e) => e.into(),
            AsciiCheckError::Status(e) => e.into(),
            AsciiCheckError::Warning(e) => e.into(),
            AsciiCheckError::Data(e) => e.into(),
            AsciiCheckError::Custom(e) => e.into(),
        }
    }
}

impl TryFrom<Error> for AsciiCheckError<AnyResponse> {
    type Error = Error;
    fn try_from(other: Error) -> Result<Self, Self::Error> {
        match other {
            Error::AsciiCheckFlag(e) => Ok(AsciiCheckError::Flag(e)),
            Error::AsciiCheckStatus(e) => Ok(AsciiCheckError::Status(e)),
            Error::AsciiCheckWarning(e) => Ok(AsciiCheckError::Warning(e)),
            Error::AsciiCheckData(e) => Ok(AsciiCheckError::Data(e)),
            Error::AsciiCheckCustom(e) => Ok(AsciiCheckError::Custom(e)),
            e => Err(e),
        }
    }
}

impl TryFrom<AsciiError> for AsciiCheckError<AnyResponse> {
    type Error = AsciiError;
    fn try_from(other: AsciiError) -> Result<Self, Self::Error> {
        match other {
            AsciiError::CheckFlag(e) => Ok(AsciiCheckError::Flag(e)),
            AsciiError::CheckStatus(e) => Ok(AsciiCheckError::Status(e)),
            AsciiError::CheckWarning(e) => Ok(AsciiCheckError::Warning(e)),
            AsciiError::CheckData(e) => Ok(AsciiCheckError::Data(e)),
            AsciiError::CheckCustom(e) => Ok(AsciiCheckError::Custom(e)),
            e => Err(e),
        }
    }
}

impl<R> From<AsciiCheckError<R>> for AsciiCheckError<AnyResponse>
where
    R: SpecificResponse,
    AnyResponse: From<R>,
{
    fn from(other: AsciiCheckError<R>) -> Self {
        match other {
            AsciiCheckError::Flag(err) => AsciiCheckError::Flag(err),
            AsciiCheckError::Status(err) => AsciiCheckError::Status(err.into()),
            AsciiCheckError::Warning(err) => AsciiCheckError::Warning(err.into()),
            AsciiCheckError::Data(err) => AsciiCheckError::Data(err.into()),
            AsciiCheckError::Custom(err) => AsciiCheckError::Custom(err.into()),
        }
    }
}

/// A Binary command failed and an Error (`255`) response was received.
#[derive(Debug, PartialEq)]
pub struct BinaryCommandFailureError(DeviceMessage);

impl_error_display! {
    BinaryCommandFailureError,
    self => "command failed: [{}] {}",
    self.code(),
    self.name().unwrap_or("<Unknown error code>")
}
impl_binary_error! { BinaryCommandFailureError }

impl BinaryCommandFailureError {
    /// Get the name of the error.
    ///
    /// If the error code is not recognized, `None` is returned.
    pub fn name(&self) -> Option<&'static str> {
        binary_code::name(self.code())
    }

    /// Get the error code.
    pub fn code(&self) -> i32 {
        self.0.data::<i32>().unwrap()
    }
}

/// A Binary response came from an unexpected target.
#[derive(Debug, PartialEq)]
pub struct BinaryUnexpectedTargetError(DeviceMessage);

impl_error_display! {
    BinaryUnexpectedTargetError,
    self => "unexpected response target: {}", self.0
}
impl_binary_error! { BinaryUnexpectedTargetError }

/// A Binary response had an unexpected message ID.
#[derive(Debug, PartialEq)]
pub struct BinaryUnexpectedIdError(DeviceMessage);

impl_error_display! {
    BinaryUnexpectedIdError,
    self => "unexpected response message ID: {}", self.0
}
impl_binary_error! { BinaryUnexpectedIdError }

/// A Binary response had an unexpected Binary command code.
#[derive(Debug, PartialEq)]
pub struct BinaryUnexpectedCommandError(DeviceMessage);

impl_error_display! {
    BinaryUnexpectedCommandError,
    self => "unexpected response command code: {}", self.0
}
impl_binary_error! { BinaryUnexpectedCommandError }

error_enum! {
/// Received an unexpected Binary response.
#[derive(Debug, PartialEq)]
pub enum BinaryUnexpectedError {
    Target(BinaryUnexpectedTargetError),
    Id(BinaryUnexpectedIdError),
    Command(BinaryUnexpectedCommandError),
}
}

macro_rules! impl_is_timeout {
    ($name:ident) => {
        impl $name {
            /// A convenience function for determining if the error is due to the
            /// port timing out.
            pub fn is_timeout(&self) -> bool {
                matches!(self, $name::Io(e) if e.kind() == std::io::ErrorKind::TimedOut)
            }
        }
    };
}

macro_rules! impl_from_ascii_check_error {
    ($name:ident {
        $(
            $from_variant:ident => $to_variant:ident
        ),+
        $(,)?
    }) => {
        impl<R> From<AsciiCheckError<R>> for $name
        where
            R: SpecificResponse,
            AnyResponse: From<R>,
        {
            fn from(other: AsciiCheckError<R>) -> Self {
                match other {
                    $(
                        AsciiCheckError::$from_variant(e) => $name::$to_variant(e.into())
                    ),+
                }
            }
        }

        impl From<AsciiCheckError<AnyResponse>> for $name {
            fn from(other: AsciiCheckError<AnyResponse>) -> Self {
                match other {
                    $(
                        AsciiCheckError::$from_variant(e) => $name::$to_variant(e)
                    ),+
                }
            }
        }
    };
}

macro_rules! impl_from_serialport_error {
    ($name:ident) => {
        impl From<serialport::Error> for $name {
            fn from(other: serialport::Error) -> Self {
                use std::io;

                match other.kind() {
                    serialport::ErrorKind::NoDevice => $name::SerialDeviceInUseOrDisconnected(
                        SerialDeviceInUseOrDisconnectedError(other.description.into_boxed_str()),
                    ),
                    serialport::ErrorKind::InvalidInput => $name::Io(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        other.description,
                    )),
                    serialport::ErrorKind::Unknown => {
                        $name::Io(io::Error::new(io::ErrorKind::Other, other.description))
                    }
                    serialport::ErrorKind::Io(kind) => {
                        $name::Io(io::Error::new(kind, other.description))
                    }
                }
            }
        }
    };
}

error_enum! {
    /// Any error returned by the [`ascii`](crate::ascii) module.
    #[derive(Debug)]
    #[non_exhaustive]
    pub enum AsciiError {
        SerialDeviceInUseOrDisconnected(SerialDeviceInUseOrDisconnectedError),
        Io(std::io::Error),
        PacketMissingStart(AsciiPacketMissingStartError),
        PacketMissingEnd(AsciiPacketMissingEndError),
        PacketMalformed(AsciiPacketMalformedError),
        InvalidChecksum(AsciiInvalidChecksumError),
        UnexpectedTarget(AsciiUnexpectedTargetError),
        UnexpectedId(AsciiUnexpectedIdError),
        UnexpectedKind(AsciiUnexpectedKindError),
        UnexpectedContinuation(AsciiUnexpectedContinuationError),
        CheckFlag(AsciiCheckFlagError),
        CheckStatus(AsciiCheckStatusError<AnyResponse>),
        CheckWarning(AsciiCheckWarningError<AnyResponse>),
        CheckData(AsciiCheckDataError<AnyResponse>),
        CheckCustom(AsciiCheckCustomError<AnyResponse>),
    }

    impl From<AsciiProtocolError> {
        PacketMissingStart => PacketMissingStart,
        PacketMissingEnd => PacketMissingEnd,
        PacketMalformed => PacketMalformed,
    }

    impl From<AsciiUnexpectedError> {
        Target => UnexpectedTarget,
        Id => UnexpectedId,
        Kind => UnexpectedKind,
        Continuation => UnexpectedContinuation,
    }
}
impl_is_timeout! { AsciiError }
impl_from_serialport_error! { AsciiError }
impl_from_ascii_check_error! {
    AsciiError {
        Flag => CheckFlag,
        Status => CheckStatus,
        Warning => CheckWarning,
        Data => CheckData,
        Custom => CheckCustom,
    }
}

error_enum! {
    /// Any error returned by the [`binary`](crate::binary) module.
    #[derive(Debug)]
    #[non_exhaustive]
    pub enum BinaryError {
        SerialDeviceInUseOrDisconnected(SerialDeviceInUseOrDisconnectedError),
        Io(std::io::Error),
        CommandFailure(BinaryCommandFailureError),
        UnexpectedTarget(BinaryUnexpectedTargetError),
        UnexpectedId(BinaryUnexpectedIdError),
        UnexpectedCommand(BinaryUnexpectedCommandError),
    }

    impl From<BinaryUnexpectedError> {
        Target => UnexpectedTarget,
        Id => UnexpectedId,
        Command => UnexpectedCommand,
    }
}
impl_is_timeout! { BinaryError }
impl_from_serialport_error! { BinaryError }

error_enum! {
    /// Any error returned by this library.
    #[derive(Debug)]
    #[non_exhaustive]
    pub enum Error {
        SerialDeviceInUseOrDisconnected(SerialDeviceInUseOrDisconnectedError),
        Io(std::io::Error),
        AsciiPacketMissingStart(AsciiPacketMissingStartError),
        AsciiPacketMissingEnd(AsciiPacketMissingEndError),
        AsciiPacketMalformed(AsciiPacketMalformedError),
        AsciiInvalidChecksum(AsciiInvalidChecksumError),
        AsciiUnexpectedTarget(AsciiUnexpectedTargetError),
        AsciiUnexpectedId(AsciiUnexpectedIdError),
        AsciiUnexpectedKind(AsciiUnexpectedKindError),
        AsciiUnexpectedContinuation(AsciiUnexpectedContinuationError),
        AsciiCheckFlag(AsciiCheckFlagError),
        AsciiCheckStatus(AsciiCheckStatusError<AnyResponse>),
        AsciiCheckWarning(AsciiCheckWarningError<AnyResponse>),
        AsciiCheckData(AsciiCheckDataError<AnyResponse>),
        AsciiCheckCustom(AsciiCheckCustomError<AnyResponse>),
        BinaryCommandFailure(BinaryCommandFailureError),
        BinaryUnexpectedTarget(BinaryUnexpectedTargetError),
        BinaryUnexpectedId(BinaryUnexpectedIdError),
        BinaryUnexpectedCommand(BinaryUnexpectedCommandError),
    }

    impl From<AsciiProtocolError> {
        PacketMissingStart => AsciiPacketMissingStart,
        PacketMissingEnd => AsciiPacketMissingEnd,
        PacketMalformed => AsciiPacketMalformed,
    }

    impl From<AsciiUnexpectedError> {
        Target => AsciiUnexpectedTarget,
        Id => AsciiUnexpectedId,
        Kind => AsciiUnexpectedKind,
        Continuation => AsciiUnexpectedContinuation,
    }

    impl From<AsciiError> {
        SerialDeviceInUseOrDisconnected => SerialDeviceInUseOrDisconnected,
        Io => Io,
        PacketMissingStart => AsciiPacketMissingStart,
        PacketMissingEnd => AsciiPacketMissingEnd,
        PacketMalformed => AsciiPacketMalformed,
        InvalidChecksum => AsciiInvalidChecksum,
        UnexpectedTarget => AsciiUnexpectedTarget,
        UnexpectedId => AsciiUnexpectedId,
        UnexpectedKind => AsciiUnexpectedKind,
        UnexpectedContinuation => AsciiUnexpectedContinuation,
        CheckFlag => AsciiCheckFlag,
        CheckStatus => AsciiCheckStatus,
        CheckWarning => AsciiCheckWarning,
        CheckData => AsciiCheckData,
        CheckCustom => AsciiCheckCustom,
    }

    impl From<BinaryUnexpectedError> {
        Target => BinaryUnexpectedTarget,
        Id => BinaryUnexpectedId,
        Command => BinaryUnexpectedCommand,
    }

    impl From<BinaryError> {
        SerialDeviceInUseOrDisconnected => SerialDeviceInUseOrDisconnected,
        Io => Io,
        CommandFailure => BinaryCommandFailure,
        UnexpectedTarget => BinaryUnexpectedTarget,
        UnexpectedId => BinaryUnexpectedId,
        UnexpectedCommand => BinaryUnexpectedCommand,
    }
}
impl_is_timeout! { Error }
impl_from_serialport_error! { Error }
impl_from_ascii_check_error! {
    Error {
        Flag => AsciiCheckFlag,
        Status => AsciiCheckStatus,
        Warning => AsciiCheckWarning,
        Data => AsciiCheckData,
        Custom => AsciiCheckCustom,
    }
}

macro_rules! define_error_codes {
    // Entry point.
    //
    // Serves to concatenate the parts of the name before defining the constants.
    (
        $(
            $num:literal: $($name_word:ident)+
        ),+
        $(,)?
    ) => {
        paste::paste! {
            define_error_codes!{@with_concatenated_name
                $(
                    $num: $($name_word)+, [< $($name_word:camel)+ >]
                 ),+
            }
        }
    };
    (@with_concatenated_name
        $(
            $num:literal: $($name_word:ident)+, $name:ident
        ),+
    ) => {
        paste::paste! {
            pub mod binary_code {
                //! Binary error codes.
                //!
                //! The codes in numerical order are:
                #![doc =
                $( "* `" $num "`: [`" $name:snake:upper "`]\n\n" )+
                ]

                $(
                    #[doc = $(" " $name_word " ")+ "(code `" $num "`)." ]
                    pub const [< $name:snake:upper >] : i32 = $num;
                )+

                /// Get the name of an error code.
                ///
                /// If the error code is not recognized, `None` is returned.
                /// The contents of the returned string may change.
                pub(crate) const fn name(code: i32) -> Option<&'static str> {
                    match code {
                        $(
                            $num => Some(stringify!($($name_word)+)),
                        )+
                        _ => None,
                    }
                }
            }
        }
    };
}

define_error_codes! {
    1: Cannot Home,
    2: Device Number Invalid,
    14: Voltage Low,
    15: Voltage High,
    18: Stored Position Invalid,
    20: Absolute Position Invalid,
    21: Relative Position Invalid,
    22: Velocity Invalid,
    36: Restore Settings Data Invalid,
    37: Resolution Invalid,
    38: Run Current Invalid,
    39: Hold Current Invalid,
    41: Home Speed Invalid,
    42: Speed Invalid,
    43: Acceleration Invalid,
    44: Maximum Position Invalid,
    45: Current Position Invalid,
    47: Offset Invalid,
    48: Alias Invalid,
    53: Setting Invalid,
    64: Command Invalid,
    65: Park State Invalid,
    67: Temperature High,
    68: Digital Input Pin Invalid,
    71: Digital Output Pin Invalid,
    74: Digital Output Mask Invalid,
    76: Analog Input Pin Invalid,
    78: Move Index Number Invalid,
    79: Index Distance Invalid,
    80: Cycle Distance Invalid,
    81: Filter Holder Id Invalid,
    87: Absolute Force Invalid,
    101: Auto Reply Disabled Moded Invalid,
    102: Message Id Mode Invalid,
    103: Home Status Invalid,
    105: Auto Home Disabled Mode Invalid,
    106: Minimum Position Invalid,
    107: Knob Disabled Mode Invalid,
    108: Knob Direction Invalid,
    109: Knob Movement Mode Invalid,
    110: Knob Jog Size Invalid,
    111: Knob Velocity Scale Invalid,
    112: Knob Velocity Profile Invalid,
    113: Acceleration Only Invalid,
    114: Deceleration Only Invalid,
    115: Move Tracking Mode Invalid,
    116: Manual Move Tracking Disabled Mode Invalid,
    117: Move Tracking Period Invalid,
    118: Closed Loop Mode Invalid,
    119: Slip Tracking Period Invalid,
    120: Stall Timeout Invalid,
    122: Baud Rate Invalid,
    123: Protocol Invalid,
    124: Baud Rate or Protocol Invalid,
    255: Busy,
    257: System Error,
    401: Storage Full,
    1600: Save Position Invalid,
    1601: Save Position Not Homed,
    1700: Return Position Invalid,
    1800: Move Position Invalid,
    1801: Move Position Not Homed,
    6501: Device Parked,
    9001: Driver Disabled,
    9301: Peripheral Inactive,
}

#[cfg(test)]
mod test {
    use super::binary_code::*;
    use super::*;
    use crate::ascii::*;
    use static_assertions::{assert_impl_all, const_assert, const_assert_eq};

    // Make sure the error enums are at most 3 words large (the same size as a String).
    // This will minimize the size of Result<R, Error>.
    const _WORD_SIZE: usize = std::mem::size_of::<&usize>();
    const_assert_eq!(
        std::mem::size_of::<AsciiCheckError<AnyResponse>>(), // AnyResponse is the largests response type
        2 * _WORD_SIZE
    );
    const_assert_eq!(std::mem::size_of::<AsciiUnexpectedError>(), 3 * _WORD_SIZE);
    const_assert_eq!(std::mem::size_of::<AsciiProtocolError>(), 3 * _WORD_SIZE);
    const_assert_eq!(std::mem::size_of::<AsciiError>(), 3 * _WORD_SIZE);
    const_assert_eq!(std::mem::size_of::<DeviceMessage>(), 8);
    const_assert!(
        std::mem::size_of::<BinaryUnexpectedError>()
            < std::mem::size_of::<DeviceMessage>() + _WORD_SIZE
    );
    const_assert_eq!(std::mem::size_of::<BinaryError>(), 3 * _WORD_SIZE);
    const_assert_eq!(std::mem::size_of::<Error>(), 3 * _WORD_SIZE);

    // Make sure that error enum types are properly convertable
    assert_impl_all!(Error: From<AsciiProtocolError>);
    assert_impl_all!(Error: From<AsciiUnexpectedError>);
    assert_impl_all!(Error: From<AsciiCheckError<AnyResponse>>);
    assert_impl_all!(Error: From<AsciiCheckError<Reply>>);
    assert_impl_all!(Error: From<AsciiCheckError<Info>>);
    assert_impl_all!(Error: From<AsciiCheckError<Alert>>);
    assert_impl_all!(Error: From<AsciiError>);
    assert_impl_all!(Error: From<BinaryError>);

    assert_impl_all!(AsciiError: From<AsciiProtocolError>);
    assert_impl_all!(AsciiError: From<AsciiUnexpectedError>);
    assert_impl_all!(AsciiError: From<AsciiCheckError<AnyResponse>>);
    assert_impl_all!(AsciiError: From<AsciiCheckError<Reply>>);
    assert_impl_all!(AsciiError: From<AsciiCheckError<Info>>);
    assert_impl_all!(AsciiError: From<AsciiCheckError<Alert>>);

    assert_impl_all!(AsciiError: TryFrom<Error>);

    assert_impl_all!(AsciiProtocolError: TryFrom<AsciiError>);
    assert_impl_all!(AsciiUnexpectedError: TryFrom<AsciiError>);
    assert_impl_all!(AsciiCheckError<AnyResponse>: TryFrom<AsciiError>);

    assert_impl_all!(AsciiProtocolError: TryFrom<Error>);
    assert_impl_all!(AsciiUnexpectedError: TryFrom<Error>);
    assert_impl_all!(AsciiCheckError<AnyResponse>: TryFrom<Error>);

    assert_impl_all!(BinaryError: From<BinaryUnexpectedError>);
    assert_impl_all!(BinaryError: TryFrom<Error>);
    assert_impl_all!(BinaryUnexpectedError: TryFrom<BinaryError>);
    assert_impl_all!(BinaryUnexpectedError: TryFrom<Error>);

    // Check that error types and responses they wrap implement AsRef and From,
    // respectively.
    macro_rules! assert_accessor_traits_to_response {
        ($type:path => $response:ident) => {
            assert_impl_all!($response: From<$type>);
            assert_impl_all!($type: AsRef<$response>);
        };
    }

    assert_accessor_traits_to_response!(AsciiUnexpectedError => AnyResponse);
    assert_accessor_traits_to_response!(AsciiUnexpectedTargetError => AnyResponse);
    assert_accessor_traits_to_response!(AsciiUnexpectedIdError => AnyResponse);
    assert_accessor_traits_to_response!(AsciiUnexpectedKindError => AnyResponse);
    assert_accessor_traits_to_response!(AsciiUnexpectedContinuationError => AnyResponse);

    // Because the `AsciiCheckError::<R>::Flag` variant always holds a `Reply`,
    // `AsciiCheckError<R>` can only implement `AsRef` when R == Reply. Similarly,
    // R can only implement `From<AsciiCheckError<R>>` when R is in (Reply,
    // AnyResponse).
    assert_accessor_traits_to_response!(AsciiCheckError<Reply> => Reply);
    assert_impl_all!(
        AnyResponse: From<AsciiCheckError<AnyResponse>>,
        From<AsciiCheckError<Reply>>,
        From<AsciiCheckError<Info>>,
        From<AsciiCheckError<Alert>>
    );

    assert_accessor_traits_to_response!(AsciiCheckFlagError => Reply);

    assert_accessor_traits_to_response!(AsciiCheckStatusError<AnyResponse> => AnyResponse);
    assert_accessor_traits_to_response!(AsciiCheckStatusError<Reply> => Reply);
    assert_accessor_traits_to_response!(AsciiCheckStatusError<Info> => Info);
    assert_accessor_traits_to_response!(AsciiCheckStatusError<Alert> => Alert);

    assert_accessor_traits_to_response!(AsciiCheckWarningError<AnyResponse> => AnyResponse);
    assert_accessor_traits_to_response!(AsciiCheckWarningError<Reply> => Reply);
    assert_accessor_traits_to_response!(AsciiCheckWarningError<Info> => Info);
    assert_accessor_traits_to_response!(AsciiCheckWarningError<Alert> => Alert);

    assert_accessor_traits_to_response!(AsciiCheckDataError<AnyResponse> => AnyResponse);
    assert_accessor_traits_to_response!(AsciiCheckDataError<Reply> => Reply);
    assert_accessor_traits_to_response!(AsciiCheckDataError<Info> => Info);
    assert_accessor_traits_to_response!(AsciiCheckDataError<Alert> => Alert);

    assert_accessor_traits_to_response!(AsciiCheckCustomError<AnyResponse> => AnyResponse);
    assert_accessor_traits_to_response!(AsciiCheckCustomError<Reply> => Reply);
    assert_accessor_traits_to_response!(AsciiCheckCustomError<Info> => Info);
    assert_accessor_traits_to_response!(AsciiCheckCustomError<Alert> => Alert);

    #[test]
    fn binary_error_code_names() {
        assert_eq!(name(PERIPHERAL_INACTIVE), Some("Peripheral Inactive"));
        assert_eq!(
            name(MANUAL_MOVE_TRACKING_DISABLED_MODE_INVALID),
            Some("Manual Move Tracking Disabled Mode Invalid")
        );
        assert_eq!(name(9999999), None);
    }
}
