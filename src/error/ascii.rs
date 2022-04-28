//! Error types related to Zaber's ASCII protocol.

use super::SerialDeviceInUseOrDisconnectedError;
use crate::ascii::{Alert, AnyResponse, Flag, Info, Reply, Response, SpecificResponse, Status};

/// Implement the `new()` and `as_bytes()` methods errors storing bytes.
macro_rules! impl_for_type_containing_bytes {
    (
        $name:ident
    ) => {
        impl $name {
            /// Create a instance of the error
            pub(crate) fn new<R: AsRef<[u8]>>(bytes: R) -> Self {
                $name(Box::from(bytes.as_ref()))
            }

            /// Get the bytes of the invalid packet.
            pub fn as_bytes(&self) -> &[u8] {
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

/// An ASCII packet was missing a start byte.
#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub struct AsciiPacketMissingStartError(Box<[u8]>);

impl_error_display! {
    AsciiPacketMissingStartError,
    self => "ASCII packet missing a start byte: {}", String::from_utf8_lossy(&self.0)
}
impl_for_type_containing_bytes! { AsciiPacketMissingStartError }

/// An ASCII packet was missing the end byte(s).
#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub struct AsciiPacketMissingEndError(Box<[u8]>);

impl_error_display! {
    AsciiPacketMissingEndError,
    self => "ASCII packet missing end byte(s): {}", String::from_utf8_lossy(&self.0)
}
impl_for_type_containing_bytes! { AsciiPacketMissingEndError }

/// An ASCII packet is malformed.
#[derive(Debug, PartialEq, Eq, Hash)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub struct AsciiPacketMalformedError(Box<[u8]>);

impl_error_display! {
    AsciiPacketMalformedError,
    self => "ASCII packet is malformed: {}", String::from_utf8_lossy(&self.0)
}
impl_for_type_containing_bytes! { AsciiPacketMalformedError }

error_enum! {
    /// Received data that did not conform to the ASCII protocol.
    #[derive(Debug, PartialEq, Eq, Hash)]
    #[non_exhaustive]
    #[cfg_attr(
    all(doc, feature = "doc_cfg"),
    doc(cfg(feature = "ascii"))
)]
    pub enum AsciiProtocolError {
        PacketMissingStart(AsciiPacketMissingStartError),
        PacketMissingEnd(AsciiPacketMissingEndError),
        PacketMalformed(AsciiPacketMalformedError),
    }
}

/// A response had an invalid checksum, indicating it was corrupt.
#[derive(Debug, PartialEq)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub struct AsciiInvalidChecksumError(AnyResponse);

impl_error_display! {
    AsciiInvalidChecksumError,
    self => "invalid checksum: {}", self.0
}

impl_traits_to_access_inner_response! { (AsciiInvalidChecksumError) -> AnyResponse { 0 } }
impl_new_for_into_any_response! { AsciiInvalidChecksumError }

/// A response came from an unexpected target.
#[derive(Debug, PartialEq)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub struct AsciiUnexpectedTargetError(AnyResponse);

impl_error_display! {
    AsciiUnexpectedTargetError,
    self => "unexpected response target: {}", self.0
}

impl_traits_to_access_inner_response! { (AsciiUnexpectedTargetError) -> AnyResponse { 0 } }
impl_new_for_into_any_response! { AsciiUnexpectedTargetError }

/// A response had an unexpected message ID.
#[derive(Debug, PartialEq)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub struct AsciiUnexpectedIdError(AnyResponse);

impl_error_display! {
    AsciiUnexpectedIdError,
    self => "unexpected response message ID: {}", self.0
}

impl_traits_to_access_inner_response! { (AsciiUnexpectedIdError) -> AnyResponse { 0 } }
impl_new_for_into_any_response! { AsciiUnexpectedIdError }

/// A response had an unexpected message kind.
#[derive(Debug, PartialEq)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub struct AsciiUnexpectedKindError(AnyResponse);

impl_error_display! {
    AsciiUnexpectedKindError,
    self => "unexpected kind of response: {}", self.0
}

impl_traits_to_access_inner_response! { (AsciiUnexpectedKindError) -> AnyResponse { 0 } }
impl_new_for_into_any_response! { AsciiUnexpectedKindError }

/// A response was expected to be a continuation info message but it was not.
#[derive(Debug, PartialEq)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub struct AsciiUnexpectedContinuationError(AnyResponse);

impl_error_display! {
    AsciiUnexpectedContinuationError,
    self => "unexpected non-continuation message: {}", self.0
}

impl_traits_to_access_inner_response! { (AsciiUnexpectedContinuationError) -> AnyResponse { 0 } }
impl_new_for_into_any_response! { AsciiUnexpectedContinuationError }

/// A [`Reply`] was received with an unexpected reply flag.
#[derive(Debug, PartialEq)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
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
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
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
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub struct AsciiCheckWarningError<R>(Box<(String, R)>);

impl_error_display! {
    <R: Response> AsciiCheckWarningError<R>,
    self => "{}: {}", self.0.0, self.0.1
}

impl<R: Response> AsciiCheckWarningError<R> {
    /// Create a new `AsciiCheckWarningError` error.
    ///
    /// `message` is a description of the error and `response` is the invalid response.
    pub fn new<T: Into<String>>(message: T, response: R) -> Self {
        AsciiCheckWarningError(Box::new((message.into(), response)))
    }
}

impl_from_specific_to_any_response! { AsciiCheckWarningError }
impl_traits_to_access_inner_response! { for<R: Response> (AsciiCheckWarningError) -> R { 0.1 } }

/// A response of type `R` was received with unexpected data.
#[derive(Debug, PartialEq)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub struct AsciiCheckDataError<R>(Box<(String, R)>);

impl<R: Response> AsciiCheckDataError<R> {
    /// Create a new `AsciiCheckDataError` error.
    ///
    /// `message` is a description of the error and `response` is the invalid response.
    pub fn new<T: Into<String>>(message: T, response: R) -> Self {
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
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub struct AsciiCheckCustomError<R>(Box<(String, R)>);

impl_error_display! {
    <R: Response> AsciiCheckCustomError<R>,
    self => "{}: {}", self.0.0, self.0.1
}

impl<R: Response> AsciiCheckCustomError<R> {
    /// Create a new `AsciiCheckCustomError` error.
    ///
    /// `message` is a description of the error and `response` is the invalid response.
    pub fn new<T: Into<String>>(message: T, response: R) -> Self {
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
    #[cfg_attr(
    all(doc, feature = "doc_cfg"),
    doc(cfg(feature = "ascii"))
)]
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
#[cfg_attr(
    all(doc, feature = "doc_cfg"),
    doc(cfg(feature = "ascii"))
)]
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
    pub fn unexpected_warning<T: Into<String>>(message: T, response: R) -> AsciiCheckError<R> {
        AsciiCheckWarningError::new(message, response).into()
    }

    /// Create an `AsciiCheckError` error indicating the data was unexpected.
    pub fn unexpected_data<T: Into<String>>(message: T, response: R) -> AsciiCheckError<R> {
        AsciiCheckWarningError::new(message, response).into()
    }

    /// Create an `AsciiCheckError` error indicating the message was invalid for some custom reason.
    pub fn custom<T: Into<String>>(message: T, response: R) -> AsciiCheckError<R> {
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

error_enum! {
    /// Any error returned by the [`ascii`](crate::ascii) module.
    #[derive(Debug)]
    #[non_exhaustive]
    #[cfg_attr(
    all(doc, feature = "doc_cfg"),
    doc(cfg(feature = "ascii"))
)]
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::ascii::*;
    use static_assertions::{assert_impl_all, const_assert_eq};

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

    // Make sure that error enum types are properly convertible
    assert_impl_all!(AsciiError: From<AsciiProtocolError>);
    assert_impl_all!(AsciiError: From<AsciiUnexpectedError>);
    assert_impl_all!(AsciiError: From<AsciiCheckError<AnyResponse>>);
    assert_impl_all!(AsciiError: From<AsciiCheckError<Reply>>);
    assert_impl_all!(AsciiError: From<AsciiCheckError<Info>>);
    assert_impl_all!(AsciiError: From<AsciiCheckError<Alert>>);

    assert_impl_all!(AsciiProtocolError: TryFrom<AsciiError>);
    assert_impl_all!(AsciiUnexpectedError: TryFrom<AsciiError>);
    assert_impl_all!(AsciiCheckError<AnyResponse>: TryFrom<AsciiError>);

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
}
