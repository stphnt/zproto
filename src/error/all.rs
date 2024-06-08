//! Error types for both ASCII and Binary protocol.

use super::*;
use crate::ascii::{AnyResponse, SpecificResponse};

error_enum! {
    /// Any error returned by this library.
    #[derive(Debug)]
    #[non_exhaustive]
    #[cfg_attr(
        all(doc, feature = "doc_cfg"),
        doc(cfg(all(feature = "ascii", feature = "binary")))
    )]
    pub enum Error {
        SerialDeviceInUseOrDisconnected(SerialDeviceInUseOrDisconnectedError),
        Io(std::io::Error),
        TryIntoSend(TryIntoSendError),
        AsciiPacketMissingStart(AsciiPacketMissingStartError),
        AsciiPacketMissingEnd(AsciiPacketMissingEndError),
        AsciiPacketMalformed(AsciiPacketMalformedError),
        AsciiInvalidChecksum(AsciiInvalidChecksumError),
        AsciiUnexpectedResponse(AsciiUnexpectedResponseError),
        AsciiUnexpectedPacket(AsciiUnexpectedPacketError),
        AsciiCheckFlag(AsciiCheckFlagError),
        AsciiCheckStatus(AsciiCheckStatusError<AnyResponse>),
        AsciiCheckWarning(AsciiCheckWarningError<AnyResponse>),
        AsciiCheckData(AsciiCheckDataError<AnyResponse>),
        AsciiCheckCustom(AsciiCheckCustomError<AnyResponse>),
        AsciiCommandSplit(AsciiCommandSplitError),
        AsciiReservedCharacter(AsciiReservedCharacterError),
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

    impl From<AsciiError> {
        SerialDeviceInUseOrDisconnected => SerialDeviceInUseOrDisconnected,
        Io => Io,
        TryIntoSend => TryIntoSend,
        PacketMissingStart => AsciiPacketMissingStart,
        PacketMissingEnd => AsciiPacketMissingEnd,
        PacketMalformed => AsciiPacketMalformed,
        InvalidChecksum => AsciiInvalidChecksum,
        UnexpectedResponse => AsciiUnexpectedResponse,
        UnexpectedPacket => AsciiUnexpectedPacket,
        CheckFlag => AsciiCheckFlag,
        CheckStatus => AsciiCheckStatus,
        CheckWarning => AsciiCheckWarning,
        CheckData => AsciiCheckData,
        CheckCustom => AsciiCheckCustom,
        CommandSplit => AsciiCommandSplit,
        ReservedCharacter => AsciiReservedCharacter,
    }

    impl From<BinaryUnexpectedError> {
        Target => BinaryUnexpectedTarget,
        Id => BinaryUnexpectedId,
        Command => BinaryUnexpectedCommand,
    }

    impl From<BinaryError> {
        SerialDeviceInUseOrDisconnected => SerialDeviceInUseOrDisconnected,
        Io => Io,
        TryIntoSend => TryIntoSend,
        CommandFailure => BinaryCommandFailure,
        UnexpectedTarget => BinaryUnexpectedTarget,
        UnexpectedId => BinaryUnexpectedId,
        UnexpectedCommand => BinaryUnexpectedCommand,
    }
}
impl_is_timeout! { Error }
impl_is_io! { Error }
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::ascii::{Alert, Info, Reply};
    use static_assertions::{assert_impl_all, const_assert_eq};

    // Make sure the error enums are at most 3 words large (the same size as a String).
    // This will minimize the size of Result<R, Error>.
    const _WORD_SIZE: usize = std::mem::size_of::<&usize>();
    const_assert_eq!(std::mem::size_of::<Error>(), 3 * _WORD_SIZE);

    // Make sure that error enum types are properly convertible
    assert_impl_all!(Error: From<AsciiProtocolError>);
    assert_impl_all!(Error: From<AsciiCheckError<AnyResponse>>);
    assert_impl_all!(Error: From<AsciiCheckError<Reply>>);
    assert_impl_all!(Error: From<AsciiCheckError<Info>>);
    assert_impl_all!(Error: From<AsciiCheckError<Alert>>);
    assert_impl_all!(Error: From<AsciiError>);
    assert_impl_all!(Error: From<BinaryError>);

    assert_impl_all!(AsciiError: TryFrom<Error>);

    assert_impl_all!(AsciiProtocolError: TryFrom<Error>);
    assert_impl_all!(AsciiCheckError<AnyResponse>: TryFrom<Error>);

    assert_impl_all!(BinaryError: TryFrom<Error>);
}
