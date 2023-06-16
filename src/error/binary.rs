//! Error types for the Zaber's Binary protocol.

use super::SerialDeviceInUseOrDisconnectedError;
use crate::binary::Message;

macro_rules! impl_binary_error {
    ($name:ident) => {
        impl $name {
            /// Create a new error.
            pub(crate) const fn new(message: Message) -> Self {
                $name(message)
            }
        }

        impl AsRef<Message> for $name {
            fn as_ref(&self) -> &Message {
                &self.0
            }
        }

        impl From<$name> for Message {
            fn from(other: $name) -> Self {
                other.0
            }
        }
    };
}

/// A Binary command failed and an Error (`255`) response was received.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "binary")))]
pub struct BinaryCommandFailureError(Message);

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
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "binary")))]
pub struct BinaryUnexpectedTargetError(Message);

impl_error_display! {
    BinaryUnexpectedTargetError,
    self => "unexpected response target: {}", self.0
}
impl_binary_error! { BinaryUnexpectedTargetError }

/// A Binary response had an unexpected message ID.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "binary")))]
pub struct BinaryUnexpectedIdError(Message);

impl_error_display! {
    BinaryUnexpectedIdError,
    self => "unexpected response message ID: {}", self.0
}
impl_binary_error! { BinaryUnexpectedIdError }

/// A Binary response had an unexpected Binary command code.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "binary")))]
pub struct BinaryUnexpectedCommandError(Message);

impl_error_display! {
    BinaryUnexpectedCommandError,
    self => "unexpected response command code: {}", self.0
}
impl_binary_error! { BinaryUnexpectedCommandError }

error_enum! {
/// Received an unexpected Binary response.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(
    all(doc, feature = "doc_cfg"),
    doc(cfg(feature = "binary"))
)]
pub enum BinaryUnexpectedError {
    Target(BinaryUnexpectedTargetError),
    Id(BinaryUnexpectedIdError),
    Command(BinaryUnexpectedCommandError),
}
}

error_enum! {
    /// Any error returned by the [`binary`](crate::binary) module.
    #[derive(Debug)]
    #[non_exhaustive]
    #[cfg_attr(
    all(doc, feature = "doc_cfg"),
    doc(cfg(feature = "binary"))
)]
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
impl_is_io! { BinaryError }
impl_from_serialport_error! { BinaryError }

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
            #[cfg_attr(
    all(doc, feature = "doc_cfg"),
    doc(cfg(feature = "binary"))
)]
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
                pub const fn name(code: i32) -> Option<&'static str> {
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
    use static_assertions::{assert_impl_all, const_assert, const_assert_eq};

    // Make sure the error enums are at most 3 words large (the same size as a String).
    // This will minimize the size of Result<R, Error>.
    const _WORD_SIZE: usize = std::mem::size_of::<&usize>();
    const_assert_eq!(std::mem::size_of::<Message>(), 8);
    const_assert!(
        std::mem::size_of::<BinaryUnexpectedError>() < std::mem::size_of::<Message>() + _WORD_SIZE
    );
    const_assert_eq!(std::mem::size_of::<BinaryError>(), 3 * _WORD_SIZE);

    assert_impl_all!(BinaryError: From<BinaryUnexpectedError>);
    assert_impl_all!(BinaryUnexpectedError: TryFrom<BinaryError>);

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
