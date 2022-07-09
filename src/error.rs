//! Error types.
//!
//! Each error is represented by a unique type that implements [`std::error::Error`].
//! However, most APIs return more than one kind of error and so will return one
//! of the higher level [enums](#enums), such as [`AsciiError`], [`BinaryError`],
//! or [`Error`]. Where appropriate, the error types are convertible to the
//! higher level enums, allowing them to be used with `?`:
//!
//! ```
//! use zproto::error::{AsciiProtocolError, Error};
//!
//! fn foo() -> Result<(), AsciiProtocolError> {
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
//! # use zproto::{ascii, error::AsciiUnexpectedResponseError};
//! #
//! # fn wrapper() {
//! let error: AsciiUnexpectedResponseError = //...
//! # todo!();
//! let response: &ascii::AnyResponse = error.as_ref();
//! // OR
//! let response: ascii::AnyResponse = error.into();
//! # }
//! ```

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

#[cfg(any(feature = "ascii", feature = "binary"))]
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

#[cfg(any(feature = "ascii", feature = "binary"))]
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

#[cfg(feature = "ascii")]
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
/// ```compile_fail
/// # // This fails to compile because the macro is not exported.
/// error_enum!{
///     // This defines the enum and From/TryFrom between ThisError and A and B.
///     #[non_exhaustive]
///     pub enum ThisError {
///         VariantA(A),
///         VariantB(B),
///         // ...
///     }
///
///     // This implements a simple From/TryFrom between ThisError and OtherType.
///     impl From<OtherType> {
///         FromVariantA => VariantA,
///         // ...
///     }
/// }
/// ```
#[cfg(any(feature = "ascii", feature = "binary"))]
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

        // Allow the enum to be convertible from an infallible error
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

        // Allow the enum to be convertible from an infallible error
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

#[cfg(feature = "ascii")]
mod ascii;
#[cfg(feature = "ascii")]
pub use ascii::*;

#[cfg(feature = "binary")]
mod binary;
#[cfg(feature = "binary")]
pub use binary::*;

#[cfg(all(feature = "ascii", feature = "binary"))]
mod all;
#[cfg(all(feature = "ascii", feature = "binary"))]
pub use all::*;

/// The specified device is either disconnected or already in use by another process.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct SerialDeviceInUseOrDisconnectedError(Box<str>);

impl_error_display! {
    SerialDeviceInUseOrDisconnectedError,
    self =>
    "the specified device is either disconnected or already in use by another process: {}", self.0
}
