//! Custom conversion traits.

/// Custom conversion trait.
///
/// Mirrors [`std::convert::From`] but is used when `std` trait cannot be.
pub trait From<T>: Sized {
	/// Create an instance of the type from another type `T`.
	fn from(value: T) -> Self;
}
