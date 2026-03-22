//! Types and traits for serializing and deserializing data in the ASCII protocol.
use std::fmt;

/// Types that can be serialized into an ASCII packet.
pub trait Serialize {
	/// Serializes the data into the packet exposed via `f`.
	fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl<T> Serialize for &T
where
	T: Serialize,
{
	fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		(*self).serialize(f)
	}
}

/// Types that can be deserialized from ASCII packet data.
pub trait Deserialize: Sized {
	/// The error returned if deserialization fails.
	type Error;

	/// Deserializes `s` from an ASCII packet into `Self`.
	///
	/// Implementations can assume `s` has no leading or trailing whitespace.
	fn deserialize(s: &str) -> Result<Self, Self::Error>;
}

macro_rules! impl_by_deferring_to_display_and_from_str {
	($($type:ty),+ $(,)?) => {
		$(
			impl Serialize for $type {
				fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
					write!(f, "{self}")
				}
			}

			impl Deserialize for $type {
				type Error = <$type as std::str::FromStr>::Err;

				fn deserialize(data: &str) -> Result<Self, Self::Error> {
					data.parse()
				}
			}
		)+
	};
}

impl_by_deferring_to_display_and_from_str! {
	u8, u16, u32, u64, u128, usize,
	i8, i16, i32, i64, i128, isize,
	f32, f64,
	char, String,
	std::net::Ipv4Addr, std::net::SocketAddrV4,
	std::net::Ipv6Addr, std::net::SocketAddrV6,
	std::net::IpAddr, std::net::SocketAddr,
}

impl Serialize for bool {
	fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str(if *self { "1" } else { "0" })
	}
}

impl Deserialize for bool {
	type Error = std::num::ParseIntError;
	fn deserialize(s: &str) -> Result<Self, Self::Error> {
		match u8::from_str_radix(s, 2)? {
			0 => Ok(false),
			// The only other possible value parsed in base 2 is 1.
			_ => Ok(true),
		}
	}
}

/// A type that implements [`std::fmt::Display`] and [`std::str::FromStr`] by
/// deferring to the [`Serialize`] and [`Deserialize`] trait.
///
/// ```
/// # use zproto::ascii::serialization::Adapter;
/// assert_eq!("true", true.to_string());
/// assert_eq!("1", Adapter(true).to_string());
///
/// assert!("1".parse::<bool>().is_err());
/// assert_eq!(
///     "1".parse::<Adapter<bool>>().unwrap().0,
///     true,
/// );
/// ```
#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
pub struct Adapter<T>(pub T);

impl<T> Adapter<T> {
	/// Consumes `Self` and returns the inner value of type `T`.
	pub fn into_inner(self) -> T {
		self.0
	}

	/// Returns a shared reference to the inner value.
	pub fn inner(&self) -> &T {
		&self.0
	}

	/// Returns an exclusice reference to the inner value.
	pub fn inner_mut(&mut self) -> &mut T {
		&mut self.0
	}
}

impl<T> std::fmt::Display for Adapter<T>
where
	T: Serialize,
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.0.serialize(f)
	}
}

impl<T> std::str::FromStr for Adapter<T>
where
	T: Deserialize,
{
	type Err = <T as Deserialize>::Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		T::deserialize(s).map(Adapter)
	}
}

impl<T> From<T> for Adapter<T> {
	fn from(value: T) -> Self {
		Adapter(value)
	}
}
