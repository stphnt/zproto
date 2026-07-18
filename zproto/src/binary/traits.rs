//! Traits encoding information about Binary commands/messages and the types that can generate them.

pub(crate) mod private {
	/// Marker trait indicating whether a type can implement a binary trait.
	pub trait Sealed {}
	impl Sealed for u8 {}
	impl<T> Sealed for &T where T: Sealed + ?Sized {}
	impl<T> Sealed for Box<T> where T: Sealed + ?Sized {}

	// Seal any tuple with the right shape. Let the sub-traits of Sealed add the
	// bounds they need. For instance we don't want ElicitsResponse to have to
	// require TakesData on a member of the tuple when that is only relevant to
	// TxMessage. This helps keep the compiler errors smaller and more relevant.
	impl<T, D> Sealed for (u8, T, D) {}
	impl<T> Sealed for (u8, T) {}
}

/// Types that can be encoded as data in a Binary message.
pub trait Data {
	/// The error returned by `try_from_data`.
	type Error;

	/// Fill `buffer` with the encoded data.
	///
	/// The `buffer` will contain at least four bytes.
	fn fill_data(&self, buffer: &mut [u8]);

	/// Decode `Self` from data.
	fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error>
	where
		Self: Sized;
}
impl Data for bool {
	type Error = std::num::TryFromIntError;

	fn fill_data(&self, buffer: &mut [u8]) {
		i32::from(*self).fill_data(buffer);
	}
	fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error> {
		// `bool` doesn't implement `TryFrom` for any integer type (there is
		// debated about what a "true" value is), so do it manually.
		match i32::from_le_bytes(buffer) {
			0 => Ok(false),
			1 => Ok(true),
			// TryFromIntError cannot be constructed outside of `std`, so create
			// one a different way.
			_ => Err(u8::try_from(-1).unwrap_err()),
		}
	}
}
impl Data for u8 {
	type Error = <Self as TryFrom<i32>>::Error;

	fn fill_data(&self, buffer: &mut [u8]) {
		i32::from(*self).fill_data(buffer);
	}
	fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error> {
		Self::try_from(i32::from_le_bytes(buffer))
	}
}
impl Data for i8 {
	type Error = <Self as TryFrom<i32>>::Error;

	fn fill_data(&self, buffer: &mut [u8]) {
		i32::from(*self).fill_data(buffer);
	}
	fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error> {
		Self::try_from(i32::from_le_bytes(buffer))
	}
}
impl Data for u16 {
	type Error = <Self as TryFrom<i32>>::Error;

	fn fill_data(&self, buffer: &mut [u8]) {
		i32::from(*self).fill_data(buffer);
	}
	fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error> {
		Self::try_from(i32::from_le_bytes(buffer))
	}
}
impl Data for i16 {
	type Error = <Self as TryFrom<i32>>::Error;

	fn fill_data(&self, buffer: &mut [u8]) {
		i32::from(*self).fill_data(buffer);
	}
	fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error> {
		Self::try_from(i32::from_le_bytes(buffer))
	}
}
impl Data for i32 {
	type Error = std::convert::Infallible;

	fn fill_data(&self, buffer: &mut [u8]) {
		self.to_le_bytes().fill_data(buffer);
	}
	fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error> {
		Ok(i32::from_le_bytes(buffer))
	}
}
impl Data for [u8; 4] {
	type Error = std::convert::Infallible;

	fn fill_data(&self, buffer: &mut [u8]) {
		for (out_byte, in_byte) in buffer[0..4].iter_mut().zip(self) {
			*out_byte = *in_byte;
		}
	}
	fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error> {
		Ok(buffer)
	}
}

/// Types that can be used as a command code in a Binary message.
pub trait Command: std::convert::TryFrom<u8> + PartialEq<u8> + Copy + private::Sealed {
	/// Get the value of the command as a `u8`.
	fn command(&self) -> u8;
}

impl Command for u8 {
	fn command(&self) -> u8 {
		*self
	}
}

/// Marker trait indicating for commands that are either set or return commands.
pub trait SetOrReturnCommand: Command + Data {}

/// Marker trait for [`Command`] types that indicates the type of the data
/// associated with the command.
///
/// A type should never implement both [`TakesData`] and [`TakesNoData`].
pub trait TakesData<T: Data>: Command {}

impl<T: Data> TakesData<T> for u8 {}

/// Marker trait for [`Command`] types that indicates data is never associated
/// with the command.
///
/// A type should never implement both [`TakesData`] and [`TakesNoData`].
pub trait TakesNoData: Command {}

/// The result of [`ElicitsResponse::expected_command`].
#[derive(Debug)]
pub enum ExpectedCommandResult<T> {
	/// Any response to this message is unexpected.
	AnyUnexpected,
	/// Any response to this message is acceptable.
	AnyAcceptable,
	/// The responses command must exactly match this specified value.
	Exactly(T),
}

/// Message types that will elicit a response response.
//
// In general, types that implement ElicitsResponse should only place bounds on
// the type's structure that is absolutely required and rely on the type bounds
// that TxMessage implementations have to enforce the message structure. This
// helps to produce more informative and succinct compiler errors.
pub trait ElicitsResponse: private::Sealed {
	/// The type of the command in the elicited response.
	type Response: Command;

	/// Get an instance of command in the expected response.
	fn expected_command(&self) -> ExpectedCommandResult<Self::Response>;
}

impl<T> ElicitsResponse for &T
where
	T: ElicitsResponse + ?Sized,
{
	type Response = T::Response;

	fn expected_command(&self) -> ExpectedCommandResult<Self::Response> {
		(*self).expected_command()
	}
}

impl<T> ElicitsResponse for Box<T>
where
	T: ElicitsResponse + ?Sized,
{
	type Response = T::Response;

	fn expected_command(&self) -> ExpectedCommandResult<Self::Response> {
		T::expected_command(self)
	}
}

/// Marker trait indicating the type of data associated with a command code when
/// it is received from a device.
pub trait ReplyData<T: Data>: Command {}

// `u8` commands could be any command and therefore could return any data type.
impl<T: Data> ReplyData<T> for u8 {}

/// Types that describe a Binary message to transmit.
pub trait TxMessage: private::Sealed {
	/// The type of the command.
	type Command: Command;

	/// Populate the given buffer with the encoder Binary message.
	///
	/// `msg` will contain at least six bytes.
	fn populate_message(&self, msg: &mut [u8]);

	/// Get the target device's address.
	fn target(&self) -> u8;

	/// Get the command number.
	fn command(&self) -> u8;
}

impl<T> TxMessage for Box<T>
where
	T: TxMessage + ?Sized,
{
	type Command = T::Command;

	fn populate_message(&self, msg: &mut [u8]) {
		(**self).populate_message(msg);
	}

	fn target(&self) -> u8 {
		(**self).target()
	}

	fn command(&self) -> u8 {
		(**self).command()
	}
}

impl<T> TxMessage for &T
where
	T: TxMessage + ?Sized,
{
	type Command = T::Command;

	fn populate_message(&self, msg: &mut [u8]) {
		(**self).populate_message(msg);
	}

	fn target(&self) -> u8 {
		(**self).target()
	}

	fn command(&self) -> u8 {
		(**self).command()
	}
}

// Messages that take data arguments have the form (address, command, data).
impl<T, D> TxMessage for (u8, T, D)
where
	D: Data,
	T: TakesData<D>,
{
	type Command = T;

	fn populate_message(&self, msg: &mut [u8]) {
		msg[0] = self.0;
		msg[1] = self.1.command();
		self.2.fill_data(&mut msg[2..]);
	}

	fn target(&self) -> u8 {
		self.0
	}

	fn command(&self) -> u8 {
		self.1.command()
	}
}

// Messages that do not take data arguments have the form (address, command).
impl<T> TxMessage for (u8, T)
where
	T: TakesNoData,
{
	type Command = T;

	fn populate_message(&self, msg: &mut [u8]) {
		msg[0] = self.0;
		msg[1] = self.1.command();
		msg[2..].fill(0);
	}

	fn target(&self) -> u8 {
		self.0
	}

	fn command(&self) -> u8 {
		self.1.command()
	}
}
