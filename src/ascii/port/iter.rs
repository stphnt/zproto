//! Types for iterating over responses.
use super::HeaderCheck;
use crate::{
	ascii::{check::NotChecked, AnyResponse, Port, Response},
	backend::Backend,
	error::{AsciiCheckError, AsciiError},
};

/// An iterator that will read `N` responses of type `R`
#[derive(Debug)]
#[must_use = "NResponses is an iterator and will not read responses unless consumed."]
pub struct NResponses<'i, 'p, B, R> {
	/// The port to read responses on
	port: &'i mut Port<'p, B>,
	/// The number of responses left to read.
	count: usize,
	/// How to check the headers of the responses.
	header_check: HeaderCheck,
	_marker: std::marker::PhantomData<(B, R)>,
}

impl<'i, 'p, B, R> NResponses<'i, 'p, B, R>
where
	B: Backend,
	R: Response,
{
	/// Create a new `NResponses` iterator.
	pub(super) fn new(port: &'i mut Port<'p, B>, header_check: HeaderCheck, count: usize) -> Self {
		port.pre_receive_response();
		NResponses {
			port,
			count,
			header_check,
			_marker: std::marker::PhantomData,
		}
	}
}

impl<'i, 'p, B, R> Iterator for NResponses<'i, 'p, B, R>
where
	B: Backend,
	R: Response,
	AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
	AsciiError: From<AsciiCheckError<R>>,
{
	type Item = Result<NotChecked<R>, AsciiError>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.count == 0 {
			return None;
		}
		let result = self.port.receive_response(self.header_check);

		self.count -= 1;
		if self.count == 0 {
			Some(self.port.post_receive_response().and(result))
		} else {
			Some(result)
		}
	}
}

/// An iterator that will read responses of type `R` from a port until a read times out.
#[derive(Debug)]
#[must_use = "ResponsesUntilTimeout is an iterator and will not read responses unless consumed."]
pub struct ResponsesUntilTimeout<'i, 'p, B, R> {
	/// The port to read responses on
	port: &'i mut Port<'p, B>,
	/// How to check the headers of the responses.
	header_check: HeaderCheck,
	/// Whether iteration is complete.
	done: bool,
	_marker: std::marker::PhantomData<(B, R)>,
}

impl<'i, 'p, B, R> ResponsesUntilTimeout<'i, 'p, B, R>
where
	B: Backend,
	R: Response,
{
	/// Create a new `ResponsesUntilTimeout` iterator.
	pub(super) fn new(port: &'i mut Port<'p, B>, header_check: HeaderCheck) -> Self {
		port.pre_receive_response();
		ResponsesUntilTimeout {
			port,
			header_check,
			done: false,
			_marker: std::marker::PhantomData,
		}
	}
}

impl<'i, 'p, B, R> Iterator for ResponsesUntilTimeout<'i, 'p, B, R>
where
	B: Backend,
	R: Response,
	AnyResponse: From<<R as TryFrom<AnyResponse>>::Error>,
	AsciiError: From<AsciiCheckError<R>>,
{
	type Item = Result<NotChecked<R>, AsciiError>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.done {
			return None;
		}
		let result = self.port.receive_response(self.header_check);
		match result {
			Err(ref e) if e.is_timeout() => {
				// We're done reading responses. Clean up.
				self.done = true;
				match self.port.post_receive_response() {
					Err(e) => Some(Err(e)),
					Ok(()) => None,
				}
			}
			_ => Some(result),
		}
	}
}
