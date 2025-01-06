//! Types for iterating over responses.
use super::{handlers::Handlers, HeaderCheck};
use crate::{
	ascii::{
		command::{Command, Target},
		port::Direction,
		response::{check::NotChecked, Alert, AnyResponse, Info, Reply, Response},
		Port,
	},
	backend::Backend,
	error::{AsciiCheckError, AsciiError},
};

/// An iterator that will read `N` responses of type `R`
#[must_use = "NResponses is an iterator and will not read responses unless consumed."]
pub struct NResponses<'i, 'p, B, R, Tag, H> {
	/// The port to read responses on
	port: &'i mut Port<'p, B, Tag, H>,
	/// The number of responses left to read.
	count: usize,
	/// How to check the headers of the responses.
	header_check: HeaderCheck,
	_marker: std::marker::PhantomData<(B, R)>,
}

impl<B, R, Tag, H> std::fmt::Debug for NResponses<'_, '_, B, R, Tag, H> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("NResponses").finish_non_exhaustive()
	}
}

impl<'i, 'p, B, R, Tag, H> NResponses<'i, 'p, B, R, Tag, H>
where
	B: Backend,
	R: Response,
	H: Handlers,
	H::PacketHandler: FnMut(&[u8], Direction) + 'p,
	H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'p,
{
	/// Create a new `NResponses` iterator.
	pub(super) fn new(
		port: &'i mut Port<'p, B, Tag, H>,
		header_check: HeaderCheck,
		count: usize,
	) -> Self {
		port.pre_receive_response();
		NResponses {
			port,
			count,
			header_check,
			_marker: std::marker::PhantomData,
		}
	}
}

impl<'p, B, R, Tag, H> Iterator for NResponses<'_, 'p, B, R, Tag, H>
where
	B: Backend,
	R: Response,
	H: Handlers,
	H::PacketHandler: FnMut(&[u8], Direction) + 'p,
	H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'p,
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
#[must_use = "ResponsesUntilTimeout is an iterator and will not read responses unless consumed."]
pub struct ResponsesUntilTimeout<'i, 'p, B, R, Tag, H> {
	/// The port to read responses on
	port: &'i mut Port<'p, B, Tag, H>,
	/// How to check the headers of the responses.
	header_check: HeaderCheck,
	/// Whether iteration is complete.
	done: bool,
	_marker: std::marker::PhantomData<(B, R)>,
}

impl<B, R, Tag, H> std::fmt::Debug for ResponsesUntilTimeout<'_, '_, B, R, Tag, H> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("ResponsesUntilTimeout")
			.finish_non_exhaustive()
	}
}

impl<'i, 'p, B, R, Tag, H> ResponsesUntilTimeout<'i, 'p, B, R, Tag, H>
where
	B: Backend,
	R: Response,
	H: Handlers,
	H::PacketHandler: FnMut(&[u8], Direction) + 'p,
	H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'p,
{
	/// Create a new `ResponsesUntilTimeout` iterator.
	pub(super) fn new(port: &'i mut Port<'p, B, Tag, H>, header_check: HeaderCheck) -> Self {
		port.pre_receive_response();
		ResponsesUntilTimeout {
			port,
			header_check,
			done: false,
			_marker: std::marker::PhantomData,
		}
	}
}

impl<'p, B, R, Tag, H> Iterator for ResponsesUntilTimeout<'_, 'p, B, R, Tag, H>
where
	B: Backend,
	R: Response,
	H: Handlers,
	H::PacketHandler: FnMut(&[u8], Direction) + 'p,
	H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'p,
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

/// An iterator that will read info message until a reply from a prior command is
/// received.
///
/// See [`Port::command_reply_infos_iter`](super::Port::command_reply_infos_iter) for details.
pub struct InfosUntilSentinel<'i, 'p, B, Tag, H> {
	port: &'i mut Port<'p, B, Tag, H>,
	header_check: HeaderCheck,
	done: bool,
}

impl<B, Tag, H> std::fmt::Debug for InfosUntilSentinel<'_, '_, B, Tag, H> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("InfosUntilSentinel").finish_non_exhaustive()
	}
}

impl<'i, 'p, B, Tag, H> InfosUntilSentinel<'i, 'p, B, Tag, H>
where
	B: Backend,
	H: Handlers,
	H::PacketHandler: FnMut(&[u8], Direction) + 'p,
	H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'p,
{
	/// Create a new `InfosUntilSentinel` instance.
	pub(crate) fn new(
		port: &'i mut Port<'p, B, Tag, H>,
		target: Target,
		info_id: Option<u8>,
		sentinel_id: Option<u8>,
	) -> Self {
		port.pre_receive_response();
		InfosUntilSentinel {
			port,
			header_check: HeaderCheck::InfoSentinelReplyMatches {
				target,
				info_id,
				sentinel_id,
			},
			done: false,
		}
	}
}

impl<'p, B, Tag, H> Iterator for InfosUntilSentinel<'_, 'p, B, Tag, H>
where
	B: Backend,
	H: Handlers,
	H::PacketHandler: FnMut(&[u8], Direction) + 'p,
	H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'p,
{
	type Item = Result<NotChecked<Info>, AsciiError>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.done {
			return None;
		}
		match self.port.receive_response(self.header_check) {
			Ok(response) => {
				match response.into_inner() {
					AnyResponse::Info(info) => Some(Ok(NotChecked::new(info))),
					AnyResponse::Reply(_) => {
						self.done = true;
						if let Err(e) = self.port.post_receive_response() {
							Some(Err(e))
						} else {
							None
						}
					}
					// The header check should ensure all other response kinds
					// are errors.
					AnyResponse::Alert(_) => unreachable!(),
				}
			}
			Err(e) => {
				self.done = true;
				// Clean up if we can. However, if we encounter an error during
				// clean up we should always return the original error as it is
				// more important.
				let _ = self.port.post_receive_response();
				Some(Err(e))
			}
		}
	}
}

/// An iterator that will continually send a command and read a reply.
pub struct Poll<'i, 'p, B, C, Tag, H> {
	pub(super) port: &'i mut Port<'p, B, Tag, H>,
	pub(super) command: C,
}

impl<B, C, Tag, H> std::fmt::Debug for Poll<'_, '_, B, C, Tag, H> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Poll").finish_non_exhaustive()
	}
}

impl<'p, B, C, Tag, H> Iterator for Poll<'_, 'p, B, C, Tag, H>
where
	B: Backend,
	C: Command,
	H: Handlers,
	H::PacketHandler: FnMut(&[u8], Direction) + 'p,
	H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'p,
{
	type Item = Result<NotChecked<Reply>, AsciiError>;

	fn next(&mut self) -> Option<Self::Item> {
		Some(self.port.command_reply(&self.command))
	}
}
