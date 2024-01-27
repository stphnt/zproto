/// Types for build responses from packets.
use crate::ascii::{parse::Packet, AnyResponse, Response as _};
use crate::error::AsciiUnexpectedPacketError;

/// Information about a response that is either complete or in the process of
/// being built.
#[derive(Debug)]
pub(crate) struct Item {
	/// The first packet in the response. Only present if the response is incomplete.
	packet: Option<Packet>,
	/// The response being built, which may or may not be complete.
	response: AnyResponse,
}

impl Item {
	/// Get a reference to the response being built, it may or may not be complete.
	fn response(&self) -> &AnyResponse {
		&self.response
	}

	/// Get a mutable reference to the response being built, it may or may not be complete.
	fn response_mut(&mut self) -> &mut AnyResponse {
		&mut self.response
	}

	/// Return whether the response being built is complete.
	fn is_complete(&self) -> bool {
		self.packet.is_none()
	}

	/// Mark the response as complete
	fn make_complete(&mut self) {
		self.packet = None;
	}

	/// Try to consume this item and get a complete response.
	///
	/// If this method returns an error, [`try_into_packet`] is guaranteed to succeed.
	fn try_into_complete_response(self) -> Result<AnyResponse, Self> {
		if self.packet.is_none() {
			Ok(self.response)
		} else {
			Err(self)
		}
	}

	/// Try to consume this item and get the packet for the incomplete response.
	///
	/// If this method returns an error, [`try_into_complete_response`] is guaranteed to succeed.
	fn try_into_packet(self) -> Result<Packet, Self> {
		if let Some(packet) = self.packet {
			Ok(packet)
		} else {
			Err(self)
		}
	}
}

/// Used to incrementally build up responses from packets.
///
/// Can build more than one response at a time and handles interleaved packets.
/// Its ability to do so depends to some degree on message IDs being enabled.
#[derive(Debug, Default)]
pub(crate) struct ResponseBuilder {
	items: Vec<Item>,
}

impl ResponseBuilder {
	/// Clear any previous state the builder may be holding.
	pub fn clear(&mut self) {
		self.items.clear();
	}

	/// Push a packet into the builder.
	///
	/// If the packet does not continue an existing message or does not start a
	/// new message, `AsciiUnexpectedPacketError` is returned.
	pub fn push(&mut self, packet: Packet) -> Result<(), AsciiUnexpectedPacketError> {
		use super::private::DataMut as _;

		if packet.cont() {
			// This packet continues another, try to find a matching incomplete
			// message to append it to.
			let mut found = false;
			for item in &mut self.items {
				if !item.is_complete()
					&& item.response().target() == packet.target()
					&& item.response().id() == packet.id()
				{
					// Found it!
					let cont_data = packet.data();
					if !cont_data.is_empty() {
						if !item.response().data().is_empty() {
							item.response_mut().data_mut().push(' ');
						}
						item.response_mut().data_mut().push_str(cont_data);
					}
					if !packet.more_packets() {
						item.make_complete();
					}
					found = true;
					break;
				}
			}
			if !found {
				return Err(AsciiUnexpectedPacketError::new(packet));
			}
		} else {
			// This conversion could fail if, for some very strange reason, we
			// received a command or we made a logic error when parsing the packet.
			let response = match AnyResponse::try_from_packet(&packet) {
				Ok(response) => response,
				Err(_) => return Err(AsciiUnexpectedPacketError::new(packet)),
			};
			self.items.push(Item {
				packet: if packet.more_packets() {
					Some(packet)
				} else {
					None
				},
				response,
			});
		}
		Ok(())
	}

	/// Get the completed response, if it is in fact complete.
	pub fn get_complete_response(&mut self) -> Option<AnyResponse> {
		// Only check if the first message is complete. It may be possible that
		// the next message is complete (perhaps because it had fewer continuation
		// packets or they happened to come earlier), but we want to return the
		// response message that was first received. This also simplifies how
		// we look for completed responses.
		let has_response = self
			.items
			.first()
			.map(|item| item.is_complete())
			.unwrap_or(false);
		if has_response {
			Some(self.items.remove(0).try_into_complete_response().unwrap())
		} else {
			None
		}
	}

	// Get first packet in the incompleted response, if it is in fact incomplete.
	pub fn get_incomplete_response_packet(&mut self) -> Option<Packet> {
		let has_packet = self
			.items
			.first()
			.map(|item| !item.is_complete())
			.unwrap_or(false);
		if has_packet {
			Some(self.items.remove(0).try_into_packet().unwrap())
		} else {
			None
		}
	}
}
