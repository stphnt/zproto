//! The ASCII info message type.

use crate::ascii::{
	response::{parse, AnyResponse, Header, Response, SpecificResponse},
	Target,
};

/// The contents of an [`Info`] message.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct InfoInner {
	pub target: Target,
	pub id: Option<u8>,
	pub data: String,
}

/// A decoded Zaber ASCII Info message.
#[derive(Debug, Clone, PartialEq)]
pub struct Info(pub(super) Box<InfoInner>);

impl Info {
	/// Try to convert a packet into an Info message.
	///
	/// The conversion will fail if the packet is the wrong kind or if the packet
	/// is not the start of a message. The packet does not need to complete the
	/// message.
	pub(crate) fn try_from_packet<T>(packet: &parse::Packet<T>) -> Result<Self, &parse::Packet<T>>
	where
		T: AsRef<[u8]>,
	{
		if packet.kind() != parse::PacketKind::Info || packet.cont() {
			return Err(packet);
		}
		Ok(InfoInner {
			target: packet.target(),
			id: packet.id(),
			data: packet.data().to_string(),
		}
		.into())
	}

	/// The device and axis number the Info message came from.
	pub fn target(&self) -> Target {
		self.0.target
	}
	/// The message ID, if any.
	pub fn id(&self) -> Option<u8> {
		self.0.id
	}
	/// The message's data.
	pub fn data(&self) -> &str {
		self.0.data.as_str()
	}
}

impl From<InfoInner> for Info {
	fn from(inner: InfoInner) -> Self {
		Info(Box::new(inner))
	}
}

impl std::convert::TryFrom<AnyResponse> for Info {
	type Error = AnyResponse;
	fn try_from(response: AnyResponse) -> Result<Self, Self::Error> {
		if let AnyResponse::Info(info) = response {
			Ok(info)
		} else {
			Err(response)
		}
	}
}

impl std::fmt::Display for Info {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", char::from(parse::INFO_MARKER))?;
		Header {
			address: self.target().device(),
			axis: self.target().axis(),
			id: self.id(),
		}
		.fmt(f)?;
		if !self.data().is_empty() {
			write!(f, " {}", self.data())?;
		}
		Ok(())
	}
}

impl Response for Info {
	fn target(&self) -> Target {
		self.target()
	}
	fn id(&self) -> Option<u8> {
		self.id()
	}
	fn data(&self) -> &str {
		self.data()
	}
}

impl SpecificResponse for Info {}
