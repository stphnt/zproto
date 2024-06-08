//! Event handlers for ports.
#[cfg(doc)]
use super::Port;
use super::{Direction, Message};

/// An event handler that is called after a packet is either transmitted or received.
///
/// See [`Port::set_packet_handler`] for more details.
pub type PacketHandler<'a> = Box<dyn FnMut(&[u8], Message, Direction) + 'a>;

impl<'a, F> crate::convert::From<F> for PacketHandler<'a>
where
    F: FnMut(&[u8], Message, Direction) + 'a,
{
    fn from(value: F) -> Self {
        Box::new(value)
    }
}

/// Deprecated. Use [`PacketHandler`] instead.
#[deprecated = "use the PacketHandler alias instead"]
pub type PacketCallback<'a> = PacketHandler<'a>;

/// Implementation detail.
///
/// A collection of event handlers that can only be used in the local thread.
#[derive(Default)]
pub struct LocalHandlers<'a> {
    pub(super) packet: Option<PacketHandler<'a>>,
}

impl std::fmt::Debug for LocalHandlers<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LocalHandlers").finish_non_exhaustive()
    }
}

impl<'a> Handlers for LocalHandlers<'a> {
    type PacketHandler = PacketHandler<'a>;
    fn packet(&mut self) -> &mut Option<Self::PacketHandler> {
        &mut self.packet
    }
}

/// Any type that defines callbacks for a [`Port`].
pub trait Handlers: Default {
    /// The type of function called when a packet is sent/received.
    type PacketHandler;

    /// Get the packet callback, if configured.
    fn packet(&mut self) -> &mut Option<Self::PacketHandler>;
}
