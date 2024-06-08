//! Handlers for events on a port.
#[cfg(doc)]
use super::Port;
use super::{Alert, Direction};

/// A callback that is called after a packet is either transmitted or received.
///
/// See [`Port::set_packet_handler`] for more details.
pub type PacketHandler<'a> = Box<dyn FnMut(&[u8], Direction) + 'a>;

/// A callback that is called when an unexpected Alert is received.
///
/// See [`Port::set_unexpected_alert_handler`] for more details.
pub type UnexpectedAlertHandler<'a> = Box<dyn FnMut(Alert) -> Result<(), Alert> + 'a>;

/// Implementation detail.
///
/// Collection of event handlers that can only be used in the local thread.
#[derive(Default)]
pub struct LocalHandlers<'a> {
    pub(super) packet: Option<PacketHandler<'a>>,
    pub(super) unexpected_alert: Option<UnexpectedAlertHandler<'a>>,
}

impl std::fmt::Debug for LocalHandlers<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LocalHandlers").finish_non_exhaustive()
    }
}
