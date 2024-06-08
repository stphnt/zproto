//! Handlers for events on a port.
#[cfg(doc)]
use super::Port;
use super::{Alert, Direction};

/// A callback that is called after a packet is either transmitted or received.
///
/// See [`Port::set_packet_handler`] for more details.
pub type PacketHandler<'a> = Box<dyn FnMut(&[u8], Direction) + 'a>;

impl<'a, F> crate::convert::From<F> for PacketHandler<'a>
where
    F: FnMut(&[u8], Direction) + 'a,
{
    fn from(value: F) -> Self {
        Box::new(value)
    }
}

/// The same as [`PacketHandler`] but also implements `Send`.
///
/// See [`Port::set_packet_handler`] for more details.
pub type SendPacketHandler<'a> = Box<dyn FnMut(&[u8], Direction) + Send + 'a>;

impl<'a, F> crate::convert::From<F> for SendPacketHandler<'a>
where
    F: FnMut(&[u8], Direction) + Send + 'a,
{
    fn from(value: F) -> Self {
        Box::new(value)
    }
}

/// A callback that is called when an unexpected Alert is received.
///
/// See [`Port::set_unexpected_alert_handler`] for more details.
pub type UnexpectedAlertHandler<'a> = Box<dyn FnMut(Alert) -> Result<(), Alert> + 'a>;

impl<'a, F> crate::convert::From<F> for UnexpectedAlertHandler<'a>
where
    F: FnMut(Alert) -> Result<(), Alert> + 'a,
{
    fn from(value: F) -> Self {
        Box::new(value)
    }
}

/// The same as [`UnexpectedAlertHandler`] but also implements `Send`.
///
/// See [`Port::set_unexpected_alert_handler`] for more details.
pub type SendUnexpectedAlertHandler<'a> = Box<dyn FnMut(Alert) -> Result<(), Alert> + Send + 'a>;

impl<'a, F> crate::convert::From<F> for SendUnexpectedAlertHandler<'a>
where
    F: FnMut(Alert) -> Result<(), Alert> + Send + 'a,
{
    fn from(value: F) -> Self {
        Box::new(value)
    }
}

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

impl<'a> Handlers for LocalHandlers<'a> {
    type PacketHandler = PacketHandler<'a>;
    type UnexpectedAlertHandler = UnexpectedAlertHandler<'a>;
    fn packet(&mut self) -> &mut Option<Self::PacketHandler> {
        &mut self.packet
    }
    fn unexpected_alert(&mut self) -> &mut Option<Self::UnexpectedAlertHandler> {
        &mut self.unexpected_alert
    }
}

impl<'a> private::Sealed for LocalHandlers<'a> {}

/// Implementation detail.
///
/// Collection of event handlers that can be sent to other threads (i.e. they implement `Send`).
#[derive(Default)]
pub struct SendHandlers<'a> {
    pub(super) packets: Option<SendPacketHandler<'a>>,
    pub(super) unexpected_alerts: Option<SendUnexpectedAlertHandler<'a>>,
}

impl std::fmt::Debug for SendHandlers<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SendHandlers").finish_non_exhaustive()
    }
}

impl<'a> Handlers for SendHandlers<'a> {
    type PacketHandler = SendPacketHandler<'a>;
    type UnexpectedAlertHandler = SendUnexpectedAlertHandler<'a>;
    fn packet(&mut self) -> &mut Option<Self::PacketHandler> {
        &mut self.packets
    }
    fn unexpected_alert(&mut self) -> &mut Option<Self::UnexpectedAlertHandler> {
        &mut self.unexpected_alerts
    }
}

impl<'a> private::Sealed for SendHandlers<'a> {}

/// Any type that defines event handlers for a [`Port`].
pub trait Handlers: Default + private::Sealed {
    /// The type of function called when a packet is sent/received.
    type PacketHandler;
    /// The type of function called when an unexpected alert message is received.
    type UnexpectedAlertHandler;

    /// Get the packet handler, if configured.
    fn packet(&mut self) -> &mut Option<Self::PacketHandler>;
    /// Get the unexpected alert handler, if configured.
    fn unexpected_alert(&mut self) -> &mut Option<Self::UnexpectedAlertHandler>;
}

mod private {
    pub trait Sealed {}
}
