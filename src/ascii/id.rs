//! Generation of ASCII message IDs

use crate::ascii::Command;

/// A trait for types that can generate ASCII message IDs.
pub trait Generator {
    /// Generate a new ASCII message ID.
    ///
    /// The message ID must be in [0, 99] and should be generated such that collisions with previous message IDs are minimized.
    fn next_id(&mut self, command: &Command) -> u8;
}

impl<T> Generator for &mut T
where
    T: Generator,
{
    fn next_id(&mut self, command: &Command) -> u8 {
        (*self).next_id(command)
    }
}

/// An ASCII message ID generator that selects IDs based on a counter
#[derive(Debug, Default)]
pub struct Counter {
    /// The last generated message ID.
    last_id: u8,
}

impl Generator for Counter {
    fn next_id(&mut self, _: &Command) -> u8 {
        self.last_id = (self.last_id + 1) % 100;
        self.last_id
    }
}
