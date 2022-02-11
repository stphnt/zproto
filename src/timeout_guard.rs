//! A "scope guard" that will reset a port's timeout when it is goes out of scope.

use crate::backend::Backend;
use std::{io, marker::PhantomData, time::Duration};

/// A port, as required by the [`TimeoutGuard`].
pub trait Port<B>: private::Sealed {
    /// Get the underlying backend.
    #[doc(hidden)]
    fn backend_mut(&mut self) -> &mut B;
    /// Poison the port.
    #[doc(hidden)]
    fn poison(&mut self, e: io::Error);
}

mod private {
    /// Marks a trait a sealed.
    pub trait Sealed {}
    #[cfg(feature = "ascii")]
    impl<B> Sealed for crate::ascii::Port<B> {}
    #[cfg(feature = "binary")]
    impl<B> Sealed for crate::binary::Port<B> {}
}

/// A "scope guard" that will update the port's timeout and then reset it when
/// it goes out of scope.
///
/// To create a guard, use the port's [`timeout_guard`](crate::ascii::Port::timeout_guard) method.
///
/// While the guard is in scope, the port can only be accessed through the guard.
/// However, because the guard implements [`Deref`](std::ops::Deref) and
/// [`DerefMut`](std::ops::DerefMut) callers can treat the guard as the port.
#[derive(Debug)]
pub struct TimeoutGuard<'a, B: Backend, P: Port<B>> {
    /// The underlying port.
    port: &'a mut P,
    /// The original timeout that will be restored when the guard is dropped.
    original_timeout: Option<Duration>,
    backend_marker: PhantomData<B>,
}

impl<'a, B: Backend, P: Port<B>> TimeoutGuard<'a, B, P> {
    /// Update the port's timeout and return a [`TimeoutGuard`] wrapping the port.
    pub(crate) fn new(port: &'a mut P, timeout: Option<Duration>) -> Result<Self, io::Error> {
        let backend = port.backend_mut();
        let original_timeout = backend.read_timeout()?;
        backend.set_read_timeout(timeout)?;
        Ok(TimeoutGuard {
            port,
            original_timeout,
            backend_marker: PhantomData,
        })
    }
}

impl<'a, B: Backend, P: Port<B>> std::ops::Deref for TimeoutGuard<'a, B, P> {
    type Target = P;
    /// Get a shared reference to the underlying port.
    fn deref(&self) -> &Self::Target {
        self.port
    }
}

impl<'a, B: Backend, P: Port<B>> std::ops::DerefMut for TimeoutGuard<'a, B, P> {
    /// Get an exclusive reference to the underlying port.
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.port
    }
}

impl<'a, B: Backend, P: Port<B>> std::ops::Drop for TimeoutGuard<'a, B, P> {
    fn drop(&mut self) {
        if let Err(err) = self
            .port
            .backend_mut()
            .set_read_timeout(self.original_timeout)
        {
            self.port.poison(io::Error::new(
                io::ErrorKind::Other,
                if let Some(timeout) = self.original_timeout {
                    format!(
                        "failed to reset timeout to {} seconds: {}",
                        timeout.as_secs(),
                        err
                    )
                } else {
                    format!("failed to reset to an infinite timeout: {}", err)
                },
            ));
        }
    }
}
