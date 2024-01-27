//! Convenience marker types

/// Convenience type for marking a Port's lifetime and backend.
#[derive(Debug, Copy, Clone)]
pub(crate) struct Markers<'a, B> {
	_lifetime: std::marker::PhantomData<&'a ()>,
	_backend: std::marker::PhantomData<B>,
}

impl<'a, B> Default for Markers<'a, B> {
	fn default() -> Self {
		Markers {
			_lifetime: std::marker::PhantomData,
			_backend: std::marker::PhantomData,
		}
	}
}
