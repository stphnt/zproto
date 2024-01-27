//! Traits to accesses data through different sharing mechanisms.

use crate::error::{LockError, LockPoisonedError, LockUnavailableError};
use std::{
	cell::{Ref, RefCell, RefMut},
	convert::Infallible,
	ops::{Deref, DerefMut},
	rc::Rc,
	sync::{Arc, Mutex, MutexGuard},
};

/// Any type that can be shared, either within a thread or across threads.
pub trait Shared<T>: Clone {
	/// The type protecting the shared resource
	type Guard<'g>: Deref<Target = T>
	where
		Self: 'g;

	/// The error type representing when the port locking mechanism has failed.
	type PoisonError<'e>: std::fmt::Debug
	where
		Self: 'e;

	/// The type protecting the shared resource.
	type Wrapper<S>: Shared<S>;

	/// Create a new instance of the shared type in the unlocked state.
	fn new(value: T) -> Self;
	/// Try to lock the underlying resource
	fn try_lock(&self) -> Result<Self::Guard<'_>, LockError>;
	/// Lock the underlying resource.
	///
	/// In multithreaded contexts, this will block the current thread until the
	/// resource is available.
	fn lock(&self) -> Result<Self::Guard<'_>, Self::PoisonError<'_>>;
	/// Consume the shared value and return it if there are no other shared references to it.
	/// If there are other references, `None` is returned and the value is dropped.
	fn into_inner(this: Self) -> Option<T>;
}

/// Any type that can be mutably shared, either within a thread or across threads.
pub trait SharedMut<T>: Shared<T> {
	/// The type protecting the shared resource
	type GuardMut<'g>: DerefMut<Target = T>
	where
		Self: 'g;

	/// Try to lock the underlying resource
	fn try_lock_mut(&self) -> Result<Self::GuardMut<'_>, LockError>;
	/// Lock the underlying resource.
	///
	/// In multithreaded contexts, this will block the current thread until the
	/// resource is available.
	fn lock_mut(&self) -> Result<Self::GuardMut<'_>, Self::PoisonError<'_>>;
}

impl<T> Shared<T> for Rc<RefCell<T>> {
	type Guard<'g> = Ref<'g, T> where Self: 'g;
	type PoisonError<'e> = Infallible where Self: 'e;
	type Wrapper<S> = Rc<RefCell<S>>;

	fn new(value: T) -> Self {
		Rc::new(RefCell::new(value))
	}
	fn try_lock(&self) -> Result<Self::Guard<'_>, LockError> {
		self.try_borrow().map_err(|_| LockUnavailableError.into())
	}
	fn lock(&self) -> Result<Self::Guard<'_>, Self::PoisonError<'_>> {
		Ok(self.borrow())
	}
	fn into_inner(this: Self) -> Option<T> {
		Rc::into_inner(this).map(RefCell::into_inner)
	}
}

impl<T> SharedMut<T> for Rc<RefCell<T>> {
	type GuardMut<'g> = RefMut<'g, T> where Self: 'g;

	fn try_lock_mut(&self) -> Result<Self::GuardMut<'_>, LockError> {
		self.try_borrow_mut()
			.map_err(|_| LockUnavailableError.into())
	}
	fn lock_mut(&self) -> Result<Self::GuardMut<'_>, Self::PoisonError<'_>> {
		Ok(self.borrow_mut())
	}
}

impl<T> Shared<T> for Arc<Mutex<T>> {
	type Guard<'g> = MutexGuard<'g, T> where Self: 'g;
	type PoisonError<'e> = std::sync::PoisonError<Self::Guard<'e>> where Self: 'e;
	type Wrapper<S> = Arc<Mutex<S>>;

	fn new(value: T) -> Self {
		Arc::new(Mutex::new(value))
	}
	fn try_lock(&self) -> Result<Self::Guard<'_>, LockError> {
		use std::sync::TryLockError;
		Mutex::try_lock(self).map_err(|err| match err {
			TryLockError::Poisoned(_) => LockPoisonedError.into(),
			TryLockError::WouldBlock => LockUnavailableError.into(),
		})
	}
	fn lock(&self) -> Result<Self::Guard<'_>, Self::PoisonError<'_>> {
		Mutex::lock(self)
	}
	fn into_inner(this: Self) -> Option<T> {
		Arc::into_inner(this).and_then(|mutex| Mutex::into_inner(mutex).ok())
	}
}

impl<T> SharedMut<T> for Arc<Mutex<T>> {
	type GuardMut<'g> = MutexGuard<'g, T> where Self: 'g;

	fn try_lock_mut(&self) -> Result<Self::GuardMut<'_>, LockError> {
		use std::sync::TryLockError;
		Mutex::try_lock(self).map_err(|err| match err {
			TryLockError::Poisoned(_) => LockPoisonedError.into(),
			TryLockError::WouldBlock => LockUnavailableError.into(),
		})
	}
	fn lock_mut(&self) -> Result<Self::GuardMut<'_>, Self::PoisonError<'_>> {
		Mutex::lock(self)
	}
}
