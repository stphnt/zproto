//! Types and traits for marking the scope of a type (i.e. whether an item is axis-scope or device-scope).
//!
//! The scope of an item is determined by whether it implements [`AxisScope`] or [`DeviceScope`].
//!
//! In some cases, types/functions need to change the required scope of their parameters based on a type
//! parameter. In such cases, the [`RequiresAxisScope`] and [`RequiresDeviceScope`] types can be used in
//! conjunction with either of the [`RequiredScopeSatisfiedBy`] or [`SatisfiesRequiredScope`] trait bounds:
//!
//! ```
//! # use zproto::ascii::chain::scope::*;
//! struct Foo<R>(std::marker::PhantomData<R>);
//! impl<R> Foo<R> {
//!     fn bar<T: SatisfiesRequiredScope<R>>(_: T) { /* ... */ }
//! }
//!
//! struct AxisThing;
//! impl AxisScope for AxisThing {};
//!
//! struct DeviceThing;
//! impl DeviceScope for DeviceThing {};
//!
//! Foo::<RequiresAxisScope>::bar(AxisThing);      // OK
//! // Foo::<RequiresAxisScope>::bar(DeviceThing); // Compilation error.
//! // Foo::<RequiresDeviceScope>::bar(AxisThing); // Compilation error.
//! Foo::<RequiresDeviceScope>::bar(DeviceThing);  // OK
//! ```
// It may be tempting to replace the scope requirement types and associated traits with one trait,
// something like:
//
// ```
// pub trait SameScope<A, B> {}
// ```
//
// And then implement it for all types A and B where both types implement the same scope trait.
// However, this will result in conflicting implementations,
//
// ```
// impl<A, B> SameScope<A, B> for A where A: DeviceScope, B: DeviceScope {}
// impl<A, B> SameScope<A, B> for A where A: AxisScope, B: AxisScope {} // conflicting
// implementation
// ```
//
// To avoid this, the traits need to be implement on concrete types, hence the Requires*Scope
// types.

/// A marker trait indicating that a type represents an axis-scope item.
pub trait AxisScope {}
impl<T> AxisScope for &T where T: AxisScope {}
impl<T> AxisScope for &mut T where T: AxisScope {}

/// A marker trait indicating that a type represents a device-scope item.
pub trait DeviceScope {}
impl<T> DeviceScope for &T where T: DeviceScope {}
impl<T> DeviceScope for &mut T where T: DeviceScope {}

/// A marker type indicating a type requires other types to satisfy an [`AxisScope`] bound.
///
/// To fully achieve this behaviour, it must be used in conjunction with one of
/// [`RequiredScopeSatisfiedBy`] or [`SatisfiesRequiredScope`]. See the [module] level
/// documentation for details.
///
/// [module]: crate::ascii::chain::scope
#[derive(Debug)]
pub enum RequiresAxisScope {}
impl AxisScope for RequiresAxisScope {}

/// A marker type indicating a type requires other types to satisfy a [`DeviceScope`] bounds.
///
/// To fully achieve this behaviour, it must be used in conjunction with one of
/// [`RequiredScopeSatisfiedBy`] or [`SatisfiesRequiredScope`]. See the [module] level
/// documentation for details.
///
/// [module]: crate::ascii::chain::scope
#[derive(Debug)]
pub enum RequiresDeviceScope {}
impl DeviceScope for RequiresDeviceScope {}

/// A marker trait indicating a type indicating a scope requirement is satisfied by the type `T`.
///
/// This allows the scope requirement to be defined by a type parameter, otherwise the
/// [`AxisScope`] or [`DeviceScope`] traits should be used as a bound directly.
///
/// ```
/// # use zproto::ascii::chain::scope::RequiredScopeSatisfiedBy;
/// # fn foo<T, R>(_: T, _: R)
/// where
///     R: RequiredScopeSatisfiedBy<T>
///     // Read as "The scope required by type R is satisfied by type T," where R must be
///     // either RequiresAxisScope or RequiresDeviceScope.
/// # {}
/// ```
///
/// The inverse bound is defined by [`SatisfiesRequiredScope`].
///
/// See the [module] level documentation for more details.
///
/// [module]: crate::ascii::chain::scope
pub trait RequiredScopeSatisfiedBy<T> {}

impl<T> RequiredScopeSatisfiedBy<T> for RequiresAxisScope where T: AxisScope {}
impl<T> RequiredScopeSatisfiedBy<T> for RequiresDeviceScope where T: DeviceScope {}

/// A marker trait indicating a type satisfies the scope requirement of `T`, which must be either
/// [`RequiresAxisScope`] and [`RequiresDeviceScope`].
///
/// This allows the scope requirement to be defined by a type parameter, otherwise the
/// [`AxisScope`] or [`DeviceScope`] traits should be used as a bound directly.
///
/// ```
/// # use zproto::ascii::chain::scope::SatisfiesRequiredScope;
/// # fn foo<T, R>(_: T, _: R)
/// where
///     T: SatisfiesRequiredScope<R>
///     // Read as "T satisfies the scope required by type R," where R must be either
///     // RequiresScopeAxis or RequiresDeviceAxis.
/// # {}
/// ```
///
/// The inverse bound is defined by [`RequiredScopeSatisfiedBy`].
///
/// See the [module] level documentation for more details.
///
/// [module]: crate::ascii::chain::scope
pub trait SatisfiesRequiredScope<T> {}

impl<T> SatisfiesRequiredScope<RequiresAxisScope> for T where T: AxisScope {}
impl<T> SatisfiesRequiredScope<RequiresDeviceScope> for T where T: DeviceScope {}
