//! The [`Routine`] trait and supporting types.
//!
//! A [`Routine`] represents types that can perform work when they are given
//! exclusive access to a shared resource via their [`run`] method. This resource
//! can be anything, but in this crate we are primarily concerned with serial/TCP
//! ports.
//!
//! [`Routine`]s are lazy and perform no work until their [`run`] method is called.
//! The compiler will raise an error if a [`Routine`] is not used.
//!
//! ```
//! # use zproto::routine::Routine;
//! # fn wrapper(mut r: impl Routine<(), Output = (), Error = ()>) -> Result<(), ()> {
//! let mut routine = // ..
//! # r;
//! # let resource = &mut ();
//! let output = routine.run(resource)?; // This is where the work actually happens
//! # Ok(())
//! # }
//! ```
//!
//! ## Composing routines
//!
//! [`Routine`]s can be composed via the trait's provided methods, similar to
//! how [`Iterator`]s can be composed. For instance, the [`and`] or [`and_then`]
//! methods will run one routine and, if it succeeds, then run a second routine.
//! The [`or`] or [`or_else`] methods, are similar except the second routine is
//! only run if the first fails. These methods are named after the logical
//! operators whose behaviour they mirror.
//!
//! ```
//! # use zproto::routine::Routine;
//! # fn wrapper(
//! #     mut routine1: impl Routine<(), Output = (), Error = ()> + Copy,
//! #     mut routine2: impl Routine<(), Output = (), Error = ()> + Copy,
//! #     mut routine3: impl Routine<(), Output = (), Error = ()> + Copy,
//! # ) -> Result<(), ()> {
//! # let resource = &mut ();
//! // Returns the first failure, or the output of the last routine.
//! let output_of_last = routine1
//!     .and(routine2)
//!     .and(routine3)
//!     .run(resource)?;
//!
//! // Returns the output of the first routine to succeed or the final error.
//! let first_output = routine1
//!     .or(routine2)
//!     .or(routine3)
//!     .run(resource)?;
//! # Ok(())
//! # }
//! ```
//!
//! To get the output of multiple routines, they can each be individually run or
//! they can be [`join`]ed. A [`Seq`] can also join a sequence of routines, but
//! the sequences can be a `tuple`, `slice`, `array`, or `Vec`, as well.
//!
//! ```
//! use zproto::routine::{Routine, Seq};
//! # fn wrapper(
//! #     mut routine1: impl Routine<(), Output = (), Error = ()> + Copy,
//! #     mut routine2: impl Routine<(), Output = (), Error = ()> + Copy,
//! #     mut routine3: impl Routine<(), Output = (), Error = ()> + Copy,
//! # ) -> Result<(), ()> {
//! # let resource = &mut ();
//!
//! let output1 = routine1.run(resource)?;
//! let output2 = routine2.run(resource)?;
//! let output3 = routine3.run(resource)?;
//!
//! // Or equivalently
//! let ((output1, output2), output3) =
//!     routine1.join(routine2).join(routine3).run(resource)?;
//!
//! // Or even
//! let (output1, output2, routine3) =
//!     Seq((routine1, routine2, routine3)).run(resource)?;
//! # Ok(())
//! # }
//! ```
//!
//! ## Why routines
//!
//! Shared exclusive/mutable resources in Rust are very strictly managed (for
//! good reason). Rust provides good mechanisms for managing them statically
//! with the borrow checker and dynamically with types like `Rc<RefCell<T>>` and
//! `Arc<Mutex<T>>`. However, each method has its tradeoffs and perform better/
//! worse in different contexts. So this library avoids picking any one sharing
//! mechanism, but rather exposes an API via routines that lets users, who
//! understand the needs of their applications best, to pick the appropriate
//! mechanism.
//!
//! [`run`]: Routine::run
//! [`and`]: Routine::and
//! [`and_then`]: Routine::and_then
//! [`or`]: Routine::or
//! [`or_else`]: Routine::or_else
//! [`join`]: Routine::join

mod seq;
pub use seq::*;

/// Represents a type that can perform a routine/function on a mutable resource `R`.
///
/// See the [`module`] level documentation for more details.
///
/// [`module`]: crate::routine
#[must_use = "routines are lazy and do nothing unless consumed"]
pub trait Routine<R> {
	/// The output of the routine
	type Output;
	/// The error returned by the routine
	type Error;

	/// Execute the routine using the provided resource.
	fn run(&mut self, resource: &'_ mut R) -> Result<Self::Output, Self::Error>;

	/// Create a new routine that runs this routine and if it is successful, runs
	/// the `next` routine. If the first routine returns an `Err`, the `next`
	/// routine is not run.
	///
	/// The behaviour is the same as how logical AND (`&&`) evaluates two conditions.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::routine::Routine;
	/// # type A = i32;
	/// # type B = u32;
	/// # type ErrA = ();
	/// # fn wrapper(
	/// #     mut first_routine: impl Routine<(), Output = A, Error = ErrA>,
	/// #     mut second_routine: impl Routine<(), Output = B, Error = ErrA> + Copy,
	/// # ) -> Result<(), ()> {
	/// # let res = &mut ();
	/// let _: Result<A, ErrA> = first_routine.run(res);
	/// let _: Result<B, ErrA> = second_routine.run(res);
	///
	/// // Run first_routine and then, if that succeeds, run second_routine and return
	/// // the result.
	/// let _: Result<B, ErrA> = first_routine
	///     .and(second_routine)
	///     .run(res);
	/// # Ok(())
	/// # }
	/// ```
	fn and<T>(self, next: T) -> And<Self, T>
	where
		Self: Sized,
		T: Routine<R, Error = Self::Error>,
	{
		And {
			first: self,
			second: next,
		}
	}

	/// Create a new routine that runs this routine and if it is successful,
	/// calls the function `f` with the `Ok` value to get the next routine to
	/// run. If the first routine returns `Err`, the function `f` is not called.
	///
	/// The behaviour is very similar to [`and`], except that you can get access
	/// to the `Ok` value returned by the first routine.
	///
	/// [`and`]: Routine::and
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::routine::Routine;
	/// # type A = i32;
	/// # type B = u32;
	/// # type ErrA = ();
	/// # fn wrapper(
	/// #     mut first_routine: impl Routine<(), Output = A, Error = ErrA>,
	/// #     mut second_routine: impl Routine<(), Output = B, Error = ErrA> + Clone,
	/// # ) -> Result<(), ()> {
	/// # let res = &mut ();
	/// let _: Result<A, ErrA> = first_routine.run(res);
	/// let _: Result<B, ErrA> = second_routine.run(res);
	///
	/// // Run first_routine and then, if that succeeds, run second_routine and
	/// // return its result.
	/// let _: Result<B, ErrA> = first_routine
	///     .and_then(|_: A| second_routine.clone())
	///     .run(res);
	/// # Ok(())
	/// # }
	/// ```
	fn and_then<F, T>(self, f: F) -> AndThen<Self, F>
	where
		Self: Sized,
		F: FnMut(<Self as Routine<R>>::Output) -> T,
		T: Routine<R, Error = Self::Error>,
	{
		AndThen { routine: self, f }
	}

	/// Create a new routine that runs this routine and if it is fails, runs
	/// the `next` routine. If the first routine returns `Ok`, the `next`
	/// routine is not run.
	///
	/// The behaviour is the same as how logical OR (`||`) evaluates two conditions.
	///
	/// ## Examples
	///
	/// ```
	/// # use zproto::routine::Routine;
	/// # type A = i32;
	/// # type ErrA = std::convert::Infallible;
	/// # type ErrB = ();
	/// # fn wrapper(
	/// #     mut first_routine: impl Routine<(), Output = A, Error = ErrA>,
	/// #     mut second_routine: impl Routine<(), Output = A, Error = ErrB> + Copy,
	/// # ) -> Result<(), ()> {
	/// # let res = &mut ();
	/// let _: Result<A, ErrA> = first_routine.run(res);
	/// let _: Result<A, ErrB> = second_routine.run(res);
	///
	/// // Run first_routine and return its result. Or, if it fails, run the
	/// // second_routine and return its result instead.
	/// let _: Result<A, ErrB> = first_routine
	///     .or(second_routine)
	///     .run(res);
	/// # Ok(())
	/// # }
	/// ```
	fn or<T>(self, other: T) -> Or<Self, T>
	where
		Self: Sized,
		T: Routine<R, Output = Self::Output>,
	{
		Or {
			first: self,
			second: other,
		}
	}

	/// Create a new routine that runs this routine and if it is fails, runs
	/// the `next` routine. If the first routine returns `Ok`, the `next`
	/// routine is not run.
	///
	/// The behaviour is very similar to [`or`], except that you can get access
	/// to the `Err` value returned by the first routine.
	///
	/// [`or`]: Routine::or
	///
	/// ## Examples
	///
	/// ```
	/// # use zproto::routine::Routine;
	/// # type A = i32;
	/// # type B = u32;
	/// # type ErrA = f32;
	/// # type ErrB = f64;
	/// # fn wrapper(
	/// #     mut first_routine: impl Routine<(), Output = A, Error = ErrA>,
	/// #     mut second_routine: impl Routine<(), Output = A, Error = ErrB> + Copy,
	/// # ) -> Result<(), ()> {
	/// # let res = &mut ();
	/// let _: Result<A, ErrA> = first_routine.run(res);
	/// let _: Result<A, ErrB> = second_routine.run(res);
	///
	/// // Run first_routine and return its result. Or else, if it fails, run
	/// // the second_routine and return its result instead.
	/// let _: Result<A, ErrB> = first_routine
	///     .or_else(|_: ErrA| second_routine.clone())
	///     .run(res);
	/// # Ok(())
	/// # }
	/// ```
	fn or_else<F, T>(self, f: F) -> OrElse<Self, F>
	where
		Self: Sized,
		F: FnMut(<Self as Routine<R>>::Error) -> T,
		T: Routine<R, Output = Self::Output>,
	{
		OrElse { routine: self, f }
	}

	/// Create a new routine that will run this routine and then, if it returns
	/// an `Ok` value, maps it to some new type `T` by applying the function `f`.
	/// If the routine returns an `Err` value, it is untouched.
	///
	/// ## Examples
	///
	/// ```
	/// # use zproto::routine::Routine;
	/// # type A = i32;
	/// # type B = i64;
	/// # type ErrA = std::convert::Infallible;
	/// # fn wrapper(
	/// #     mut routine: impl Routine<(), Output = A, Error = ErrA>,
	/// # ) -> Result<(), ()> {
	/// # let res = &mut ();
	/// let _: Result<A, ErrA> = routine.run(res);
	/// let _: Result<B, ErrA> = routine.map(B::from).run(res);
	/// # Ok(())
	/// # }
	/// ```
	fn map<F, T>(self, f: F) -> Map<Self, F>
	where
		Self: Sized,
		F: FnMut(Self::Output) -> T,
	{
		Map { routine: self, f }
	}

	/// Create a new routine that will run this routine and then, if it returns
	/// an `Err` value, maps it to some new type `E` by applying the function `f`.
	/// If the routine returns an `Ok` value, it is untouched.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::routine::Routine;
	/// # type A = i32;
	/// # type ErrA = std::convert::Infallible;
	/// # struct ErrB;
	/// # impl From<ErrA> for ErrB {
	/// #     fn from(other: ErrA) -> Self { todo!() }
	/// # }
	/// # fn wrapper(
	/// #     mut routine: impl Routine<(), Output = A, Error = ErrA>,
	/// # ) -> Result<(), ()> {
	/// # let res = &mut ();
	/// #
	/// let _: Result<A, ErrA> = routine.run(res);
	/// let _: Result<A, ErrB> = routine.map_err(ErrB::from).run(res);
	/// # Ok(())
	/// # }
	/// ```
	fn map_err<F, T>(self, f: F) -> MapErr<Self, F>
	where
		Self: Sized,
		F: FnMut(Self::Error) -> T,
	{
		MapErr { routine: self, f }
	}

	/// Create a routine that will call this routine `n` times, returning either
	/// the final `Ok` or the first `Err`. It is possible that the inner routine
	/// will never be run (if `n` is 0) so the generated routine returns a
	/// `Result<Option<T>, _>`.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::routine::Routine;
	/// # type T = i32;
	/// # fn wrapper(
	/// #     mut routine: impl Routine<(), Output = T, Error = ()>,
	/// # ) -> Result<(), ()> {
	/// # let res = &mut ();
	/// #
	/// // Run the routine 4 times, returning the first `Err` or the last `Ok`
	/// let _: Result<T, _> = routine.run(res);
	/// let _: Result<Option<T>, _> = routine.repeat(4).run(res);
	/// # Ok(())
	/// # }
	/// ```
	fn repeat(self, n: usize) -> Repeat<Self>
	where
		Self: Sized,
	{
		Repeat {
			routine: self,
			count: n,
		}
	}

	/// Create a routine that will repeat this routine while the predicate `f`
	/// returns true. It is possible that the inner routine will never run
	/// (if `f` returns `false` immediately), so the generated routine returns a
	/// `Result<Option<T>, _>`.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::routine::Routine;
	/// # type T = i32;
	/// # fn wrapper(
	/// #     mut routine: impl Routine<(), Output = T, Error = ()>,
	/// # ) -> Result<(), ()> {
	/// # let res = &mut ();
	/// #
	/// # fn should_keep_going() -> bool { false }
	/// #
	/// let _: Result<T, _> = routine.run(res);
	/// let _: Result<Option<T>, _> = routine
	///     .repeat_while(|| should_keep_going())
	///     .run(res);
	/// # Ok(())
	/// # }
	/// ```
	fn repeat_while<F>(self, f: F) -> While<Self, F>
	where
		Self: Sized,
		F: FnMut() -> bool,
	{
		While { routine: self, f }
	}

	/// Create a routine that will run this routine and some `other` routine,
	/// returning the `Ok` value for both routines as a tuple or the first `Err`.
	///
	/// ## Example
	///
	/// ```
	/// # use zproto::routine::Routine;
	/// # type A = i32;
	/// # type B = u32;
	/// # type ErrA = ();
	/// # fn wrapper(
	/// #     mut first_routine: impl Routine<(), Output = A, Error = ()>,
	/// #     mut second_routine: impl Routine<(), Output = B, Error = ()>,
	/// # ) -> Result<(), ()> {
	/// # let res = &mut ();
	/// let _: Result<A, ErrA> = first_routine.run(res);
	/// let _: Result<B, ErrA> = second_routine.run(res);
	///
	/// // Joining two routines returns the result of both on success.
	/// let _: Result<(A, B), ErrA> = first_routine
	///     .join(second_routine)
	///     .run(res);
	/// # Ok(())
	/// # }
	/// ```
	fn join<T>(self, other: T) -> Join<Self, T>
	where
		Self: Sized,
		T: Routine<R>,
	{
		Join { a: self, b: other }
	}
}

impl<R, T> Routine<R> for &mut T
where
	T: Routine<R> + ?Sized,
{
	type Output = <T as Routine<R>>::Output;
	type Error = <T as Routine<R>>::Error;

	fn run(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error> {
		(*self).run(resource)
	}
}

impl<R, T> Routine<R> for Box<T>
where
	T: Routine<R> + ?Sized,
{
	type Output = <T as Routine<R>>::Output;
	type Error = <T as Routine<R>>::Error;

	fn run(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error> {
		self.as_mut().run(resource)
	}
}

/// A routine that maps the `Ok` value of another routine to some new value via a
/// function `f`.
///
/// It is created via the [`map`] method on [`Routine`].
///
/// [`map`]: Routine::map
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct Map<R, F> {
	routine: R,
	f: F,
}

impl<R, S, F, T> Routine<R> for Map<S, F>
where
	F: FnMut(<S as Routine<R>>::Output) -> T,
	S: Routine<R>,
{
	type Output = T;
	type Error = <S as Routine<R>>::Error;

	fn run(&mut self, resource: &'_ mut R) -> Result<Self::Output, Self::Error> {
		match self.routine.run(resource) {
			Ok(value) => Ok((self.f)(value)),
			Err(err) => Err(err),
		}
	}
}

/// A routine that maps the `Err` value of another routine to some new value via
/// a function `f`.
///
/// It is created via the [`map_err`] method on [`Routine`].
///
/// [`map_err`]: Routine::map_err
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct MapErr<R, F> {
	routine: R,
	f: F,
}

impl<R, S, F, T> Routine<R> for MapErr<S, F>
where
	F: FnMut(<S as Routine<R>>::Error) -> T,
	S: Routine<R>,
{
	type Output = <S as Routine<R>>::Output;
	type Error = T;

	fn run(&mut self, resource: &'_ mut R) -> Result<Self::Output, Self::Error> {
		match self.routine.run(resource) {
			Err(err) => Err((self.f)(err)),
			Ok(value) => Ok(value),
		}
	}
}

/// A routine that runs one routine and, if it is successful, runs a second one,
/// returning its result. The `Ok` value of the first routine is discarded.
///
/// It is created via the [`and`] method on [`Routine`].
///
/// [`and`]: Routine::and
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct And<S, T> {
	first: S,
	second: T,
}

impl<E, R, S, T> Routine<R> for And<S, T>
where
	S: Routine<R, Error = E>,
	T: Routine<R, Error = E>,
{
	type Output = <T as Routine<R>>::Output;
	type Error = <T as Routine<R>>::Error;

	fn run(&mut self, resource: &'_ mut R) -> Result<Self::Output, Self::Error> {
		let _ = self.first.run(resource)?;
		self.second.run(resource)
	}
}

/// A routine that runs one routine and, if it is successful, passes the `Ok`
/// value to a function which will return the second routine to run.
///
/// It is created via the [`and_then`] method on [`Routine`].
///
/// [`and_then`]: Routine::and_then
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct AndThen<T, F> {
	routine: T,
	f: F,
}

impl<E, F, R, S, T> Routine<R> for AndThen<S, F>
where
	F: FnMut(<S as Routine<R>>::Output) -> T,
	S: Routine<R, Error = E>,
	T: Routine<R, Error = E>,
{
	type Output = <T as Routine<R>>::Output;
	type Error = <S as Routine<R>>::Error;

	fn run(&mut self, resource: &'_ mut R) -> Result<Self::Output, Self::Error> {
		let output = self.routine.run(resource)?;
		(self.f)(output).run(resource)
	}
}

/// A routine that runs one routine and, if it fails, runs a second one,
/// returning its result. The `Err` value of the first routine is discarded.
///
/// It is created via the [`or`] method on [`Routine`].
///
/// [`or`]: Routine::or
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct Or<S, T> {
	first: S,
	second: T,
}

impl<O, R, S, T> Routine<R> for Or<S, T>
where
	S: Routine<R, Output = O>,
	T: Routine<R, Output = O>,
{
	type Output = <S as Routine<R>>::Output;
	type Error = <T as Routine<R>>::Error;

	fn run(&mut self, resource: &'_ mut R) -> Result<Self::Output, Self::Error> {
		match self.first.run(resource) {
			Err(_) => self.second.run(resource),
			Ok(ok) => Ok(ok),
		}
	}
}

/// A routine that runs one routine and, if it is fails, passes the `Err`
/// value to a function which will return the second routine to run.
///
/// It is created via the [`or_else`] method on [`Routine`].
///
/// [`or_else`]: Routine::or_else
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct OrElse<T, F> {
	routine: T,
	f: F,
}

impl<F, O, R, S, T> Routine<R> for OrElse<S, F>
where
	F: FnMut(<S as Routine<R>>::Error) -> T,
	S: Routine<R, Output = O>,
	T: Routine<R, Output = O>,
{
	type Output = <T as Routine<R>>::Output;
	type Error = <T as Routine<R>>::Error;

	fn run(&mut self, resource: &'_ mut R) -> Result<Self::Output, Self::Error> {
		match self.routine.run(resource) {
			Err(err) => (self.f)(err).run(resource),
			Ok(value) => Ok(value),
		}
	}
}

/// A routine that runs a routine `n` times, returning the first `Err` or the
/// last `Ok`. If `n` is 0, the inner routine will not be run and `Ok(None)`
/// will be returned.
///
/// It is created via the [`repeat`] method on [`Routine`].
///
/// [`repeat`]: Routine::repeat
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct Repeat<T> {
	routine: T,
	count: usize,
}

impl<R, T> Routine<R> for Repeat<T>
where
	T: Routine<R>,
{
	type Output = Option<<T as Routine<R>>::Output>;
	type Error = <T as Routine<R>>::Error;

	fn run(&mut self, resource: &'_ mut R) -> Result<Self::Output, Self::Error> {
		let mut value = None;
		for _ in 0..self.count {
			value = Some(self.routine.run(resource)?);
		}
		Ok(value)
	}
}

/// A routine that runs a routine while a predicate returns `true` or until the
/// routine returns `Err`. The first `Err` or final `Ok` is returned.
///
/// It is created via the [`repeat_while`] method on [`Routine`].
///
/// [`repeat_while`]: Routine::repeat_while
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct While<T, F> {
	routine: T,
	f: F,
}

impl<F, R, T> Routine<R> for While<T, F>
where
	T: Routine<R>,
	F: FnMut() -> bool,
{
	type Output = Option<<T as Routine<R>>::Output>;
	type Error = <T as Routine<R>>::Error;

	fn run(&mut self, resource: &'_ mut R) -> Result<Self::Output, Self::Error> {
		let mut value = None;
		while (self.f)() {
			value = Some(self.routine.run(resource)?);
		}
		Ok(value)
	}
}

/// A routine that will run this routine and some `other` routine, returning the
/// `Ok` value for both routines or the first `Err`.
///
/// It is created via the [`join`] method on [`Routine`].
///
/// [`join`]: Routine::join
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct Join<A, B> {
	a: A,
	b: B,
}

impl<A, B, E, R> Routine<R> for Join<A, B>
where
	A: Routine<R, Error = E>,
	B: Routine<R, Error = E>,
{
	type Output = (<A as Routine<R>>::Output, <B as Routine<R>>::Output);
	type Error = <A as Routine<R>>::Error;

	fn run(&mut self, resource: &'_ mut R) -> Result<Self::Output, Self::Error> {
		Ok((self.a.run(resource)?, self.b.run(resource)?))
	}
}

/// Converts a type into a [`Routine`].
pub trait IntoRoutine<R> {
	/// The output type returned by the generated routine.
	type Output;
	/// The error type returned by the generated routine.
	type Error;
	/// The type of the generated routine.
	type IntoRoutine: Routine<R, Output = Self::Output, Error = Self::Error>;

	/// Consume the item and return a [`Routine`].
	fn into_routine(self) -> Self::IntoRoutine;
}

impl<R, T> IntoRoutine<R> for T
where
	T: Routine<R>,
{
	type Output = <T as Routine<R>>::Output;
	type Error = <T as Routine<R>>::Error;
	type IntoRoutine = T;

	fn into_routine(self) -> Self::IntoRoutine {
		self
	}
}

#[cfg(test)]
mod test {
	#[allow(clippy::wildcard_imports)]
	use super::*;

	#[derive(Debug, Copy, Clone)]
	struct OkRoutine<T>(T);

	impl<T> Routine<()> for OkRoutine<T>
	where
		T: Copy,
	{
		type Output = T;
		type Error = i32;

		fn run(&mut self, (): &mut ()) -> Result<Self::Output, Self::Error> {
			Ok(self.0)
		}
	}

	#[derive(Debug, Copy, Clone, PartialEq, Eq)]
	struct ErrRoutine(i32);

	impl Routine<()> for ErrRoutine {
		type Output = i32;
		type Error = i32;

		fn run(&mut self, (): &mut ()) -> Result<Self::Output, Self::Error> {
			Err(self.0)
		}
	}

	/// Returns `Ok(value)` for `count` times, and then `Err(value)` from then on.
	#[derive(Debug, Copy, Clone, PartialEq, Eq)]
	struct IterRoutine {
		value: i32,
		count: usize,
		called_count: usize,
	}

	impl Routine<()> for IterRoutine {
		type Output = i32;
		type Error = i32;

		fn run(&mut self, (): &mut ()) -> Result<Self::Output, Self::Error> {
			self.called_count += 1;
			let is_ok = self.count > 0;
			self.count = self.count.saturating_sub(1);
			if is_ok {
				Ok(self.value)
			} else {
				Err(self.value)
			}
		}
	}

	#[test]
	fn map() {
		assert_eq!(Ok(2), OkRoutine(1).map(|x| 2 * x).run(&mut ()));
		assert_eq!(Err(1), ErrRoutine(1).map(|x| 2 * x).run(&mut ()));
	}

	#[test]
	fn map_err() {
		assert_eq!(Ok(1), OkRoutine(1).map_err(|x| 2.0 * f64::from(x)).run(&mut ()));
		assert_eq!(
			Err(2.0),
			ErrRoutine(1).map_err(|x| 2.0 * f64::from(x)).run(&mut ())
		);
	}

	#[test]
	fn and() {
		assert_eq!(Ok("hi"), OkRoutine(1).and(OkRoutine("hi")).run(&mut ()));
		assert_eq!(Err(1), ErrRoutine(1).and(OkRoutine("hi")).run(&mut ()));
	}

	#[test]
	fn and_then() {
		assert_eq!(
			Ok(2),
			OkRoutine(1)
				.and_then(|value| OkRoutine(value * 2))
				.run(&mut ())
		);
		assert_eq!(
			Err(1),
			ErrRoutine(1)
				.and_then(|value| OkRoutine(value * 2))
				.run(&mut ())
		);
	}

	#[test]
	fn or() {
		assert_eq!(Ok(1), OkRoutine(1).or(OkRoutine(2)).run(&mut ()));
		assert_eq!(Ok(1), OkRoutine(1).or(ErrRoutine(2)).run(&mut ()));
		assert_eq!(Ok(2), ErrRoutine(1).or(OkRoutine(2)).run(&mut ()));
		assert_eq!(Err(2), ErrRoutine(1).or(ErrRoutine(2)).run(&mut ()));
	}

	#[test]
	fn or_else() {
		assert_eq!(
			Ok(1),
			OkRoutine(1).or_else(|_err| OkRoutine(2)).run(&mut ())
		);
		assert_eq!(
			Ok(1),
			OkRoutine(1).or_else(|_err| ErrRoutine(2)).run(&mut ())
		);
		assert_eq!(
			Ok(2),
			ErrRoutine(1).or_else(|_err| OkRoutine(2)).run(&mut ())
		);
		assert_eq!(
			Err(2),
			ErrRoutine(1).or_else(|_err| ErrRoutine(2)).run(&mut ())
		);
	}

	#[test]
	fn repeat() {
		let mut routine = IterRoutine {
			value: 3,
			count: 5,
			called_count: 0,
		};
		let value = (&mut routine).repeat(5).run(&mut ()).unwrap();
		assert_eq!(value, Some(3));
		assert_eq!(routine.called_count, 5);

		let value = (&mut routine).repeat(5).run(&mut ()).unwrap_err();
		assert_eq!(value, 3);
		assert_eq!(routine.called_count, 6);

		let value = OkRoutine(1).repeat(0).run(&mut ()).unwrap();
		assert!(value.is_none());
	}

	#[test]
	fn repeat_while() {
		let mut count = 0;
		let mut routine = IterRoutine {
			value: 3,
			count: 5,
			called_count: 0,
		};
		let value = (&mut routine)
			.repeat_while(|| {
				let temp = count;
				count += 1;
				temp < 3
			})
			.run(&mut ())
			.unwrap();
		assert_eq!(value, Some(3));
		assert_eq!(routine.called_count, 3);

		let value = (&mut routine)
			.repeat_while(|| true)
			.run(&mut ())
			.unwrap_err();
		assert_eq!(value, 3);
		assert_eq!(routine.called_count, 6); // 5 Ok + 1 Err

		let value = (&mut routine).repeat_while(|| false).run(&mut ()).unwrap();
		assert_eq!(value, None);
		assert_eq!(routine.called_count, 6);
	}

	#[test]
	fn join() {
		let values = OkRoutine(1).join(OkRoutine(2)).run(&mut ()).unwrap();
		assert_eq!(values, (1, 2));
	}
}
