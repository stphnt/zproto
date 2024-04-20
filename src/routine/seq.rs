use super::Routine;

/// A routine that wraps a sequence of routines, running each in order and
/// returning all of their results together.
///
/// The wrapped sequence of routines can be a tuple, array, slice, or `Vec`.
///
/// ## Example
///
/// ```
/// # use zproto::routine::{Routine, Seq};
/// # fn wrapper(
/// #     mut routine1: impl Routine<(), Output = (), Error = ()> + Copy,
/// #     mut routine2: impl Routine<(), Output = (), Error = ()> + Copy,
/// #     mut routine3: impl Routine<(), Output = (), Error = ()> + Copy,
/// # ) -> Result<(), ()> {
/// # let resource = &mut ();
/// let (out1, out2, out3) = Seq((routine1, routine2, routine3)).run(resource)?;
///
/// let outputs: Vec<_> = Seq([routine1, routine1, routine1]).run(resource)?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct Seq<T>(pub T);

impl<R, T> Routine<R> for Seq<T>
where
	T: SeqRoutine<R>,
{
	type Output = <T as SeqRoutine<R>>::Output;
	type Error = <T as SeqRoutine<R>>::Error;

	fn run(&mut self, resource: &'_ mut R) -> Result<Self::Output, Self::Error> {
		self.0.run_seq(resource)
	}
}

/// Represents a type that can be used within a [`Seq`].
pub trait SeqRoutine<R> {
	/// The output of the routine.
	type Output;
	/// The error returned by the routine.
	type Error;

	/// Mirrors the [`Seq::run`] method, but is specifically for running a sequence of routines.
	fn run_seq(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error>;
}

impl<T, R> SeqRoutine<R> for &mut T
where
	T: SeqRoutine<R>,
{
	type Output = <T as SeqRoutine<R>>::Output;
	type Error = <T as SeqRoutine<R>>::Error;
	fn run_seq(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error> {
		(*self).run_seq(resource)
	}
}

impl<R, E, A, B> SeqRoutine<R> for (A, B)
where
	A: Routine<R, Error = E>,
	B: Routine<R>,
	E: From<<B as Routine<R>>::Error>,
{
	type Output = (<A as Routine<R>>::Output, <B as Routine<R>>::Output);
	type Error = E;
	fn run_seq(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error> {
		Ok((self.0.run(resource)?, self.1.run(resource)?))
	}
}

impl<R, E, A, B, C> SeqRoutine<R> for (A, B, C)
where
	A: Routine<R, Error = E>,
	B: Routine<R>,
	C: Routine<R>,
	E: From<<B as Routine<R>>::Error>,
	E: From<<C as Routine<R>>::Error>,
{
	type Output = (
		<A as Routine<R>>::Output,
		<B as Routine<R>>::Output,
		<C as Routine<R>>::Output,
	);
	type Error = E;
	fn run_seq(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error> {
		Ok((
			self.0.run(resource)?,
			self.1.run(resource)?,
			self.2.run(resource)?,
		))
	}
}

impl<R, E, A, B, C, D> SeqRoutine<R> for (A, B, C, D)
where
	A: Routine<R, Error = E>,
	B: Routine<R>,
	C: Routine<R>,
	D: Routine<R>,
	E: From<<B as Routine<R>>::Error>,
	E: From<<C as Routine<R>>::Error>,
	E: From<<D as Routine<R>>::Error>,
{
	type Output = (
		<A as Routine<R>>::Output,
		<B as Routine<R>>::Output,
		<C as Routine<R>>::Output,
		<D as Routine<R>>::Output,
	);
	type Error = E;
	fn run_seq(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error> {
		Ok((
			self.0.run(resource)?,
			self.1.run(resource)?,
			self.2.run(resource)?,
			self.3.run(resource)?,
		))
	}
}

impl<R, E, A, B, C, D, F> SeqRoutine<R> for (A, B, C, D, F)
where
	A: Routine<R, Error = E>,
	B: Routine<R>,
	C: Routine<R>,
	D: Routine<R>,
	F: Routine<R>,
	E: From<<B as Routine<R>>::Error>,
	E: From<<C as Routine<R>>::Error>,
	E: From<<D as Routine<R>>::Error>,
	E: From<<F as Routine<R>>::Error>,
{
	type Output = (
		<A as Routine<R>>::Output,
		<B as Routine<R>>::Output,
		<C as Routine<R>>::Output,
		<D as Routine<R>>::Output,
		<F as Routine<R>>::Output,
	);
	type Error = E;
	fn run_seq(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error> {
		Ok((
			self.0.run(resource)?,
			self.1.run(resource)?,
			self.2.run(resource)?,
			self.3.run(resource)?,
			self.4.run(resource)?,
		))
	}
}

impl<R, E, A, B, C, D, F, G> SeqRoutine<R> for (A, B, C, D, F, G)
where
	A: Routine<R, Error = E>,
	B: Routine<R>,
	C: Routine<R>,
	D: Routine<R>,
	F: Routine<R>,
	G: Routine<R>,
	E: From<<B as Routine<R>>::Error>,
	E: From<<C as Routine<R>>::Error>,
	E: From<<D as Routine<R>>::Error>,
	E: From<<F as Routine<R>>::Error>,
	E: From<<G as Routine<R>>::Error>,
{
	type Output = (
		<A as Routine<R>>::Output,
		<B as Routine<R>>::Output,
		<C as Routine<R>>::Output,
		<D as Routine<R>>::Output,
		<F as Routine<R>>::Output,
		<G as Routine<R>>::Output,
	);
	type Error = E;
	fn run_seq(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error> {
		Ok((
			self.0.run(resource)?,
			self.1.run(resource)?,
			self.2.run(resource)?,
			self.3.run(resource)?,
			self.4.run(resource)?,
			self.5.run(resource)?,
		))
	}
}

impl<R, T, const N: usize> SeqRoutine<R> for [T; N]
where
	T: Routine<R>,
{
	type Output = Vec<<T as Routine<R>>::Output>;
	type Error = <T as Routine<R>>::Error;

	fn run_seq(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error> {
		let mut output = Vec::new();
		for item in self {
			output.push(item.run(resource)?);
		}
		Ok(output)
	}
}

impl<R, T> SeqRoutine<R> for [T]
where
	T: Routine<R>,
{
	type Output = Vec<<T as Routine<R>>::Output>;
	type Error = <T as Routine<R>>::Error;

	fn run_seq(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error> {
		let mut output = Vec::new();
		for item in self {
			output.push(item.run(resource)?);
		}
		Ok(output)
	}
}

impl<R, T> SeqRoutine<R> for Vec<T>
where
	T: Routine<R>,
{
	type Output = Vec<<T as Routine<R>>::Output>;
	type Error = <T as Routine<R>>::Error;

	fn run_seq(&mut self, resource: &mut R) -> Result<Self::Output, Self::Error> {
		self.as_mut_slice().run_seq(resource)
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[derive(Debug, Copy, Clone)]
	struct ConstRoutine<T>(T);

	impl<'a, T> Routine<()> for ConstRoutine<T>
	where
		T: Copy + 'a,
	{
		type Output = T;
		type Error = String;

		fn run(&mut self, _: &mut ()) -> Result<Self::Output, Self::Error> {
			Ok(self.0)
		}
	}

	#[derive(Debug, Copy, Clone, PartialEq, Eq)]
	struct FailRoutine(u32);

	impl Routine<()> for FailRoutine {
		type Output = u32;
		type Error = String;

		fn run(&mut self, _: &mut ()) -> Result<Self::Output, Self::Error> {
			Err(format!("{}", self.0))
		}
	}

	#[derive(Debug, Copy, Clone, PartialEq, Eq)]
	struct ConditionalPassRoutine(bool, u32);

	impl Routine<()> for ConditionalPassRoutine {
		type Output = u32;
		type Error = String;

		fn run(&mut self, port: &mut ()) -> Result<Self::Output, Self::Error> {
			if self.0 {
				Ok(self.1)
			} else {
				FailRoutine(self.1).run(port)
			}
		}
	}

	#[test]
	fn array() {
		let res = &mut ();

		let mut routine = Seq([
			ConstRoutine(1u32),
			ConstRoutine(2),
			ConstRoutine(3),
			ConstRoutine(4),
		]);
		assert_eq!([1, 2, 3, 4].as_slice(), routine.run(res).unwrap());

		let mut routine = Seq([
			ConditionalPassRoutine(true, 1u32),
			ConditionalPassRoutine(false, 2),
			ConditionalPassRoutine(true, 3),
			ConditionalPassRoutine(true, 4),
		]);
		assert_eq!("2", routine.run(res).unwrap_err());
	}

	#[test]
	fn vec() {
		let res = &mut ();

		let mut routine = Seq(vec![
			ConstRoutine(1u32),
			ConstRoutine(2),
			ConstRoutine(3),
			ConstRoutine(4),
		]);
		assert_eq!([1, 2, 3, 4].as_slice(), routine.run(res).unwrap());
	}

	#[test]
	fn tuples() {
		let res = &mut ();

		let mut routines = Seq((ConstRoutine(1u32), ConstRoutine(5)));
		assert_eq!((1u32, 5i32), routines.run(res).unwrap());
		let mut routines = Seq((ConstRoutine(1), FailRoutine(1)));
		let error = routines.run(res).unwrap_err();
		assert_eq!(error, "1");

		let mut routines = Seq((ConstRoutine(1u32), ConstRoutine(5), ConstRoutine("hi")));
		assert_eq!((1u32, 5i32, "hi"), routines.run(res).unwrap());
		let mut routines = Seq((ConstRoutine(1), FailRoutine(1), FailRoutine(2)));
		let error = routines.run(res).unwrap_err();
		assert_eq!(error, "1");

		let mut routines = Seq((
			ConstRoutine(1u32),
			ConstRoutine(5),
			ConstRoutine("hi"),
			ConstRoutine(b"hi"),
		));
		assert_eq!((1u32, 5i32, "hi", b"hi"), routines.run(res).unwrap());
		let mut routines = Seq((
			ConstRoutine(1),
			FailRoutine(1),
			ConstRoutine(2),
			FailRoutine(3),
		));
		let error = routines.run(res).unwrap_err();
		assert_eq!(error, "1");

		let mut routines = Seq((
			ConstRoutine(1u32),
			ConstRoutine(5),
			ConstRoutine("hi"),
			ConstRoutine(b"hi"),
			ConstRoutine((1, 2)),
		));
		assert_eq!(
			(1u32, 5i32, "hi", b"hi", (1, 2)),
			routines.run(res).unwrap()
		);
		let mut routines = Seq((
			ConstRoutine(1),
			FailRoutine(1),
			ConstRoutine(2),
			FailRoutine(3),
			FailRoutine(4),
		));
		let error = routines.run(res).unwrap_err();
		assert_eq!(error, "1");
	}
}
