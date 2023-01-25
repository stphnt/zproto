//! Types for checking the contents of an ASCII response via a `Port`'s
//! `*_with_check` methods.
//!
//! The [`strict`] and [`minimal`] functions define two common sets of response
//! checks. [`strict`] ensures all reply flags are OK and there are never any
//! warnings. [`minimal`] is similar but more relaxed. While it still ensures
//! all reply flags are OK, it only ensures that there are no fault (`F*`) level
//! warning flags.
//!
//! It is also easy to define your own custom response checks. The [`Check`]
//! trait defines the interface all "checkers" must implement. It is
//! implemented for all closures that take a [`Response`](crate::ascii::Response)
//! and return a `Result<Response, AsciiCheckError>`. However, you are
//! encouraged to use the [functions](#functions) provided in this module to
//! generate checking functions. They cover many of the common cases, can be
//! combined together, generate useful error messages, and make code much
//! clearer.
//!
//! For instance, to generate a function that checks if a reply has the flag
//! `OK` and no warning flags (the equivalent of the `strict` function) you
//! could use:
//!
//! ```rust
//! # use zproto::ascii::{
//! #     Flag,
//! #     Reply,
//! #     check::{all, Check, flag_is, warning_is_none},
//! # };
//! # fn wrapper() -> impl Check<Reply> {
//! all((
//!    flag_is(Flag::Ok),
//!    warning_is_none(),
//! ))
//! # }
//! ```
//!
//! What each part does should hopefully be self explanatory from the function
//! names:
//!   * [`all`] checks that all of the checks passed to it pass (notice that they
//!     are enclosed in a [`tuple`])
//!   * [`flag_is`] checks that the reply flag on the [`Reply`] is the specified
//!     value
//!   * [`warning_is_none`] checks that the warning is `--`.
//!
//! There are also the `status_*`, `warning_*`, `flag_*`, or [`parsed_data_is`]
//! functions to generate functions to check a response's status, warnings, reply
//! flags, and data, respectively. If the validation logic for a response is more
//! complex, the [`predicate`] function allows you to validate responses using
//! a closure that simply returns a `bool`.

use crate::ascii::response::{
    AnyResponse, Flag, Reply, Response, ResponseWithStatus, ResponseWithWarning, SpecificResponse,
    Status, Warning,
};
use crate::error::*;

/// A trait for checking the contents of a response.
///
/// See the [`check`](self) module level documentation for more information about this trait.
pub trait Check<R: Response>: private::Sealed<R> {
    /// Check the contents of a response.
    ///
    /// If the contents of the response are considered valid, the response should be returned.
    /// Otherwise return the response as a member in the error.
    fn check(&self, response: R) -> Result<R, AsciiCheckError<R>>;
}

impl<R: Response, F: Fn(R) -> Result<R, AsciiCheckError<R>>> Check<R> for F {
    fn check(&self, response: R) -> Result<R, AsciiCheckError<R>> {
        (self)(response)
    }
}

/// A helper type for converting any type that implements `Check<R: Response>`
/// to a new type that implements `Check<AnyResponse>`.
///
/// Users shouldn't need to use this directly in most cases.
#[derive(Debug)]
pub struct AnyResponseCheck<K, R>(K, std::marker::PhantomData<R>);

impl<K, R> Check<AnyResponse> for AnyResponseCheck<K, R>
where
    K: Check<R>,
    R: SpecificResponse,
{
    fn check(
        &self,
        any_response: AnyResponse,
    ) -> Result<AnyResponse, AsciiCheckError<AnyResponse>> {
        match R::try_from(any_response) {
            Ok(response) => self.0.check(response).map(Into::into).map_err(Into::into),
            Err(any_response) => Ok(any_response),
        }
    }
}

impl<K> Check<AnyResponse> for AnyResponseCheck<K, AnyResponse>
where
    K: Check<AnyResponse>,
{
    fn check(
        &self,
        any_response: AnyResponse,
    ) -> Result<AnyResponse, AsciiCheckError<AnyResponse>> {
        self.0.check(any_response)
    }
}

impl<K, R> From<K> for AnyResponseCheck<K, R>
where
    R: Response,
    K: Check<R>,
{
    fn from(other: K) -> AnyResponseCheck<K, R> {
        AnyResponseCheck(other, std::marker::PhantomData)
    }
}

mod private {
    use super::*;
    pub trait Sealed<R: Response> {}

    impl<R: Response, F: Fn(R) -> Result<R, AsciiCheckError<R>>> Sealed<R> for F {}
    impl<K: Check<R>, R: Response> Sealed<AnyResponse> for AnyResponseCheck<K, R> {}
}

/// Return a check that verifies the response's [`Warning`] matches the specified warning.
///
/// ## Example
/// ```rust
/// # use zproto::ascii::{check::{Check, warning_is}, Warning, Reply};
/// # fn wrapper() -> impl Check<Reply> {
/// warning_is("WR")
/// # }
/// ```
pub fn warning_is<R, W>(warning: W) -> impl Check<R>
where
    R: ResponseWithWarning,
    Warning: std::convert::TryFrom<W>,
{
    use std::convert::TryFrom as _;
    let warning = Warning::try_from(warning).unwrap_or_else(|_| panic!("Invalid warning"));
    move |response: R| {
        if response.warning() == warning {
            Ok(response)
        } else {
            Err(
                AsciiCheckWarningError::new(format!("expected {} warning flag", warning), response)
                    .into(),
            )
        }
    }
}

/// Return a check that verifies the response's [`Warning`] is in the specified list.
///
/// The list can be a slice, array, or tuple of types that are comparible to a [`Warning`].
///
/// ## Example
/// ```rust
/// # use zproto::ascii::{check::{Check, warning_in}, Warning, Reply};
/// # fn wrapper() -> impl Check<Reply> {
/// warning_in(["WR", "WH"])
/// # }
/// ```
pub fn warning_in<R: ResponseWithWarning, L: WarningList>(warnings: L) -> impl Check<R> {
    move |response: R| {
        if warnings.contains(response.warning()) {
            Ok(response)
        } else {
            use std::fmt::Write as _;
            let mut msg = String::new();
            msg.write_str("expected one of ").unwrap();
            warnings.write_fmt_to_str(&mut msg);
            msg.write_str(" warning flag(s)").unwrap();
            Err(AsciiCheckWarningError::new(msg, response).into())
        }
    }
}

/// Return a check that verifies the response's [`Warning`] is below the fault
/// level, i.e., `--`, `N*`, or `W*`.
pub fn warning_below_fault<R: ResponseWithWarning>() -> impl Check<R> {
    |response: R| {
        if !response.warning().is_fault() {
            Ok(response)
        } else {
            Err(
                AsciiCheckWarningError::new("expected warning below fault (F) level", response)
                    .into(),
            )
        }
    }
}

/// Return a check that verifies the response's [`Warning`] is below the
/// warning level, i.e., `--` or `N*`.
pub fn warning_below_warning<R: ResponseWithWarning>() -> impl Check<R> {
    |response: R| {
        let warning = response.warning();
        if !warning.is_fault() && !warning.is_warning() {
            Ok(response)
        } else {
            Err(
                AsciiCheckWarningError::new("expected warning below warning (W) level", response)
                    .into(),
            )
        }
    }
}

/// Return a check that verifies the response's [`Warning`] is `--`.
pub fn warning_is_none<R: ResponseWithWarning>() -> impl Check<R> {
    |response: R| {
        if response.warning().is_none() {
            Ok(response)
        } else {
            Err(AsciiCheckWarningError::new("expected no warning (--)", response).into())
        }
    }
}

/// Return a check that verifies the response's [`Status`] matches the specified status.
///
/// ## Example
/// ```rust
/// # use zproto::ascii::{check::{Check, status_is}, Status, Reply};
/// # fn wrapper() -> impl Check<Reply> {
/// status_is(Status::Idle)
/// # }
/// ```
pub fn status_is<R: ResponseWithStatus>(status: Status) -> impl Check<R> {
    move |response: R| {
        if response.status() == status {
            Ok(response)
        } else {
            Err(AsciiCheckStatusError::new(status, response).into())
        }
    }
}

/// Return a check that verifies the response's [`Status`] is `IDLE`.
pub fn status_idle<R: ResponseWithStatus>() -> impl Check<R> {
    status_is(Status::Idle)
}

/// Return a check that verifies the response's [`Status`] is `BUSY`.
pub fn status_busy<R: ResponseWithStatus>() -> impl Check<R> {
    status_is(Status::Busy)
}

/// Return a check that verifies the response's [`Flag`] matches the specified flag.
///
/// ## Example
/// ```rust
/// # use zproto::ascii::{check::{Check, flag_is}, Flag, Reply};
/// # fn wrapper() -> impl Check<Reply> {
/// flag_is(Flag::Ok)
/// # }
/// ```
pub fn flag_is(flag: Flag) -> impl Check<Reply> {
    move |response: Reply| {
        if response.flag() == flag {
            Ok(response)
        } else {
            Err(AsciiCheckFlagError::new(flag, response).into())
        }
    }
}

/// Return a check that verifies the response's [`Flag`] is `OK`.
pub fn flag_ok() -> impl Check<Reply> {
    flag_is(Flag::Ok)
}

/// Return a check that verifies the response's [`Flag`] is `RJ`.
pub fn flag_rj() -> impl Check<Reply> {
    flag_is(Flag::Ok)
}

/// Return a check that verifies the response's [`Flag`] is `OK` and that the
/// provided `check` also passes.
///
/// ## Example
/// ```rust
/// # use zproto::ascii::{check::{Check, flag_ok_and, warning_is}, Flag, Reply};
/// # fn wrapper() -> impl Check<Reply> {
/// flag_ok_and(warning_is("WR"))
/// # }
/// ```
pub fn flag_ok_and(check: impl Check<Reply>) -> impl Check<Reply> {
    all((flag_ok(), check))
}

/// Return a check that verifies the parsed response data matches the specified value.
///
/// If the response data cannot be parsed as the specified value or value does
/// not match an error is returned.
///
/// ## Example
/// ```rust
/// # use zproto::ascii::{check::{Check, parsed_data_is}, Reply};
/// # fn wrapper() -> impl Check<Reply> {
/// parsed_data_is(256)
/// # }
/// ```
pub fn parsed_data_is<
    R: Response,
    T: std::str::FromStr + std::cmp::PartialEq<T> + std::fmt::Debug,
>(
    value: T,
) -> impl Check<R> {
    move |response: R| {
        if let Ok(parsed) = response.data().parse::<T>() {
            if parsed == value {
                Ok(response)
            } else {
                Err(AsciiCheckDataError::new(format!("expected data {:?}", value), response).into())
            }
        } else {
            Err(AsciiCheckDataError::new("could not parse data as expected type", response).into())
        }
    }
}

/// Return a check that will validate a response against all the specified checks.
///
/// Once one check fails, no further checks are run. Note that the checks must
/// be passed as members of a [`tuple`].
///
/// ## Example
/// ```rust
/// # use zproto::ascii::{*, check::*};
/// # fn wrapper() -> impl Check<Reply> {
/// all((
///     flag_is(Flag::Ok),
///     warning_in(("WR", "WH"))
/// ))
/// # }
/// ```
pub fn all<R: Response, C: CheckAll<R>>(checks: C) -> impl Check<R> {
    move |response: R| checks.check(response)
}

/// Return a strict check for the specified message type.
///
/// For [`Reply`](crate::ascii::Reply) this is equivalent to
/// ```rust
/// # use zproto::ascii::{check::*, *};
/// # fn wrapper() -> impl Check<Reply> {
/// flag_ok_and(warning_is_none())
/// # }
/// ```
///
/// For [`Alert`](crate::ascii::Alert) this is equivalent to
/// ```rust
/// # use zproto::ascii::{check::*, *};
/// # fn wrapper() -> impl Check<Alert> {
/// warning_is_none()
/// # }
/// ```
///
/// For [`Info`](crate::ascii::Info) no validation is done.
///
/// For [`AnyResponse`](crate::ascii::AnyResponse), one of the above checks is
/// chosen at runtime based on the kind of response.
pub fn strict<R: Response>() -> impl Check<R> {
    R::strict()
}

/// Return a check that performs minimal checks of the specified message type
/// while still checking for major problems with devices. In most cases, more
/// rigorous checks are probably warranted.
///
/// For [`Reply`](crate::ascii::Reply) this is equivalent to
/// ```rust
/// # use zproto::ascii::{check::*, *};
/// # fn wrapper() -> impl Check<Reply> {
/// flag_ok_and(warning_below_fault())
/// # }
/// ```
///
/// For [`Alert`](crate::ascii::Alert) this is equivalent to
/// ```rust
/// # use zproto::ascii::{check::*, *};
/// # fn wrapper() -> impl Check<Alert> {
/// warning_below_fault()
/// # }
/// ```
///
/// For [`Info`](crate::ascii::Info) no validation is done.
///
/// For [`AnyResponse`](crate::ascii::AnyResponse), one of the above checks is
/// chosen at runtime based on the kind of response.
pub fn minimal<R: Response>() -> impl Check<R> {
    R::minimal()
}

/// Return a check that does not validate the response.
pub fn unchecked<R: Response>() -> impl Check<R> {
    |response: R| Ok(response)
}

/// Return a check that validates a response with the given `predicate`.
///
/// The response is considered valid if and only if `predicate` returns `true`.
///
/// ## Example
/// ```rust
/// # use zproto::ascii::{check::{Check, predicate}, Reply};
/// # fn wrapper() -> impl Check<Reply> {
/// let expected_value = "256";
/// let check = predicate(move |reply: &Reply| {
///     reply.data() == expected_value
/// });
/// # check
/// # }
/// ```
pub fn predicate<R: Response, P: Fn(&R) -> bool>(predicate: P) -> impl Check<R> {
    move |response: R| {
        if predicate(&response) {
            Ok(response)
        } else {
            Err(AsciiCheckCustomError::unknown(response).into())
        }
    }
}

/// A helper trait for the [`all`] function.
///
/// It is implemented on tuples up to 5 elements in length.
pub trait CheckAll<R: Response> {
    /// Check the contents of a response against all member checks
    ///
    /// If the contents of the response are considered valid by all member checks, the response should be returned.
    /// Otherwise return the response as a member in the error.
    fn check(&self, response: R) -> Result<R, AsciiCheckError<R>>;
}

impl<R: Response, A: Check<R>> CheckAll<R> for (A,) {
    fn check(&self, response: R) -> Result<R, AsciiCheckError<R>> {
        self.0.check(response)
    }
}

impl<R: Response, A: Check<R>, B: Check<R>> CheckAll<R> for (A, B) {
    fn check(&self, response: R) -> Result<R, AsciiCheckError<R>> {
        let response = self.0.check(response)?;
        let response = self.1.check(response)?;
        Ok(response)
    }
}

impl<R: Response, A: Check<R>, B: Check<R>, C: Check<R>> CheckAll<R> for (A, B, C) {
    fn check(&self, response: R) -> Result<R, AsciiCheckError<R>> {
        let response = self.0.check(response)?;
        let response = self.1.check(response)?;
        let response = self.2.check(response)?;
        Ok(response)
    }
}

impl<R: Response, A: Check<R>, B: Check<R>, C: Check<R>, D: Check<R>> CheckAll<R> for (A, B, C, D) {
    fn check(&self, response: R) -> Result<R, AsciiCheckError<R>> {
        let response = self.0.check(response)?;
        let response = self.1.check(response)?;
        let response = self.2.check(response)?;
        let response = self.3.check(response)?;
        Ok(response)
    }
}

impl<R: Response, A: Check<R>, B: Check<R>, C: Check<R>, D: Check<R>, E: Check<R>> CheckAll<R>
    for (A, B, C, D, E)
{
    fn check(&self, response: R) -> Result<R, AsciiCheckError<R>> {
        let response = self.0.check(response)?;
        let response = self.1.check(response)?;
        let response = self.2.check(response)?;
        let response = self.3.check(response)?;
        let response = self.4.check(response)?;
        Ok(response)
    }
}

/// A helper trait for the [`warning_in`] function.
///
/// It is implemented for arrays and slices of any type that can be compared to a [`Warning`].
/// It is also implemented for tuples up to 5 elements in length and containing types that can be compared to a [`Warning`].
pub trait WarningList {
    /// Returns whether the specified warning is present in the list
    fn contains(&self, warning: Warning) -> bool;
    /// Write the contents of the list to the string `s`.
    fn write_fmt_to_str(&self, s: &mut String);
}

impl<A> WarningList for (A,)
where
    A: AsRef<[u8]>,
{
    fn contains(&self, warning: Warning) -> bool {
        warning == self.0
    }
    fn write_fmt_to_str(&self, s: &mut String) {
        use std::fmt::Write as _;
        s.write_fmt(format_args!("{}", String::from_utf8_lossy(self.0.as_ref())))
            .unwrap();
    }
}

impl<A, B> WarningList for (A, B)
where
    A: AsRef<[u8]>,
    B: AsRef<[u8]>,
{
    fn contains(&self, warning: Warning) -> bool {
        warning == self.0 || warning == self.1
    }
    fn write_fmt_to_str(&self, s: &mut String) {
        use std::fmt::Write as _;
        s.write_fmt(format_args!(
            "{} or {}",
            String::from_utf8_lossy(self.0.as_ref()),
            String::from_utf8_lossy(self.1.as_ref())
        ))
        .unwrap();
    }
}

impl<A, B, C> WarningList for (A, B, C)
where
    A: AsRef<[u8]>,
    B: AsRef<[u8]>,
    C: AsRef<[u8]>,
{
    fn contains(&self, warning: Warning) -> bool {
        warning == self.0 || warning == self.1 || warning == self.2
    }
    fn write_fmt_to_str(&self, s: &mut String) {
        use std::fmt::Write as _;
        s.write_fmt(format_args!(
            "{}, {}, or {}",
            String::from_utf8_lossy(self.0.as_ref()),
            String::from_utf8_lossy(self.1.as_ref()),
            String::from_utf8_lossy(self.2.as_ref())
        ))
        .unwrap();
    }
}

impl<A, B, C, D> WarningList for (A, B, C, D)
where
    A: AsRef<[u8]>,
    B: AsRef<[u8]>,
    C: AsRef<[u8]>,
    D: AsRef<[u8]>,
{
    fn contains(&self, warning: Warning) -> bool {
        warning == self.0 || warning == self.1 || warning == self.2 || warning == self.3
    }
    fn write_fmt_to_str(&self, s: &mut String) {
        use std::fmt::Write as _;
        s.write_fmt(format_args!(
            "{}, {}, {}, or {}",
            String::from_utf8_lossy(self.0.as_ref()),
            String::from_utf8_lossy(self.1.as_ref()),
            String::from_utf8_lossy(self.2.as_ref()),
            String::from_utf8_lossy(self.3.as_ref())
        ))
        .unwrap();
    }
}

impl<A, B, C, D, E> WarningList for (A, B, C, D, E)
where
    A: AsRef<[u8]>,
    B: AsRef<[u8]>,
    C: AsRef<[u8]>,
    D: AsRef<[u8]>,
    E: AsRef<[u8]>,
{
    fn contains(&self, warning: Warning) -> bool {
        warning == self.0
            || warning == self.1
            || warning == self.2
            || warning == self.3
            || warning == self.4
    }
    fn write_fmt_to_str(&self, s: &mut String) {
        use std::fmt::Write as _;
        s.write_fmt(format_args!(
            "{}, {}, {}, {}, or {}",
            String::from_utf8_lossy(self.0.as_ref()),
            String::from_utf8_lossy(self.1.as_ref()),
            String::from_utf8_lossy(self.2.as_ref()),
            String::from_utf8_lossy(self.3.as_ref()),
            String::from_utf8_lossy(self.4.as_ref())
        ))
        .unwrap();
    }
}

impl<A> WarningList for [A]
where
    A: AsRef<[u8]>,
{
    fn contains(&self, warning: Warning) -> bool {
        for item in self {
            if warning == *item {
                return true;
            }
        }
        false
    }
    fn write_fmt_to_str(&self, s: &mut String) {
        use std::fmt::Write as _;
        for (i, item) in self.iter().enumerate() {
            let prefix = match i {
                0 => "",
                i if i == self.len() - 1 => ", or ",
                _ => ", ",
            };
            s.write_fmt(format_args!(
                "{}{}",
                prefix,
                String::from_utf8_lossy(item.as_ref())
            ))
            .unwrap();
        }
    }
}

impl<A, const SIZE: usize> WarningList for [A; SIZE]
where
    A: AsRef<[u8]>,
{
    fn contains(&self, warning: Warning) -> bool {
        for item in self {
            if warning == *item {
                return true;
            }
        }
        false
    }
    fn write_fmt_to_str(&self, s: &mut String) {
        use std::fmt::Write as _;
        for (i, item) in self.iter().enumerate() {
            let prefix = match i {
                0 => "",
                i if i == self.len() - 1 => ", or ",
                _ => ", ",
            };
            s.write_fmt(format_args!(
                "{}{}",
                prefix,
                String::from_utf8_lossy(item.as_ref())
            ))
            .unwrap();
        }
    }
}

impl<T: WarningList> WarningList for &T {
    fn contains(&self, warning: Warning) -> bool {
        (*self).contains(warning)
    }
    fn write_fmt_to_str(&self, s: &mut String) {
        (*self).write_fmt_to_str(s)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ascii::{parse::Packet, Reply};

    #[test]
    fn check_reply() {
        struct Case<'a> {
            reply: Reply,
            checker: &'a dyn Check<Reply>,
            expected: Result<(), AsciiCheckError<Reply>>,
        }

        let ok_busy_reply =
            Reply::try_from_packet(&Packet::new(b"@01 1 12 OK BUSY -- 0\r\n").unwrap()).unwrap();
        let ok_idle_reply =
            Reply::try_from_packet(&Packet::new(b"@01 1 12 OK IDLE -- 0\r\n").unwrap()).unwrap();
        let ok_idle_ff_reply =
            Reply::try_from_packet(&Packet::new(b"@01 1 12 OK IDLE FF 0\r\n").unwrap()).unwrap();
        let ok_idle_wh_reply =
            Reply::try_from_packet(&Packet::new(b"@01 1 12 OK IDLE WH 0\r\n").unwrap()).unwrap();
        let ok_idle_ni_reply =
            Reply::try_from_packet(&Packet::new(b"@01 1 12 OK IDLE NI 0\r\n").unwrap()).unwrap();
        let rj_idle_reply =
            Reply::try_from_packet(&Packet::new(b"@01 1 12 RJ IDLE -- BADCOMMAND\r\n").unwrap())
                .unwrap();

        let predicate_check = predicate(|reply: &Reply| {
            if reply.flag() == Flag::Ok {
                reply.status() == Status::Idle
            } else {
                reply.data() == "0"
            }
        });

        let cases = &[
            Case {
                reply: ok_busy_reply.clone(),
                checker: &flag_ok(),
                expected: Ok(()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &flag_is(Flag::Rj),
                expected: Err(AsciiCheckFlagError::new(Flag::Rj, ok_busy_reply.clone()).into()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &flag_ok_and(status_is(Status::Busy)),
                expected: Ok(()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &flag_ok_and(status_is(Status::Idle)),
                expected: Err(
                    AsciiCheckStatusError::new(Status::Idle, ok_busy_reply.clone()).into(),
                ),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &unchecked(),
                expected: Ok(()),
            },
            Case {
                reply: rj_idle_reply.clone(),
                checker: &unchecked(),
                expected: Ok(()),
            },
            Case {
                reply: ok_idle_reply.clone(),
                checker: &strict(),
                expected: Ok(()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &strict(),
                expected: Ok(()),
            },
            Case {
                reply: ok_idle_ni_reply.clone(),
                checker: &strict(),
                expected: Err(AsciiCheckWarningError::new(
                    "expected -- warning flag",
                    ok_idle_ni_reply.clone(),
                )
                .into()),
            },
            Case {
                reply: rj_idle_reply.clone(),
                checker: &strict(),
                expected: Err(AsciiCheckFlagError::new(Flag::Ok, rj_idle_reply.clone()).into()),
            },
            Case {
                reply: ok_idle_reply.clone(),
                checker: &minimal(),
                expected: Ok(()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &minimal(),
                expected: Ok(()),
            },
            Case {
                reply: ok_idle_ni_reply.clone(),
                checker: &minimal(),
                expected: Ok(()),
            },
            Case {
                reply: ok_idle_wh_reply.clone(),
                checker: &minimal(),
                expected: Ok(()),
            },
            Case {
                reply: ok_idle_ff_reply.clone(),
                checker: &minimal(),
                expected: Err(AsciiCheckWarningError::new(
                    "expected warning below fault (F) level",
                    ok_idle_ff_reply.clone(),
                )
                .into()),
            },
            Case {
                reply: rj_idle_reply.clone(),
                checker: &minimal(),
                expected: Err(AsciiCheckFlagError::new(Flag::Ok, rj_idle_reply.clone()).into()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &warning_is("WR"),
                expected: Err(AsciiCheckWarningError::new(
                    "expected WR warning flag",
                    ok_busy_reply.clone(),
                )
                .into()),
            },
            Case {
                reply: ok_idle_wh_reply.clone(),
                checker: &warning_is("WH"),
                expected: Ok(()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &warning_is_none(),
                expected: Ok(()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &warning_in(("WR", Warning::NONE)),
                expected: Ok(()),
            },
            Case {
                reply: ok_idle_ff_reply.clone(),
                checker: &warning_in(("WR", Warning::NONE)),
                expected: Err(AsciiCheckWarningError::new(
                    "expected one of WR or -- warning flag(s)",
                    ok_idle_ff_reply.clone(),
                )
                .into()),
            },
            Case {
                reply: ok_idle_ff_reply.clone(),
                checker: &warning_below_fault(),
                expected: Err(AsciiCheckWarningError::new(
                    "expected warning below fault (F) level",
                    ok_idle_ff_reply.clone(),
                )
                .into()),
            },
            Case {
                reply: ok_idle_wh_reply.clone(),
                checker: &warning_below_fault(),
                expected: Ok(()),
            },
            Case {
                reply: ok_idle_wh_reply.clone(),
                checker: &warning_below_warning(),
                expected: Err(AsciiCheckWarningError::new(
                    "expected warning below warning (W) level",
                    ok_idle_wh_reply.clone(),
                )
                .into()),
            },
            Case {
                reply: ok_idle_ni_reply.clone(),
                checker: &warning_below_warning(),
                expected: Ok(()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &all((
                    flag_is(Flag::Ok),
                    warning_in(&["--"]),
                    status_is(Status::Busy),
                )),
                expected: Ok(()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &parsed_data_is(0),
                expected: Ok(()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &parsed_data_is(1),
                expected: Err(
                    AsciiCheckDataError::new("expected data 1", ok_busy_reply.clone()).into(),
                ),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &parsed_data_is(std::num::NonZeroIsize::new(1).unwrap()),
                expected: Err(AsciiCheckDataError::new(
                    "could not parse data as expected type",
                    ok_busy_reply.clone(),
                )
                .into()),
            },
            Case {
                reply: ok_busy_reply.clone(),
                checker: &predicate_check,
                expected: Err(AsciiCheckCustomError::new(
                    "invalid response",
                    ok_busy_reply.clone(),
                )
                .into()),
            },
            Case {
                reply: ok_idle_reply.clone(),
                checker: &predicate_check,
                expected: Ok(()),
            },
        ];

        for (i, case) in cases.iter().enumerate() {
            println!("case {}: {}", i, case.reply);
            let actual = case.checker.check(case.reply.clone());
            match &case.expected {
                Ok(_) => assert!(actual.is_ok(), "unexpected error: {actual:?}"),
                Err(expected_err) => {
                    let actual_err = actual.unwrap_err();
                    assert_eq!(*expected_err, actual_err);
                }
            }
        }
    }

    #[test]
    fn test_warning_list() {
        // Make sure a warning list can be created from all the relevant types
        warning_in::<Reply, _>((
            Warning::from(b"FF"),
            "WR",
            b"RR",
            "--".to_string(),
            b"--".to_vec(),
        ));
    }
}
