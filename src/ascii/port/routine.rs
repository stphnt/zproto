//! Routines for common operations on a port.

use crate::{
    ascii::{
        check::{self, Check},
        Command, Port, Reply, Response,
    },
    backend::Backend,
    error::AsciiError,
};

/// Any type that executes a routine operation (e.g., send a command and then read it's reply) on a
/// port, and then check the output of the operation.
pub trait Routine<R: Response>: Size {
    /// Perform the operation and check the output using the provided `checker`.
    fn check<K: Check<R>>(self, checker: K) -> Result<R, AsciiError>;
    /// Perform the operation and check the output using [`strict`](check::strict) checks.
    fn check_minimal(self) -> Result<R, AsciiError> {
        self.check(check::minimal())
    }
    /// Perform the operation and check the output using [`minimal`](check::minimal) checks.
    fn check_strict(self) -> Result<R, AsciiError> {
        self.check(check::strict())
    }
}

/// A routine to send a command and then read a single response.
#[derive(Debug)]
#[must_use]
pub struct CommandReply<'a, 'p, B, C> {
    /// The port to send the command over
    pub(crate) port: &'a mut Port<'p, B>,
    /// The command to send.
    pub(crate) cmd: C,
}

impl<'a, 'p, B, C> Routine<Reply> for CommandReply<'a, 'p, B, C>
where
    B: Backend,
    C: Command,
{
    fn check<K: Check<Reply>>(self, checker: K) -> Result<Reply, AsciiError> {
        let cmd = self.cmd.as_ref();
        let id = self.port.command(cmd)?;
        self.port.pre_receive_response();
        let response = self.port.receive_response(
            |_| super::HeaderCheckAction::Check {
                target: cmd.target(),
                id,
            },
            Some(checker),
        )?;
        self.port.post_receive_response()?;
        Ok(response)
    }
}
