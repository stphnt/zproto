//! A simple but easy to use library for communicating with Zaber products.
//!
//! Both Zaber's [`ASCII`](ascii) and [`Binary`](binary) protocols are supported
//! over serial and TCP ports.

#![deny(missing_docs)]
#![deny(rustdoc::missing_crate_level_docs)]
#![deny(missing_debug_implementations)]

pub mod ascii;
pub mod backend;
pub mod binary;
pub mod error;
pub mod timeout_guard;
