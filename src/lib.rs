//! A simple but easy to use library for communicating with Zaber products.
//!
//! Both Zaber's [`ASCII`](ascii) and [`Binary`](binary) protocols are supported
//! over serial and TCP ports.

#![deny(missing_docs)]
#![deny(rustdoc::missing_crate_level_docs)]
#![deny(missing_debug_implementations)]
#![cfg_attr(all(doc, feature = "doc_cfg"), feature(doc_cfg))]

#[cfg(feature = "ascii")]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub mod ascii;
pub mod backend;
#[cfg(feature = "binary")]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "binary")))]
pub mod binary;
pub mod error;
pub mod timeout_guard;

#[cfg(not(any(feature = "ascii", feature = "binary")))]
compile_error!("At least one of the `ascii` or `binary` features must be specified");
