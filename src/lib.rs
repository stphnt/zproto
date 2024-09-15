//! A simple library for communicating with Zaber products.
//!
//! Both Zaber's ASCII and Binary protocols are supported over serial and TCP
//! ports. See the [`ascii`] and [`binary`] modules for an introduction to their
//! use.

#![cfg_attr(all(doc, feature = "doc_cfg"), feature(doc_cfg))]

// This lint is denied here rather than in the Cargo.toml because we don't want it applied
// to the example scripts as well (where it always fails).
#![cfg_attr(not(test), deny(unused_crate_dependencies))]

#[cfg(feature = "ascii")]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "ascii")))]
pub mod ascii;
pub mod backend;
#[cfg(feature = "binary")]
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "binary")))]
pub mod binary;
pub mod convert;
pub mod error;
pub mod routine;
pub mod timeout_guard;

#[cfg(not(any(feature = "ascii", feature = "binary")))]
compile_error!("At least one of the `ascii` or `binary` features must be specified");

// Check the contents of the README (but don't include it in the docs).
#[doc = include_str!("../README.md")]
#[allow(unused_doc_comments)]
extern "C" {}
