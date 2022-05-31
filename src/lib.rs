//! Library learning using [anti-unification] of e-graphs.
//!
//! [anti-unification]: https://en.wikipedia.org/wiki/Anti-unification_(computer_science)

#![warn(
    clippy::all,
    clippy::pedantic,
    anonymous_parameters,
    elided_lifetimes_in_paths,
    missing_copy_implementations,
    missing_debug_implementations,
    trivial_casts,
    unreachable_pub,
    unused_lifetimes,
    missing_docs
)]
#![allow(clippy::non_ascii_literal)]

pub mod ast_node;
mod dfta;
pub mod dreamcoder;
pub mod experiments;
pub mod extract;
pub mod learn;
pub mod sexp;
pub mod simple_lang;
pub mod teachable;
