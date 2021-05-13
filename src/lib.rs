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
    single_use_lifetimes,
    trivial_casts,
    unreachable_pub,
    unused_lifetimes,
    missing_docs
)]

pub mod anti_unify;
pub mod smiley_lang;

pub use smiley_lang::Rewrite;
