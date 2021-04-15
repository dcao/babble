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
    unused_lifetimes
)]
#![allow(clippy::multiple_crate_versions)]

//! babble root library

pub mod anti_unify;
mod macros;
pub mod rewrites;
pub mod smiley_lang;

pub use smiley_lang::Rewrite;
