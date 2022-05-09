//! Types and utilities for interacting with Dream&shy;Coder.

// Note: We write Dream&shy;Coder instead of DreamCoder to avoid a false
// positive from a clippy lint.

pub mod expr;
pub mod json;
mod parse;
pub mod types;
mod util;
