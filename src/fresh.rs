//! Utilities for generating fresh variable names.

use egg::Symbol;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Shared counter for producing fresh variables.
static COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Return a fresh variable name beginning with `prefix`.
pub fn gen(prefix: &str) -> Symbol {
    let i = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}{}", prefix, i).into()
}
