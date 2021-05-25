//! Types and utilities for interacting with Dream&shy;Coder.

use itertools::Itertools;

use self::json::{CompressionInput, CompressionOutput};

// Note: We write Dream&shy;Coder instead of DreamCoder to avoid a false
// positive from a clippy lint.

pub mod json;
pub mod types;
pub mod expr;
mod parse;
mod util;

/// Emulate Dream&shy;Coder's compression tool.
#[must_use]
pub fn run(input: CompressionInput) -> CompressionOutput {
    eprintln!("Emulating DreamCoder's `compression` tool.");

    eprintln!(
        "Primitives:\n    {}",
        &input
            .dsl
            .productions
            .iter()
            .map(|p| &p.expression)
            .chunks(10)
            .into_iter()
            .map(|mut exprs| exprs.join(", "))
            .join(",\n    ")
    );

    eprintln!("Tasks:");
    for frontier in &input.frontiers {
        eprintln!(
            "    {}: {}",
            frontier.task.as_ref().map_or("<unnamed>", |s| s.as_ref()),
            frontier.request
        );
    }

    CompressionOutput {
        dsl: input.dsl,
        frontiers: input.frontiers,
    }
}
