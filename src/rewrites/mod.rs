//! Rewrites which enable anti-unification.

pub mod anti_unify;
pub mod intro;
pub mod rotate;

pub use anti_unify::*;
pub use intro::*;
pub use rotate::*;

pub use crate::Rewrite;
pub use egg::rewrite as rw;

pub fn base_rw() -> Vec<Rewrite> {
    vec![
        rw!("scale-base-circle"; "circle" => "(scale 1 circle)"),
        rw!("scale-base-line"; "line" => "(scale 1 line)"),
        rw!("move-base-circle"; "circle" => "(move 0 0 circle)"),
        rw!("move-base-line"; "line" => "(move 0 0 line)"),
    ]
}
