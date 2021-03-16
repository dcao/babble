//! Rewrite rules for anti-unifying expressions

use crate::Rewrite;
use egg::rewrite as rw;

/// Rewrite rules for anti-unifying expressions.
/// At the moment, we only support anti-unifying two expressions, and
/// we can only anti-unify functions at the top-level. But it's a start...
pub fn anti_unif_rw() -> Vec<Rewrite> {
    vec![
        rw!("anti-unify-zero";
            "(let ?v1 ?a (let ?v2 ?a ?r))" =>
            "(let f ?a (let ?v1 f (let ?v2 f ?r)))"),
        rw!("anti-unify-one";
            "(let ?v1 (app (fn ?a ?f) ?a1) (let ?v2 (app (fn ?a ?f) ?a2) ?r))" =>
            "(let f (fn ?a ?f) (let ?v1 (app f ?a1) (let ?v2 (app f ?a2) ?r)))"),
        rw!("anti-unify-two";
            "(let ?v1 (app (app ?f ?a1) ?a2) (let ?v2 (app (app ?f ?b1) ?b2) ?r))" =>
            "(let f ?f (let ?v1 (app (app f ?a1) ?a2) (let ?v2 (app (app f ?b1) ?b2) ?r)))"),
        rw!("anti-unify-three";
            "(let ?v1 (app (app (app ?f ?a1) ?a2) ?a3) (let ?v2 (app (app (app ?f ?b1) ?b2) ?b3) ?r))" =>
            "(let f ?f (let ?v1 (app (app (app f ?a1) ?a2) ?a3) (let ?v2 (app (app (app f ?b1) ?b2) ?b3) ?r)))"),
    ]
}
