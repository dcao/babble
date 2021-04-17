//! Rewrites which enable anti-unification.

use babble_macros::rewrite_rules;
use lazy_static::lazy_static;

pub use crate::Rewrite;

lazy_static! {
    /// Rewrite rules for graphics primitives.
    pub static ref BASE: &'static [Rewrite] = rewrite_rules! {
        scale_base_circle: "circle" => "(scale 1 circle)";
        scale_base_line: "line" => "(scale 1 line)";
        move_base_circle: "circle" => "(move 0 0 circle)";
        move_base_circle: "line" => "(move 0 0 line)";
    }
    .leak();

    /// Rewrite rules introducing identity function applications.
    pub static ref INTRO_FN: &'static [Rewrite] = rewrite_rules! {
        intro_fn_circle: "circle" => "(app (fn s s) circle)";
        intro_fn_line: "line" => "(app (fn s s) line)";
        // intro_fn_move_id: "(move ?x ?y ?b)" => "(app (fn m m) (move ?x ?y ?b))";
        intro_fn_move_x: "(move ?x ?y ?b)" => "(move (app (fn x x) ?x) ?y ?b)";
        intro_fn_move_y: "(move ?x ?y ?b)" => "(move ?x (app (fn y y) ?y) ?b)";
        intro_fn_move_both: "(move ?v ?v ?b)" => "(app (fn xy (move xy xy ?b)) ?v)";
        // intro_fn_move_b: "(move ?x ?y ?b)" => "(move ?x ?y (app (fn mb mb) ?b))";
        // intro_fn_scale_id: "(scale ?z ?b)" => "(app (fn sc sc) (scale ?z ?b))";
        intro_fn_scale_z: "(scale ?z ?b)" => "(scale (app (fn z z) ?z) ?b)";
        // intro_fn_scale_b: "(scale ?z ?b)" => "(scale ?z (app (fn sb sb) ?b))";
        // intro_fn_rotate_id: "(rotate ?r ?b)" => "(app (fn rt rt) (rotate ?r ?b))";
        intro_fn_rotate_r: "(rotate ?r ?b)" => "(rotate (app (fn r r) ?r) ?b)";
        // intro_fn_rotate_b: "(rotate ?r ?b)" => "(rotate ?r (app (fn rb rb) ?b))";
    }
    .leak();

    /// Rewrite rules for rotating a function up.
    // TODO: Do we need a custom searcher to also match:
    // (anything [varargs] (app (fn ?f ?b) ?a) [varargs])
    // or
    // ((app (fn ?f ?b) a) [varargs])
    //
    // Should we even consider the fn case?
    // Just focus on the args first probably idk
    //
    // This doesn't handle lets and more advanced things tho - it's just a stopgap
    // intro rewrites:
    // (circle) -> (app (fn x x) (circle))
    // (line) -> (app (fn x x) (line))
    // x (num/var) -> (app (fn x x) (line))
    //
    // (move (app (fn ?p ?b) ?a) ?h1 ?h2)
    // (move ?h1 (app (fn ?p ?b) ?a) ?h2)
    // (move ?h1 ?h2 (app (fn ?p ?b) ?a))
    // (scale (app (fn ?p ?b) ?a) ?h1 ?h2)
    // (scale ?h1 (app (fn ?p ?b) ?a) ?h2)
    // (scale ?h1 ?h2 (app (fn ?p ?b) ?a))
    // (rotate (app (fn ?p ?b) ?a) ?h1)
    // (rotate ?h1 (app (fn ?p ?b) ?a))
    pub static ref ROTATE_FN: &'static [Rewrite] = rewrite_rules! {
        rotate_move_1: "(move (app (fn ?p ?b) ?a) ?h1 ?h2)" => "(app (fn ?p (move ?b ?h1 ?h2)) ?a)";
        rotate_move_2: "(move ?h1 (app (fn ?p ?b) ?a) ?h2)" => "(app (fn ?p (move ?h1 ?b ?h2)) ?a)";
        rotate_move_3: "(move ?h1 ?h2 (app (fn ?p ?b) ?a))" => "(app (fn ?p (move ?h1 ?h2 ?b)) ?a)";
        rotate_scale_1: "(scale (app (fn ?p ?b) ?a) ?h1)" => "(app (fn ?p (scale ?b ?h1)) ?a)";
        rotate_scale_2: "(scale ?h1 (app (fn ?p ?b) ?a))" => "(app (fn ?p (scale ?h1 ?b)) ?a)";
        rotate_rotate_1: "(rotate (app (fn ?p ?b) ?a) ?h1)" => "(app (fn ?p (rotate ?b ?h1)) ?a)";
        rotate_rotate_2: "(rotate ?h1 (app (fn ?p ?b) ?a))" => "(app (fn ?p (rotate ?h1 ?b)) ?a)";
        rotate_compose_1: "(+ (app (fn ?p ?b) ?a) ?h1)" => "(app (fn ?p (+ ?b ?h1)) ?a)";
        rotate_compose_2: "(+ ?h1 (app (fn ?p ?b) ?a))" => "(app (fn ?p (+ ?h1 ?b)) ?a)";

        collapse_fn: "(app (fn ?p (app (fn ?p ?b) ?a)) ?a)" => "(app (fn ?p ?b) ?a)";

        fn_commute: "(app (fn ?p1 (app (fn ?p2 ?b) ?a2)) ?a1)" => "(app (fn ?p2 (app (fn ?p1 ?b) ?a1)) ?a2)";
        compose_commute_1: "(+ ?a (+ ?b ?c))" => "(+ ?b (+ ?a ?c))";
        compose_commute_2: "(+ ?a ?b)" => "(+ ?b ?a)";
    }
    .leak();

    /// A single rewrite which consolidates consecutive
    /// function introductions and applications.
    pub static ref ADJACENT: &'static [Rewrite] = rewrite_rules! {
        adjacent: "(app (fn ?p1 (app (fn ?p2 ?b) ?a2)) ?a1)"
            => "(app (app (fn ?p1 (fn ?p2 ?b)) ?a1) ?a2)";
    }
    .leak();

    /// Rewrite rules for anti-unification.
    pub static ref ANTI_UNIFY: &'static [Rewrite] = rewrite_rules! {
        anti_unify_zero: "(let ?v1 ?a (let ?v2 ?a ?r))" => "(let f ?a (let ?v1 f (let ?v2 f ?r)))";
        anti_unify_one: "(let ?v1 (app (fn ?a ?f) ?a1) (let ?v2 (app (fn ?a ?f) ?a2) ?r))"
            => "(let f (fn ?a ?f) (let ?v1 (app f ?a1) (let ?v2 (app f ?a2) ?r)))";
        anti_unify_two: "(let ?v1 (app (app ?f ?a1) ?a2) (let ?v2 (app (app ?f ?b1) ?b2) ?r))"
            => "(let f ?f (let ?v1 (app (app f ?a1) ?a2) (let ?v2 (app (app f ?b1) ?b2) ?r)))";
        anti_unify_three: "(let ?v1 (app (app (app ?f ?a1) ?a2) ?a3) (let ?v2 (app (app (app ?f ?b1) ?b2) ?b3) ?r))"
            => "(let f ?f (let ?v1 (app (app (app f ?a1) ?a2) ?a3) (let ?v2 (app (app (app f ?b1) ?b2) ?b3) ?r)))";
    }
    .leak();
}
