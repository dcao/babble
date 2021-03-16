//! Rewrite rule for rotating a function up.

use crate::Rewrite;
use egg::rewrite as rw;

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

pub fn rotate_rw() -> Vec<Rewrite> {
    vec![
        rw!("rotate-move-1"; "(move (app (fn ?p ?b) ?a) ?h1 ?h2)" => "(app (fn ?p (move ?b ?h1 ?h2)) ?a)"),
        rw!("rotate-move-2"; "(move ?h1 (app (fn ?p ?b) ?a) ?h2)" => "(app (fn ?p (move ?h1 ?b ?h2)) ?a)"),
        rw!("rotate-move-3"; "(move ?h1 ?h2 (app (fn ?p ?b) ?a))" => "(app (fn ?p (move ?h1 ?h2 ?b)) ?a)"),
        rw!("rotate-scale-1"; "(scale (app (fn ?p ?b) ?a) ?h1)" => "(app (fn ?p (scale ?b ?h1)) ?a)"),
        rw!("rotate-scale-2"; "(scale ?h1 (app (fn ?p ?b) ?a))" => "(app (fn ?p (scale ?h1 ?b)) ?a)"),
        rw!("rotate-rotate-1"; "(rotate (app (fn ?p ?b) ?a) ?h1)" => "(app (fn ?p (rotate ?b ?h1)) ?a)"),
        rw!("rotate-rotate-2"; "(rotate ?h1 (app (fn ?p ?b) ?a))" => "(app (fn ?p (rotate ?h1 ?b)) ?a)"),
        rw!("rotate-compose-1"; "(+ (app (fn ?p ?b) ?a) ?h1)" => "(app (fn ?p (+ ?b ?h1)) ?a)"),
        rw!("rotate-compose-2"; "(+ ?h1 (app (fn ?p ?b) ?a))" => "(app (fn ?p (+ ?h1 ?b)) ?a)"),

        rw!("collapse-fn"; "(app (fn ?p (app (fn ?p ?b) ?a)) ?a)" => "(app (fn ?p ?b) ?a)"),

        rw!("fn-commute"; "(app (fn ?p1 (app (fn ?p2 ?b) ?a2)) ?a1)" => "(app (fn ?p2 (app (fn ?p1 ?b) ?a1)) ?a2)"),
        rw!("compose-commute-1"; "(+ ?a (+ ?b ?c))" => "(+ ?b (+ ?a ?c))"),
        rw!("compose-commute-2"; "(+ ?a ?b)" => "(+ ?b ?a)"),
    ]
}

/// Returns a list containing a single rewrite which consolidates consecutive
/// function introductions and applications.
pub fn adjacent_rw() -> Vec<Rewrite> {
    vec![rw!("adjacent";
            "(app (fn ?p1 (app (fn ?p2 ?b) ?a2)) ?a1)" =>
            "(app (app (fn ?p1 (fn ?p2 ?b)) ?a1) ?a2)")]
}
