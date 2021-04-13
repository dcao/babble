use crate::smiley_lang::Rewrite;
use egg::rewrite as rw;

/*
struct IntroFresh;

// x -> (app (fn ?fresh ?fresh) x)
impl Applier<Smiley, ()> for IntroFresh {
    fn apply_one(&self, egraph: &mut EGraph, eclass: Id, _subst: &Subst) -> Vec<Id> {
        let sym = egraph.add(Smiley::Symbol(format!("x").into()));

        let f = egraph.add(Smiley::Fn([sym, sym]));
        let a = egraph.add(Smiley::App([f, eclass]));

        vec![a]
    }
}
*/

// TODO: less janky solution for fresh params and variable capture
// See egg/tests/lambda.rs
pub fn intro_fn() -> Vec<Rewrite> {
    vec![
        rw!("intro-fn-circle"; "circle" => "(app (fn s s) circle)"),
        rw!("intro-fn-line"; "line" => "(app (fn s s) line)"),
        // rw!("intro-fn-move-id"; "(move ?x ?y ?b)" => "(app (fn m m) (move ?x ?y ?b))"),
        rw!("intro-fn-move-x"; "(move ?x ?y ?b)" => "(move (app (fn x x) ?x) ?y ?b)"),
        rw!("intro-fn-move-y"; "(move ?x ?y ?b)" => "(move ?x (app (fn y y) ?y) ?b)"),
        rw!("intro-fn-move-both"; "(move ?v ?v ?b)" => "(app (fn xy (move xy xy ?b)) ?v)"),
        // rw!("intro-fn-move-b"; "(move ?x ?y ?b)" => "(move ?x ?y (app (fn mb mb) ?b))"),
        // rw!("intro-fn-scale-id"; "(scale ?z ?b)" => "(app (fn sc sc) (scale ?z ?b))"),
        rw!("intro-fn-scale-z"; "(scale ?z ?b)" => "(scale (app (fn z z) ?z) ?b)"),
        // rw!("intro-fn-scale-b"; "(scale ?z ?b)" => "(scale ?z (app (fn sb sb) ?b))"),
        // rw!("intro-fn-rotate-id"; "(rotate ?r ?b)" => "(app (fn rt rt) (rotate ?r ?b))"),
        rw!("intro-fn-rotate-r"; "(rotate ?r ?b)" => "(rotate (app (fn r r) ?r) ?b)"),
        // rw!("intro-fn-rotate-b"; "(rotate ?r ?b)" => "(rotate ?r (app (fn rb rb) ?b))"),
    ]
}
