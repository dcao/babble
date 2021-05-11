//! The AST defining the smiley language.

// TODO: Remove this once define_language! allows doc strings
#![allow(missing_docs)]

use babble_macros::rewrite_rules;
use crate::{anti_unify::AntiUnifTgt, rewrites};
use egg::{define_language, CostFunction, Extractor, Id, Language, RecExpr, Runner};
use ordered_float::NotNan;

/// E-graphs in the `Smiley` language.
pub type EGraph = egg::EGraph<Smiley, ()>;

/// Rewrites in the `Smiley` language.
pub type Rewrite = egg::Rewrite<Smiley, ()>;

/// Number constants in the `Smiley` language.
pub type Constant = NotNan<f64>;

define_language! {
    /// A smiley expression
    pub enum Smiley {
        Signed(i32),
        Float(Constant),

        // Meta:
        "let" = Let([Id; 3]),
        "fn" = Fn(Id),
        "app" = App([Id; 2]),
        "var" = Var(Id),

        // Shapes:
        "circle" = Circle,
        "line" = Line,

        "move" = Move([Id; 3]),
        "scale" = Scale([Id; 2]),
        "rotate" = Rotate([Id; 2]),
        "+" = Compose([Id; 2]),

        Symbol(egg::Symbol),
    }
}

// returns a Condition - when let lifting between two lets, when given two vars,
// if the second has a smaller id, it can be lifted.
fn can_let_lift(a: &'static str, b: &'static str) -> impl Fn(&mut EGraph, Id, &egg::Subst) -> bool {
    let a = a.parse().unwrap();
    let b = b.parse().unwrap();
    move |_, _, subst| {
        subst[b] < subst[a]
    }
}

impl AntiUnifTgt for Smiley {
    fn lambda(body: Id) -> Self {
        Self::Fn(body)
    }

    fn is_lambda(node: &Self) -> bool {
        match node {
            Self::Fn(_) => true,
            _ => false,
        }
    }

    fn app(lambda: Id, arg: Id) -> Self {
        Self::App([lambda, arg])
    }

    fn lambda_arg(ix: usize) -> Self {
        Self::Symbol(egg::Symbol::from(format!("arg_{}", ix.to_string())))
    }

    fn fn_sym(hash: u64) -> Self {
        Self::Symbol(egg::Symbol::from(format!("fn_{}", hash.to_string())))
    }

    fn lib(name: Id, lam: Id, body: Id) -> Self {
        Self::Let([name, lam, body])
    }

    fn lift_lets() -> Vec<egg::Rewrite<Self, ()>> {
        rewrite_rules! {
            // unify_let: "(let ?a ?b (let ?a ?b ?c))" => "(let ?a ?b ?c)"; // todo: this is unifying different symbols lol
            // lift_lets: "(let ?a1 ?b1 (let ?a2 ?b2 ?c))" => "(let ?a2 ?b2 (let ?a1 ?b1 ?c))" if can_let_lift("?a1", "?b1");
            // lift_lets_2: "(let ?a (let ?fn ?body ?app) ?c)" => "(let ?fn ?body (let ?a ?app ?c))";
            // todo: for testing
            lift_lets_1: "(let s1 (let ?fname (fn ?body) ?z) ?b)" => "?z";
            manual_1: "(+ (move 4 4 ?a) (+ (move 3 2 ?b) (+ (move 4 3 (scale 9 ?c)) (move 5 2 ?d))))" => "(let fn_10203736404544521385 (fn (+ (move 4 4 (scale 2 arg_0)) (+ (move 3 2 arg_0) (+ (move 4 3 (scale 9 circle)) (move 5 2 arg_0))))) (app fn_10203736404544521385 82))";
            manual_2: "(+ (move 4 4 (scale 2 ?a)) ?b)" => "whatever";
            manual_3: "(+ (move 4 4 (scale ?a line)) ?b)" => "whatever";
            manual_4: "(+ (move ?a 4 (scale 2 line)) ?b)" => "whatever";
            manual_5: "(+ (move ?a 4 (scale 2 line)) (+ (move 3 2 line) (+ ?c ?d)))" => "whatever";
            manual_6: "(+ (move 4 4 (scale 2 line)) (+ (move 3 2 line) (+ (move 4 3 (scale 9 circle)) ?d)))" => "whatever";
            manual_7: "(+ (move 4 4 (scale 2 line)) (+ (move 3 2 line) (+ (move 4 3 (scale 9 circle)) (move 5 2 line))))" => "whatever";

            // lift_let_move_1: "(move (let ?a ?b ?c) ?m1 ?m2)" => "(let ?a ?b (move ?c ?m1 ?m2))";
            // lift_let_move_2: "(move ?m1 (let ?a ?b ?c) ?m2)" => "(let ?a ?b (move ?m1 ?c ?m2))";
            // lift_let_move_3: "(move ?m1 ?m2 (let ?a ?b ?c))" => "(let ?a ?b (move ?m1 ?m2 ?c))";
            // lift_let_scale_1: "(scale (let ?a ?b ?c) ?m1)" => "(let ?a ?b (scale ?c ?m1))";
            // lift_let_scale_2: "(scale ?m1 (let ?a ?b ?c))" => "(let ?a ?b (scale ?m1 ?c))";
            // lift_let_rotate_1: "(rotate (let ?a ?b ?c) ?m1)" => "(let ?a ?b (rotate ?c ?m1))";
            // lift_let_rotate_2: "(rotate ?m1 (let ?a ?b ?c))" => "(let ?a ?b (rotate ?m1 ?c))";
            // lift_let_compose_1: "(+ (let ?a ?b ?c) ?m1)" => "(let ?a ?b (+ ?c ?m1))";
            // lift_let_compose_2: "(+ ?m1 (let ?a ?b ?c))" => "(let ?a ?b (+ ?m1 ?c))";
            // // lift_let_lambda: "(fn (let ?a ?b ?c))" => "(let ?a ?b (fn ?c))";
            // // lift_let_app_1: "(app ?m1 (let ?a ?b ?c))" => "(let ?a ?b (app ?m1 ?c))";
            // // lift_let_app_2: "(app ?m1 (let ?a ?b ?c))" => "(let ?a ?b (app ?c ?m1))";
        }
    }
}

/// Get a `Vec` of all the rewrite rules.
#[must_use]
pub fn rules() -> Vec<Rewrite> {
    [
        *rewrites::INTRO_FN,
        *rewrites::ANTI_UNIFY,
        *rewrites::ROTATE_FN,
        *rewrites::ADJACENT,
    ]
    .concat()
}

struct ToySize;
impl CostFunction<Smiley> for ToySize {
    type Cost = f64;
    fn cost<C>(&mut self, enode: &Smiley, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            Smiley::Scale(_) | Smiley::Rotate(_) | Smiley::Move(_) => 16.0,
            _ => 1.0,
        };
        enode.fold(op_cost, |sum, id| sum + costs(id))
    }
}

/// Execute `EGraph` building and program extraction on a single expression
/// containing all of the programs to extract common fragments out of.
///
/// Programs should be in the form:
/// ```ignore
/// let e1 = (...)
/// let e2 = (...)
/// let e3 = (...)
/// ```
///
/// # Panics
/// Panics if `expr` is not a valid smiley expression.
#[must_use]
pub fn run_single(expr: &str) {
    let mut g = EGraph::new(());

    // First, parse the expression and build an egraph from it
    let expr = expr.parse().unwrap();
    let root = g.add_expr(&expr);

    crate::anti_unify::anti_unify(g, root);
}
