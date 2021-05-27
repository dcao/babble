//! The AST defining the smiley language.

// TODO: Remove this once define_language! allows doc strings
#![allow(missing_docs)]

use babble_macros::rewrite_rules;
use crate::{anti_unify::{AntiUnifTgt, AUAnalysis}};
use egg::{Language, define_language, Id, Analysis, Symbol};
use ordered_float::NotNan;
use hashbrown::HashSet;

/// E-graphs in the `Smiley` language.
pub type EGraph = egg::EGraph<Smiley, SmileyAnalysis>;

/// Rewrites in the `Smiley` language.
pub type Rewrite = egg::Rewrite<Smiley, SmileyAnalysis>;

/// Number constants in the `Smiley` language.
pub type Constant = NotNan<f64>;

define_language! {
    /// A smiley expression
    pub enum Smiley {
        Signed(i32),
        Float(Constant),

        // Meta:
        "let" = Let([Id; 3]),
        "lib" = Lib([Id; 3]),
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
    move |_, _, x| {
        x[a] < x[b]
    }
}

#[derive(Default, Clone, Copy, Debug)]
pub struct SmileyAnalysis;

impl Analysis<Smiley> for SmileyAnalysis {
    type Data = HashSet<Symbol>;

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> Option<std::cmp::Ordering> {
        let before_len = to.len();
        // to.free.extend(from.free);
        to.retain(|i| from.contains(i));
        let did_change = before_len != to.len();
        if did_change {
            None
        } else {
            Some(std::cmp::Ordering::Greater)
        }
    }

    fn make(egraph: &EGraph, enode: &Smiley) -> Self::Data {
        let f = |i: &Id| egraph[*i].data.iter().cloned();
        let mut free = HashSet::default();
        match enode {
            Smiley::Symbol(v) => {
                if v.as_str().starts_with("fn_") {
                    free.insert(*v);
                }
            }
            Smiley::Lib([v, a, b]) => {
                free.extend(f(b));
                egraph[*v].iter().for_each(|x| match x { Smiley::Symbol(x) => { free.remove(x); }, _ => {} });
                free.extend(f(a));
            }
            _ => enode.for_each(|c| free.extend(&egraph[c].data)),
        }
        free
    }
}

impl AUAnalysis<Smiley> for SmileyAnalysis {}

impl AntiUnifTgt for Smiley {
    type Analysis = SmileyAnalysis;
    
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
        Self::Symbol(egg::Symbol::from(format!("${}", ix.to_string())))
    }

    fn fn_sym(hash: u64) -> Self {
        Self::Symbol(egg::Symbol::from(format!("fn_{}", hash.to_string())))
    }

    fn lib(name: Id, lam: Id, body: Id) -> Self {
        Self::Lib([name, lam, body])
    }

    fn lift_lets() -> Vec<egg::Rewrite<Self, Self::Analysis>> {
        rewrite_rules! {
            // fixme: lib-let lifting. experimental
            lift_lib_let: "(let ?a1 ?b1 (lib ?a2 ?b2 ?c))" => "(lib ?a2 ?b2 (let ?a1 ?b1 ?c))";
            lift_lib_let_2: "(let ?a (lib ?fn ?body ?app) ?c)" => "(lib ?fn ?body (let ?a ?app ?c))";
            unify_lib: "(lib ?a ?b (lib ?a ?b ?c))" => "(lib ?a ?b ?c)";
            order_lib: "(lib ?a1 ?b1 (lib ?a2 ?b2 ?c))" => "(lib ?a2 ?b2 (lib ?a1 ?b1 ?c))" if can_let_lift("?a1", "?a2");
            
            // unify_let: "(let ?a ?b (let ?a ?b ?c))" => "(let ?a ?b ?c)";
            // lift_lets: "(let ?a1 ?b1 (let ?a2 ?b2 ?c))" => "(let ?a2 ?b2 (let ?a1 ?b1 ?c))" if can_let_lift("?a1", "?a2");
            // lift_lets_2: "(let ?a (let ?fn ?body ?app) ?c)" => "(let ?fn ?body (let ?a ?app ?c))";
            todo_pls_delete: "(move 5 2 ?15)" => "(lib fn_1176429130788038353 (fn (move 5 2 $0)) (app fn_1176429130788038353 ?15))";

            // lift_let_move_1: "(move (let ?a ?b ?c) ?m1 ?m2)" => "(let ?a ?b (move ?c ?m1 ?m2))";
            // lift_let_move_2: "(move ?m1 (let ?a ?b ?c) ?m2)" => "(let ?a ?b (move ?m1 ?c ?m2))";
            // lift_let_move_3: "(move ?m1 ?m2 (let ?a ?b ?c))" => "(let ?a ?b (move ?m1 ?m2 ?c))";
            // lift_let_scale_1: "(scale (let ?a ?b ?c) ?m1)" => "(let ?a ?b (scale ?c ?m1))";
            // lift_let_scale_2: "(scale ?m1 (let ?a ?b ?c))" => "(let ?a ?b (scale ?m1 ?c))";
            // lift_let_rotate_1: "(rotate (let ?a ?b ?c) ?m1)" => "(let ?a ?b (rotate ?c ?m1))";
            // lift_let_rotate_2: "(rotate ?m1 (let ?a ?b ?c))" => "(let ?a ?b (rotate ?m1 ?c))";
            // lift_let_compose_1: "(+ (let ?a ?b ?c) ?m1)" => "(let ?a ?b (+ ?c ?m1))";
            // lift_let_compose_2: "(+ ?m1 (let ?a ?b ?c))" => "(let ?a ?b (+ ?m1 ?c))";
            // lift_let_lambda: "(fn (let ?a ?b ?c))" => "(let ?a ?b (fn ?c))";
            // lift_let_app_1: "(app ?m1 (let ?a ?b ?c))" => "(let ?a ?b (app ?m1 ?c))";
            // lift_let_app_2: "(app ?m1 (let ?a ?b ?c))" => "(let ?a ?b (app ?c ?m1))";
        }
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
    let mut g = EGraph::new(SmileyAnalysis::default());

    // First, parse the expression and build an egraph from it
    let expr = expr.parse().unwrap();
    let root = g.add_expr(&expr);

    crate::anti_unify::anti_unify(g, root);
}
