//! The AST defining the smiley language.

// TODO: Remove this once define_language! allows doc strings
#![allow(missing_docs)]

use crate::{anti_unify::AntiUnifTgt, fresh};
use babble_macros::rewrite_rules;
use egg::{define_language, Analysis, Condition, EClass, Id, Language, Runner, Subst, Symbol};
use ordered_float::NotNan;
use std::{cmp::Ordering, collections::HashSet, iter::FromIterator};

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
        "lambda" = Lambda(Id),
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

/// Analysis which maintains a set of potentially free variables for each
/// e-class. For example, the set of potentially free variables for an e-class
/// representing the expressions `(app f circle)`, `(scale 1 x)`, and `line`
/// will be `{f, x}`.
#[derive(Default, Clone, Copy, Debug)]
pub struct SmileyAnalysis;

impl Analysis<Smiley> for SmileyAnalysis {
    type Data = HashSet<Symbol>;

    /// Set `a` to the union of `a` and `b`.
    fn merge(&self, a: &mut Self::Data, b: Self::Data) -> Option<Ordering> {
        if a.is_subset(&b) {
            if a.is_superset(&b) {
                Some(Ordering::Equal)
            } else {
                *a = b;
                Some(Ordering::Less)
            }
        } else if a.is_superset(&b) {
            Some(Ordering::Greater)
        } else {
            a.extend(b.into_iter());
            None
        }
    }

    /// Return all variables potentially free in `enode`.
    fn make(egraph: &EGraph, enode: &Smiley) -> Self::Data {
        match *enode {
            Smiley::Symbol(var) => HashSet::from_iter([var]),
            Smiley::Let([var, a, b]) | Smiley::Lib([var, a, b]) => {
                let mut free = egraph[b].data.clone();
                for var in &egraph[var].data {
                    free.remove(var);
                }
                free.extend(&egraph[a].data);
                free
            }
            _ => enode
                .children()
                .iter()
                .flat_map(|child| &egraph[*child].data)
                .copied()
                .collect(),
        }
    }
}

impl AntiUnifTgt<SmileyAnalysis> for Smiley {
    fn lambda(body: Id) -> Self {
        Self::Lambda(body)
    }

    fn app(lambda: Id, arg: Id) -> Self {
        Self::App([lambda, arg])
    }

    fn lambda_arg(ix: usize) -> Self {
        Self::Symbol(egg::Symbol::from(format!("${}", ix.to_string())))
    }

    fn fn_sym() -> Self {
        Self::Symbol(fresh::gen("f"))
    }

    fn lib(name: Id, lam: Id, body: Id) -> Self {
        Self::Lib([name, lam, body])
    }

    fn lift_lets() -> Vec<egg::Rewrite<Self, SmileyAnalysis>> {
        rewrite_rules! {
            // TODO: Check for captures of de Bruijn variables and re-index if necessary.
            lift_lambda: "(lambda (lib ?x ?v ?e))" => "(lib ?x ?v (lambda ?e))";

            // (Effectively) unary operators
            lift_scale: "(scale ?a (lib ?x ?v ?e))" => "(lib ?x ?v (scale ?a ?e))";
            lift_rotate: "(rotate ?a (lib ?x ?v ?e))" => "(lib ?x ?v (rotate ?a ?e))";
            lift_move: "(move ?a ?b (lib ?x ?v ?e))" => "(lib ?x ?v (move ?a ?b ?e))";

            // Binary operators
            lift_compose_both: "(+ (lib ?x ?v ?e1) (lib ?x ?v ?e2))" => "(lib ?x ?v (+ ?e1 ?e2))";
            lift_compose_left: "(+ (lib ?x ?v ?e1) ?e2)" => "(lib ?x ?v (+ ?e1 ?e2))" if not_free_in("?e2", "?x");
            lift_compose_right: "(+ ?e1 (lib ?x ?v ?e2))" => "(lib ?x ?v (+ ?e1 ?e2))" if not_free_in("?e1", "?x");

            lift_app_both: "(app (lib ?x ?v ?e1) (lib ?x ?v ?e2))" => "(lib ?x ?v (app ?e1 ?e2))";
            lift_app_left: "(app (lib ?x ?v ?e1) ?e2)" => "(lib ?x ?v (app ?e1 ?e2))" if not_free_in("?e2", "?x");
            lift_app_right: "(app ?e1 (lib ?x ?v ?e2))" => "(lib ?x ?v (app ?e1 ?e2))" if not_free_in("?e1", "?x");

            // Binding expressions
            lift_let_both: "(let ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
            lift_let_body: "(let ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
            lift_let_binding: "(let ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");

            lift_lib_both: "(lib ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
            lift_lib_body: "(lib ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
            lift_lib_binding: "(lib ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");
        }
    }
}

/// Produces a `Condition` which is true if and only if the `Condition`s `p` and
/// `q` are both true. If `p` is false, this condition short-circuits and does
/// not check `q`.
fn and<L, A, P, Q>(p: P, q: Q) -> impl Condition<L, A>
where
    L: Language,
    A: Analysis<L>,
    P: Condition<L, A>,
    Q: Condition<L, A>,
{
    move |egraph: &mut egg::EGraph<L, A>, id: Id, subst: &Subst| {
        p.check(egraph, id, subst) && q.check(egraph, id, subst)
    }
}

/// Produces a `Condition` which is true if and only if the variable matched by
/// `var` is not potentially free in the expression matched by `expr`.
/// Both `expr` and `var` must be pattern variables (e.g. "?e" and "?x").
///
/// # Panics
/// Panics if `var` matches something other than a single symbol.
fn not_free_in(expr: &'static str, var: &'static str) -> impl Condition<Smiley, SmileyAnalysis> {
    fn get_var_sym<D>(eclass: &EClass<Smiley, D>) -> Option<Symbol> {
        if eclass.nodes.len() == 1 {
            if let Smiley::Symbol(var_sym) = eclass.nodes[0] {
                return Some(var_sym);
            }
        }
        None
    }

    let var_metavar = var.parse().unwrap();
    let expr_metavar = expr.parse().unwrap();
    move |egraph: &mut EGraph, _id: Id, subst: &Subst| {
        let var_eclass = &egraph[subst[var_metavar]];
        let var_sym = get_var_sym(var_eclass).expect("not a variable");
        let free_vars = &egraph[subst[expr_metavar]].data;
        !free_vars.contains(&var_sym)
    }
}

/// Execute `EGraph` building and program extraction on a single expression
/// containing all of the programs to extract common fragments out of.
pub fn run_single(runner: Runner<Smiley, SmileyAnalysis>) {
    crate::anti_unify::anti_unify(runner);
}
