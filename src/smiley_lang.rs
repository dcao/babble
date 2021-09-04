//! The AST defining the smiley language.

use crate::{
    anti_unify::{anti_unify, Antiunifiable},
    expr::{Arity, Expr},
};
use babble_macros::rewrite_rules;
use egg::{
    Analysis, AstSize, Condition, EClass, EGraph, Extractor, Id, Language, Rewrite, Runner, Subst,
    Symbol,
};
use lazy_static::lazy_static;
use ordered_float::NotNan;
use std::{
    cmp::Ordering,
    collections::HashSet,
    convert::TryInto,
    fmt::{self, Display, Formatter},
    iter::FromIterator,
    num::ParseIntError,
    str::FromStr,
};

/// The operations/AST nodes of the "Smiley" language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Smiley {
    /// A signed integer constant.
    Signed(i32),
    /// A floating-point constant.
    Float(NotNan<f64>),
    /// An identifier. This generally represents a named variable.
    Ident(Symbol),
    /// A de Bruijn-indexed variable. These are represented with a dollar sign
    /// followed by the index, i.e. `$0`, `$123`.
    Var(usize),
    /// A unit circle.
    Circle,
    /// A unit line.
    Line,
    /// Translate a picture.
    Move,
    /// Scale a picture.
    Scale,
    /// Rotate a picture.
    Rotate,
    /// Union two pictures.
    Compose,
    /// Apply a function to an argument.
    Apply,
    /// Create an anonymous, de Bruijn-indexed function.
    Lambda,
    /// Bind a value to a name within an expression.
    Let,
    /// Bind a learned library function to a name within an expression. This is
    /// functionally identical to [`Smiley::Let`], but indicates that the
    /// function was learned through anti-unification. This creates a helpful
    /// visual distinction and allows rewrite rules to selectively target
    /// learned functions.
    Lib,
}

impl Arity for Smiley {
    fn arity(&self) -> usize {
        match self {
            Self::Signed(_)
            | Self::Float(_)
            | Self::Ident(_)
            | Self::Var(_)
            | Self::Circle
            | Self::Line => 0,
            Self::Lambda => 1,
            Self::Scale | Self::Rotate | Self::Compose | Self::Apply => 2,
            Self::Move | Self::Let | Self::Lib => 3,
        }
    }
}

impl Display for Smiley {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Signed(n) => n.fmt(f),
            Self::Float(g) => g.fmt(f),
            Self::Ident(s) => s.fmt(f),
            Self::Var(i) => write!(f, "${}", i),
            Self::Circle => f.write_str("circle"),
            Self::Line => f.write_str("line"),
            Self::Move => f.write_str("move"),
            Self::Scale => f.write_str("scale"),
            Self::Rotate => f.write_str("rotate"),
            Self::Compose => f.write_str("+"),
            Self::Apply => f.write_str("apply"),
            Self::Lambda => f.write_str("lambda"),
            Self::Let => f.write_str("let"),
            Self::Lib => f.write_str("lib"),
        }
    }
}

impl FromStr for Smiley {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let kind = match s {
            "circle" => Self::Circle,
            "line" => Self::Line,
            "lambda" => Self::Lambda,
            "scale" => Self::Scale,
            "move" => Self::Move,
            "rotate" => Self::Rotate,
            "apply" => Self::Apply,
            "let" => Self::Let,
            "lib" => Self::Lib,
            "+" => Self::Compose,
            _ => {
                if let Some(i) = s.strip_prefix('$') {
                    Self::Var(i.parse()?)
                } else if let Ok(n) = s.parse() {
                    Self::Signed(n)
                } else if let Ok(f) = s.parse() {
                    Self::Float(f)
                } else {
                    Self::Ident(s.into())
                }
            }
        };
        Ok(kind)
    }
}

impl Antiunifiable for Smiley {
    fn lambda() -> Self {
        Self::Lambda
    }

    fn apply() -> Self {
        Self::Apply
    }

    fn arg(index: usize) -> Self {
        Self::Var(index)
    }

    fn ident(name: Symbol) -> Self {
        Self::Ident(name)
    }

    fn lib() -> Self {
        Self::Lib
    }
}

/// Analysis which maintains a set of potentially free variables for each
/// e-class. For example, the set of potentially free variables for an e-class
/// representing the expressions `(app f circle)`, `(scale 1 x)`, and `line`
/// will be `{f, x}`.
#[derive(Default, Clone, Copy, Debug)]
pub struct SmileyAnalysis;

impl Analysis<Expr<Smiley>> for SmileyAnalysis {
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
    fn make(egraph: &EGraph<Expr<Smiley>, Self>, enode: &Expr<Smiley>) -> Self::Data {
        match enode.kind() {
            Smiley::Ident(var) => HashSet::from_iter([*var]),
            Smiley::Let | Smiley::Lib => {
                let [var, a, b]: [Id; 3] = enode.children().try_into().unwrap();
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
    move |egraph: &mut EGraph<L, A>, id: Id, subst: &Subst| {
        p.check(egraph, id, subst) && q.check(egraph, id, subst)
    }
}

/// Produces a [`Condition`] which is true if and only if the variable matched by
/// `var` is not potentially free in the expression matched by `expr`.
/// Both `expr` and `var` must be pattern variables (e.g. "?e" and "?x").
///
/// # Panics
/// Panics if `var` matches something other than a single symbol.
fn not_free_in(
    expr: &'static str,
    var: &'static str,
) -> impl Condition<Expr<Smiley>, SmileyAnalysis> {
    fn get_var_sym<D>(eclass: &EClass<Expr<Smiley>, D>) -> Option<Symbol> {
        if eclass.nodes.len() == 1 {
            if let Smiley::Ident(var_sym) = eclass.nodes[0].kind() {
                return Some(*var_sym);
            }
        }
        None
    }

    let var_metavar = var.parse().unwrap();
    let expr_metavar = expr.parse().unwrap();
    move |egraph: &mut EGraph<_, SmileyAnalysis>, _, subst: &Subst| {
        let var_eclass = &egraph[subst[var_metavar]];
        let var_sym = get_var_sym(var_eclass).expect("not a variable");
        let free_vars = &egraph[subst[expr_metavar]].data;
        !free_vars.contains(&var_sym)
    }
}

lazy_static! {
    /// Rewrite rules which move containing expressions inside of [`Smiley::Lib`] expressions.
    static ref LIFT_LIB_REWRITES: &'static [Rewrite<Expr<Smiley>, SmileyAnalysis>] = rewrite_rules! {
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

        lift_apply_both: "(apply (lib ?x ?v ?e1) (lib ?x ?v ?e2))" => "(lib ?x ?v (apply ?e1 ?e2))";
        lift_apply_left: "(apply (lib ?x ?v ?e1) ?e2)" => "(lib ?x ?v (apply ?e1 ?e2))" if not_free_in("?e2", "?x");
        lift_apply_right: "(apply ?e1 (lib ?x ?v ?e2))" => "(lib ?x ?v (apply ?e1 ?e2))" if not_free_in("?e1", "?x");

        // Binding expressions
        lift_let_both: "(let ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
        lift_let_body: "(let ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
        lift_let_binding: "(let ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");

        lift_lib_both: "(lib ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
        lift_lib_body: "(lib ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
        lift_lib_binding: "(lib ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");
    }.leak();
}

/// Execute `EGraph` building and program extraction on a single expression
/// containing all of the programs to extract common fragments out of.
pub fn run_single(runner: Runner<Expr<Smiley>, SmileyAnalysis>) {
    // let e1 = runner.egraph.lookup_expr(&"(scale 2 (move 5 7 (rotate 90 line)))".parse().unwrap()).unwrap();
    // let e2 = runner.egraph.lookup_expr(&"(scale 2 (move 5 7 (rotate 90 circle)))".parse().unwrap()).unwrap();
    // let e3 = runner.egraph.lookup_expr(&"(scale 2 (move 5 7 (rotate 90 (scale 3 line))))".parse().unwrap()).unwrap();

    let au_rewrites = anti_unify(&runner.egraph);

    let mut runner = runner.with_iter_limit(1).run(au_rewrites.iter());

    // runner.egraph.check_goals(e1, &["(lib ?f (lambda (scale 2 (move 5 7 (rotate 90 $0)))) (apply ?f line))".parse().unwrap()]);
    // runner.egraph.check_goals(e2, &["(lib ?f (lambda (scale 2 (move 5 7 (rotate 90 $0)))) (apply ?f circle))".parse().unwrap()]);
    // runner.egraph.check_goals(e3, &["(lib ?f (lambda (scale 2 (move 5 7 (rotate 90 $0)))) (apply ?f (scale 3 line)))".parse().unwrap()]);

    runner.stop_reason = None;
    // eprintln!("{:?}", DebugEGraph::new(&runner.egraph));

    let runner = runner
        .with_iter_limit(30)
        .with_time_limit(core::time::Duration::from_secs(40))
        .run(LIFT_LIB_REWRITES.iter());

    let extractor = Extractor::new(&runner.egraph, AstSize);
    let (cost, expr) = extractor.find_best(runner.roots[0]);

    eprintln!("Cost: {}\n", cost);
    eprintln!("{}", expr.pretty(100));
}
