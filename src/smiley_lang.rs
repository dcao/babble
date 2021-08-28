//! The AST defining the smiley language.

// TODO: Remove this once define_language! allows doc strings
#![allow(missing_docs)]

use crate::{
    anti_unify::{anti_unify, AntiUnifTgt},
    fresh,
};
use babble_macros::rewrite_rules;
use egg::{
    Analysis, AstSize, Condition, EClass, Extractor, FromOp, FromOpError, Id, Language, Runner,
    Subst, Symbol,
};
use lazy_static::lazy_static;
use ordered_float::NotNan;
use std::{
    cmp::Ordering,
    collections::HashSet,
    convert::TryInto,
    fmt::{self, Display, Formatter},
    iter::FromIterator,
};

/// E-graphs in the `Smiley` language.
pub type EGraph = egg::EGraph<Smiley, SmileyAnalysis>;

/// Rewrites in the `Smiley` language.
pub type Rewrite = egg::Rewrite<Smiley, SmileyAnalysis>;

/// Number constants in the `Smiley` language.
pub type Constant = NotNan<f64>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SmileyKind {
    // Base cases
    Signed(i32),
    Float(Constant),
    Ident(Symbol),
    Var(usize),
    Circle,
    Line,

    Move,
    Scale,
    Rotate,
    Compose,

    Apply,
    Lambda,
    Let,
    Lib,
}

impl SmileyKind {
    #[must_use]
    pub fn arity(&self) -> usize {
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

impl Display for SmileyKind {
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SmileyExpr<T> {
    kind: SmileyKind,
    children: Vec<T>,
}

impl Language for SmileyExpr<Id> {
    fn matches(&self, other: &Self) -> bool {
        self.kind == other.kind
    }

    fn children(&self) -> &[Id] {
        &self.children
    }

    fn children_mut(&mut self) -> &mut [Id] {
        &mut self.children
    }
}

impl FromOp for SmileyExpr<Id> {
    type Error = FromOpError;

    fn from_op(op: &str, children: Vec<Id>) -> Result<Self, Self::Error> {
        let kind = match op {
            "circle" => SmileyKind::Circle,
            "line" => SmileyKind::Line,
            "lambda" => SmileyKind::Lambda,
            "scale" => SmileyKind::Scale,
            "move" => SmileyKind::Move,
            "rotate" => SmileyKind::Rotate,
            "apply" => SmileyKind::Apply,
            "let" => SmileyKind::Let,
            "lib" => SmileyKind::Lib,
            "+" => SmileyKind::Compose,
            _ => {
                if let Some(i) = op.strip_prefix('$') {
                    let i = i
                        .parse()
                        .map_err(|_| FromOpError::new(op, children.clone()))?;
                    SmileyKind::Var(i)
                } else if let Ok(n) = op.parse() {
                    SmileyKind::Signed(n)
                } else if let Ok(f) = op.parse() {
                    SmileyKind::Float(f)
                } else {
                    SmileyKind::Ident(op.into())
                }
            }
        };
        if kind.arity() == children.len() {
            Ok(Self { kind, children })
        } else {
            Err(FromOpError::new(op, children))
        }
    }
}

impl<T> Display for SmileyExpr<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

type Smiley = SmileyExpr<Id>;

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
        match enode.kind {
            SmileyKind::Ident(var) => HashSet::from_iter([var]),
            SmileyKind::Let | SmileyKind::Lib => {
                let [var, a, b]: [Id; 3] = enode.children.clone().try_into().unwrap();
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

impl AntiUnifTgt for Smiley {
    type Kind = SmileyKind;

    fn kind(&self) -> Self::Kind {
        self.kind
    }

    fn from_parts<I>(kind: Self::Kind, children: I) -> Self
    where I: IntoIterator<Item = Id> {
        Self {
            kind,
            children: children.into_iter().collect(),
        }
    }

    fn lambda(body: Id) -> Self {
        Self {
            kind: SmileyKind::Lambda,
            children: vec![body],
        }
    }

    fn app(lambda: Id, arg: Id) -> Self {
        Self {
            kind: SmileyKind::Apply,
            children: vec![lambda, arg],
        }
    }

    fn lambda_arg(i: usize) -> Self {
        Self {
            kind: SmileyKind::Var(i),
            children: vec![],
        }
    }

    fn fn_sym() -> Self {
        Self {
            kind: SmileyKind::Ident(fresh::gen("f")),
            children: vec![],
        }
    }

    fn lib(name: Id, lambda: Id, body: Id) -> Self {
        Self {
            kind: SmileyKind::Lib,
            children: vec![name, lambda, body],
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
            if let SmileyKind::Ident(var_sym) = eclass.nodes[0].kind {
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

lazy_static! {
    static ref LIFT_LIB_REWRITES: &'static [Rewrite] = rewrite_rules! {
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
pub fn run_single(runner: Runner<Smiley, SmileyAnalysis>) {
    let au_rewrites = anti_unify(&runner.egraph);

    let runner = runner
        .with_time_limit(core::time::Duration::from_secs(40))
        .run(au_rewrites.iter().chain(LIFT_LIB_REWRITES.iter()));

    let extractor = Extractor::new(&runner.egraph, AstSize);
    let (cost, expr) = extractor.find_best(runner.roots[0]);

    eprintln!("Cost: {}\n", cost);
    eprintln!("{}", expr.pretty(100));
}
