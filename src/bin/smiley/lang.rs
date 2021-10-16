//! The AST defining the smiley language.

use babble::{
    ast_node::{Arity, AstNode},
    free_vars::{and, not_free_in, FreeVarAnalysis, FreeVars},
    learn::LearnedLibrary,
    teachable::{BindingExpr, DeBruijnIndex, Teachable},
};
use babble_macros::rewrite_rules;
use egg::{AstSize, Extractor, Rewrite, Runner, Symbol};
use lazy_static::lazy_static;
use ordered_float::NotNan;
use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
    num::ParseIntError,
    str::FromStr,
};

/// The operations/AST nodes of the "Smiley" language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Smiley {
    /// A signed integer constant.
    Int(i32),
    /// A floating-point constant.
    Float(NotNan<f64>),
    /// An identifier. This generally represents a named variable.
    Ident(Symbol),
    /// A de Bruijn-indexed variable. These are represented with a dollar sign
    /// followed by the index, i.e. `$0`, `$123`.
    Index(DeBruijnIndex),
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
    fn min_arity(&self) -> usize {
        match self {
            Self::Int(_)
            | Self::Float(_)
            | Self::Ident(_)
            | Self::Index(_)
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
            Self::Int(n) => n.fmt(f),
            Self::Float(g) => g.fmt(f),
            Self::Ident(s) => s.fmt(f),
            Self::Index(i) => write!(f, "{}", i),
            Self::Circle => f.write_str("circle"),
            Self::Line => f.write_str("line"),
            Self::Move => f.write_str("move"),
            Self::Scale => f.write_str("scale"),
            Self::Rotate => f.write_str("rotate"),
            Self::Compose => f.write_str("+"),
            Self::Apply => f.write_str("@"),
            Self::Lambda => f.write_str("λ"),
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
            "lambda" | "λ" => Self::Lambda,
            "scale" => Self::Scale,
            "move" => Self::Move,
            "rotate" => Self::Rotate,
            "apply" | "@" => Self::Apply,
            "let" => Self::Let,
            "lib" => Self::Lib,
            "+" => Self::Compose,
            _ => {
                if let Ok(index) = s.parse() {
                    Self::Index(index)
                } else if let Ok(n) = s.parse() {
                    Self::Int(n)
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

impl Teachable for Smiley {
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T> {
        match binding_expr {
            BindingExpr::Lambda(body) => AstNode::new(Self::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => AstNode::new(Self::Apply, [fun, arg]),
            BindingExpr::Index(index) => AstNode::leaf(Self::Index(DeBruijnIndex(index))),
            BindingExpr::Ident(ident) => AstNode::leaf(Self::Ident(ident)),
            BindingExpr::Lib { ident, value, body } => {
                AstNode::new(Self::Lib, [ident, value, body])
            }
        }
    }

    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
        let binding_expr = match node.as_parts() {
            (Self::Lambda, [body]) => BindingExpr::Lambda(body),
            (Self::Apply, [fun, arg]) => BindingExpr::Apply(fun, arg),
            (&Self::Index(DeBruijnIndex(index)), []) => BindingExpr::Index(index),
            (&Self::Ident(ident), []) => BindingExpr::Ident(ident),
            (Self::Lib, [ident, value, body]) => BindingExpr::Lib { ident, value, body },
            _ => return None,
        };
        Some(binding_expr)
    }
}

impl FreeVars for Smiley {
    fn get_ident(&self) -> Option<Symbol> {
        match *self {
            Self::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    fn free_vars(&self, children: &[&HashSet<Symbol>]) -> HashSet<Symbol> {
        match (self, children) {
            (&Self::Ident(ident), []) => {
                let mut result = HashSet::new();
                result.insert(ident);
                result
            }
            (Self::Let | Self::Lib, &[ident, val, body]) => &(body - ident) | val,
            (_, children) => children
                .iter()
                .flat_map(|child| child.iter().copied())
                .collect(),
        }
    }
}

lazy_static! {
    /// Rewrite rules which move containing expressions inside of
    /// [`Smiley::Lib`] expressions.
    static ref LIFT_LIB_REWRITES: &'static [Rewrite<AstNode<Smiley>, FreeVarAnalysis<Smiley>>] = rewrite_rules! {
        // TODO: Check for captures of de Bruijn variables and re-index if necessary.
        // lift_lambda: "(lambda (lib ?x ?v ?e))" => "(lib ?x ?v (lambda ?e))";

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
pub(crate) fn run_single(runner: Runner<AstNode<Smiley>, FreeVarAnalysis<Smiley>>) {
    // let e1 = runner.egraph.lookup_expr(&"(scale 2 (move 5 7 (rotate 90 line)))".parse().unwrap()).unwrap();
    // let e2 = runner.egraph.lookup_expr(&"(scale 2 (move 5 7 (rotate 90 circle)))".parse().unwrap()).unwrap();
    // let e3 = runner.egraph.lookup_expr(&"(scale 2 (move 5 7 (rotate 90 (scale 3 line))))".parse().unwrap()).unwrap();

    let learned_lib = LearnedLibrary::from(&runner.egraph);
    let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();

    let mut runner = runner.with_iter_limit(1).run(lib_rewrites.iter());

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

    eprintln!("Final expression (cost {}):", cost);
    eprintln!("{}", expr.pretty(100));
}
