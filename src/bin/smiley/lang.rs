//! The AST defining the smiley language.

use babble::{
    ast_node::{Arity, AstNode},
    lift_lib::LiftLib,
    teachable::{BindingExpr, DeBruijnIndex, Teachable},
};
use egg::Rewrite;
use lazy_static::lazy_static;
use ordered_float::NotNan;
use std::{
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
    /// A de Bruijn-indexed variable. These are represented with a dollar sign
    /// followed by the index, i.e. `$0`, `$123`.
    Var(DeBruijnIndex),
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
    /// Bind a value to `$0` within an expression.
    Lib,
    /// Shift indices.
    Shift,
}
impl Arity for Smiley {
    fn min_arity(&self) -> usize {
        match self {
            Self::Int(_) | Self::Float(_) | Self::Var(_) | Self::Circle | Self::Line => 0,
            Self::Lambda | Self::Shift | Self::Compose => 1,
            Self::Scale | Self::Rotate | Self::Apply | Self::Lib => 2,
            Self::Move => 3,
        }
    }

    fn max_arity(&self) -> Option<usize> {
        match self {
            Self::Compose => None,
            other => Some(other.min_arity()),
        }
    }
}

impl Display for Smiley {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(n) => n.fmt(f),
            Self::Float(g) => g.fmt(f),
            Self::Var(i) => write!(f, "{}", i),
            Self::Circle => f.write_str("circle"),
            Self::Line => f.write_str("line"),
            Self::Move => f.write_str("move"),
            Self::Scale => f.write_str("scale"),
            Self::Rotate => f.write_str("rotate"),
            Self::Compose => f.write_str("+"),
            Self::Apply => f.write_str("@"),
            Self::Lambda => f.write_str("λ"),
            Self::Lib => f.write_str("lib"),
            Self::Shift => f.write_str("shift"),
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
            "lib" => Self::Lib,
            "+" => Self::Compose,
            "shift" => Self::Shift,
            _ => {
                if let Ok(index) = s.parse() {
                    Self::Var(index)
                } else if let Ok(n) = s.parse() {
                    Self::Int(n)
                } else if let Ok(f) = s.parse() {
                    Self::Float(f)
                } else {
                    panic!("how to parse")
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
            BindingExpr::Var(index) => AstNode::leaf(Self::Var(DeBruijnIndex(index))),
            BindingExpr::Let(bound_value, body) => AstNode::new(Self::Lib, [bound_value, body]),
            BindingExpr::Shift(body) => AstNode::new(Self::Shift, [body]),
        }
    }

    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
        let binding_expr = match node.as_parts() {
            (Self::Lambda, [body]) => BindingExpr::Lambda(body),
            (Self::Apply, [fun, arg]) => BindingExpr::Apply(fun, arg),
            (&Self::Var(DeBruijnIndex(index)), []) => BindingExpr::Var(index),
            (Self::Lib, [bound_value, body]) => BindingExpr::Let(bound_value, body),
            (Self::Shift, [body]) => BindingExpr::Shift(body),
            _ => return None,
        };
        Some(binding_expr)
    }
}

lazy_static! {
    /// Rewrite rules which move containing expressions inside of
    /// [`Smiley::Lib`] expressions.
    pub(crate) static ref LIFT_LIB_REWRITES: &'static [Rewrite<AstNode<Smiley>, ()>] = vec![
        LiftLib::rewrite("lift_compose", Smiley::Compose),
        LiftLib::rewrite("lift_rotate", Smiley::Rotate),
        LiftLib::rewrite("lift_move", Smiley::Move),
        LiftLib::rewrite("lift_scale", Smiley::Scale),
        LiftLib::rewrite("lift_apply", Smiley::Apply),
    ]
    .leak();

        // TODO: Check for captures of de Bruijn variables and re-index if necessary.
        // lift_lambda: "(lambda (lib ?x ?v ?e))" => "(lib ?x ?v (lambda ?e))";

        // (Effectively) unary operators
        // lift_scale: "(scale ?a (lib ?x ?v ?e))" => "(lib ?x ?v (scale ?a ?e))";
        // lift_rotate: "(rotate ?a (lib ?x ?v ?e))" => "(lib ?x ?v (rotate ?a ?e))";
        // lift_move: "(move ?a ?b (lib ?x ?v ?e))" => "(lib ?x ?v (move ?a ?b ?e))";

        // // Binary operators
        // lift_compose_both: "(+ (lib ?x ?v ?e1) (lib ?x ?v ?e2))" => "(lib ?x ?v (+ ?e1 ?e2))";
        // lift_compose_left: "(+ (lib ?x ?v ?e1) ?e2)" => "(lib ?x ?v (+ ?e1 ?e2))" if not_free_in("?e2", "?x");
        // lift_compose_right: "(+ ?e1 (lib ?x ?v ?e2))" => "(lib ?x ?v (+ ?e1 ?e2))" if not_free_in("?e1", "?x");

        // lift_apply_both: "(apply (lib ?x ?v ?e1) (lib ?x ?v ?e2))" => "(lib ?x ?v (apply ?e1 ?e2))";
        // lift_apply_left: "(apply (lib ?x ?v ?e1) ?e2)" => "(lib ?x ?v (apply ?e1 ?e2))" if not_free_in("?e2", "?x");
        // lift_apply_right: "(apply ?e1 (lib ?x ?v ?e2))" => "(lib ?x ?v (apply ?e1 ?e2))" if not_free_in("?e1", "?x");

        // // Binding expressions
        // lift_let_both: "(let ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
        // lift_let_body: "(let ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
        // lift_let_binding: "(let ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");

        // lift_lib_both: "(lib ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
        // lift_lib_body: "(lib ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
        // lift_lib_binding: "(lib ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");
}
