//! The AST defining a 3D CAD language.

use babble::{
    ast_node::{Arity, AstNode},
    learn::{LibId, ParseLibIdError},
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
pub(crate) enum CAD {
    Int(i32),
    Cube,
    Sphere,
    Translate,
    Scale,
    Rotate,
    Union,
    Diff,
    Lambda,
    Lib(LibId),
    LibVar(LibId),
    Apply,
    Var(DeBruijnIndex),
    Shift,
}

impl Arity for CAD {
    fn min_arity(&self) -> usize {
        match self {
            CAD::Int(_) => 0,
            CAD::Cube => 0,
            CAD::Sphere => 0,
            CAD::Var(_) => 0,
            CAD::Translate => 3,
            CAD::Scale => 3,
            CAD::Rotate => 3,
            CAD::Union => 2,
            CAD::Diff => 2,
            CAD::Lambda => 1,
            CAD::Lib(_) => 2,
            CAD::LibVar(_) => 0,
            CAD::Apply => 2,
            CAD::Shift => 1,
        }
    }
}

impl Display for CAD {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CAD::Int(n) => n.fmt(f),
            CAD::Cube => f.write_str("cube"),
            CAD::Sphere => f.write_str("sphere"),
            CAD::Translate => f.write_str("translate"),
            CAD::Scale => f.write_str("scale"),
            CAD::Rotate => f.write_str("rotate"),
            CAD::Union => f.write_str("union"),
            CAD::Diff => f.write_str("diff"),
            CAD::Lambda => f.write_str("λ"),
            CAD::Lib(ix) => write!(f, "lib {}", ix),
            CAD::LibVar(ix) => write!(f, "{}", ix),
            CAD::Apply => f.write_str("@"),
            CAD::Var(i) => write!(f, "{}", i),
            CAD::Shift => f.write_str("shift"),
        }
    }
}

impl FromStr for CAD {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let cad = match s {
            "cube" => Self::Cube,
            "sphere" => Self::Sphere,
            "translate" => Self::Translate,
            "rotate" => Self::Rotate,
            "scale" => Self::Scale,
            "union" => Self::Union,
            "diff" => Self::Diff,
            "apply" | "@" => Self::Apply,
            "shift" => Self::Shift,
            "lambda" | "λ" => Self::Lambda,
            _ => {
                if let Ok(index) = s.parse::<DeBruijnIndex>() {
                    Self::Var(index)
                } else if let Ok(lv) = s.parse::<LibId>() {
                    Self::LibVar(lv)
                } else if let Ok(lv) = s
                    .strip_prefix("lib ")
                    .ok_or(ParseLibIdError::NoLeadingL)
                    .and_then(|x| x.parse())
                {
                    Self::Lib(lv)
                } else if let Ok(n) = s.parse::<i32>() {
                    Self::Int(n)
                } else {
                    panic!("Cannot parse {}", s)
                }
            }
        };
        Ok(cad)
    }
}

impl Teachable for CAD {
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T> {
        match binding_expr {
            BindingExpr::Lambda(body) => AstNode::new(Self::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => AstNode::new(Self::Apply, [fun, arg]),
            BindingExpr::Var(index) => AstNode::leaf(Self::Var(DeBruijnIndex(index))),
            BindingExpr::LibVar(ix) => AstNode::leaf(Self::LibVar(ix)),
            BindingExpr::Let(ix, bound_value, body) => {
                AstNode::new(Self::Lib(ix), [bound_value, body])
            }
            BindingExpr::Shift(body) => AstNode::new(Self::Shift, [body]),
        }
    }

    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
        let binding_expr = match node.as_parts() {
            (Self::Lambda, [body]) => BindingExpr::Lambda(body),
            (Self::Apply, [fun, arg]) => BindingExpr::Apply(fun, arg),
            (&Self::Var(DeBruijnIndex(index)), []) => BindingExpr::Var(index),
            (Self::Lib(ix), [bound_value, body]) => BindingExpr::Let(*ix, bound_value, body),
            (Self::Shift, [body]) => BindingExpr::Shift(body),
            _ => return None,
        };
        Some(binding_expr)
    }
}

lazy_static! {
    pub(crate) static ref LIFT_LIB_REWRITES: &'static [Rewrite<AstNode<CAD>, ()>] = vec![ ]
    .leak();
}