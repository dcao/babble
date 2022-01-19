use std::{
    convert::Infallible,
    fmt::{self, Display, Formatter},
    str::FromStr,
};

use egg::Symbol;

use crate::{
    ast_node::{Arity, AstNode},
    teachable::{BindingExpr, DeBruijnIndex, Teachable},
};

/// Simplest language to use with babble
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SimpleOp {
    /// A function application
    Apply,
    /// A de Bruijn-indexed variable
    Var(usize),
    /// An uninterpreted symbol
    Symbol(Symbol),
    /// An anonymous function
    Lambda,
    /// A library function binding
    Lib,
    /// A shift
    Shift,
}

impl Arity for SimpleOp {
    fn min_arity(&self) -> usize {
        match self {
            Self::Var(_) | Self::Symbol(_) => 0,
            Self::Lambda | Self::Shift => 1,
            Self::Apply | Self::Lib => 2,
        }
    }
}

impl Display for SimpleOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Apply => "@",
            Self::Lambda => "λ",
            Self::Shift => "shift",
            Self::Lib => "lib",
            Self::Var(index) => {
                return write!(f, "${}", index);
            }
            Self::Symbol(sym) => {
                return write!(f, "{}", sym);
            }
        };
        f.write_str(s)
    }
}

impl FromStr for SimpleOp {
    type Err = Infallible;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let op = match input {
            "shift" => Self::Shift,
            "apply" | "@" => Self::Apply,
            "lambda" | "λ" => Self::Lambda,
            "lib" => Self::Lib,
            input => input
                .parse()
                .map(|DeBruijnIndex(index)| Self::Var(index))
                .unwrap_or_else(|_| Self::Symbol(input.into())),
        };
        Ok(op)
    }
}

impl Teachable for SimpleOp {
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T> {
        match binding_expr {
            BindingExpr::Lambda(body) => AstNode::new(Self::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => AstNode::new(Self::Apply, [fun, arg]),
            BindingExpr::Var(index) => AstNode::leaf(Self::Var(index)),
            BindingExpr::Let(bound_value, body) => AstNode::new(Self::Lib, [bound_value, body]),
            BindingExpr::Shift(body) => AstNode::new(Self::Shift, [body]),
        }
    }

    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
        let binding_expr = match node.as_parts() {
            (Self::Lambda, [body]) => BindingExpr::Lambda(body),
            (Self::Apply, [fun, arg]) => BindingExpr::Apply(fun, arg),
            (Self::Var(index), []) => BindingExpr::Var(*index),
            (Self::Lib, [bound_value, body]) => BindingExpr::Let(bound_value, body),
            (Self::Shift, [body]) => BindingExpr::Shift(body),
            _ => return None,
        };
        Some(binding_expr)
    }
}
