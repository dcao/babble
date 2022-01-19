//! The language of Dream&shy;Coder expressions.

use std::{
    borrow::Cow,
    convert::{TryFrom, TryInto},
    fmt::{self, Display, Formatter},
    slice,
    str::FromStr,
};

use babble::{
    ast_node::{Arity, AstNode, Expr},
    teachable::{BindingExpr, Teachable},
};
use egg::{FromOp, FromOpError, Id, Language, RecExpr, Symbol};
use internment::ArcIntern;
use nom::error::convert_error;
use serde::{Deserialize, Serialize};

use super::{
    parse,
    util::{parens, DeBruijnIndex},
};

#[allow(single_use_lifetimes)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
struct RawExpr<'a>(Cow<'a, str>);

/// An expression in Dream&shy;Coder's generic programming language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "RawExpr<'_>")]
#[serde(into = "RawExpr<'_>")]
pub struct DcExpr(Expr<DreamCoderOp>);

impl From<Expr<DreamCoderOp>> for DcExpr {
    fn from(expr: Expr<DreamCoderOp>) -> Self {
        Self(expr)
    }
}

impl From<DcExpr> for Expr<DreamCoderOp> {
    fn from(expr: DcExpr) -> Self {
        expr.0
    }
}

impl From<DcExpr> for RecExpr<AstNode<DreamCoderOp>> {
    fn from(expr: DcExpr) -> Self {
        expr.0.into()
    }
}

impl From<DcExpr> for RawExpr<'_> {
    fn from(expr: DcExpr) -> Self {
        Self(expr.to_string().into())
    }
}

impl<'a> TryFrom<RawExpr<'a>> for DcExpr {
    type Error = ParseExprError;

    fn try_from(raw_expr: RawExpr<'a>) -> Result<Self, Self::Error> {
        raw_expr.0.parse()
    }
}

/// An AST node in the DreamCoder language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DreamCoderOp {
    /// A variable.
    Var(usize),

    /// A symbol, typically one of the language's primitives.
    Symbol(Symbol),

    /// An "inlined" expression. This is how Dream&shy;Coder represents learned
    /// functions.
    Inlined(ArcIntern<DcExpr>),

    /// An anonymous function.
    Lambda,

    /// An application of a function to a variable. Dream&shy;Coder allows
    /// applying a function to multiple arguments, we translate these to nested
    /// applications. That is, `(foo bar baz quux)`, is interpreted as
    /// `(((foo bar) baz) quux)`.
    App,

    Lib,
    Shift,
}

impl Arity for DreamCoderOp {
    fn min_arity(&self) -> usize {
        match self {
            DreamCoderOp::Var(_) | DreamCoderOp::Symbol(_) | DreamCoderOp::Inlined(_) => 0,
            DreamCoderOp::Lambda | DreamCoderOp::Shift => 1,
            DreamCoderOp::App | DreamCoderOp::Lib => 2,
        }
    }
}

impl Teachable for DreamCoderOp {
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T> {
        match binding_expr {
            BindingExpr::Var(index) => AstNode::leaf(DreamCoderOp::Var(index)),
            BindingExpr::Lambda(body) => AstNode::new(DreamCoderOp::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => AstNode::new(DreamCoderOp::App, [fun, arg]),
            BindingExpr::Let(def, body) => AstNode::new(DreamCoderOp::Lib, [def, body]),
            BindingExpr::Shift(expr) => AstNode::new(DreamCoderOp::Shift, [expr]),
        }
    }

    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
        let binding_expr = match node.as_parts() {
            (DreamCoderOp::Var(index), []) => BindingExpr::Var(*index),
            (DreamCoderOp::Lambda, [body]) => BindingExpr::Lambda(body),
            (DreamCoderOp::App, [fun, arg]) => BindingExpr::Apply(fun, arg),
            (DreamCoderOp::Lib, [def, body]) => BindingExpr::Let(def, body),
            (DreamCoderOp::Shift, [expr]) => BindingExpr::Shift(expr),
            _ => return None,
        };
        Some(binding_expr)
    }
}

impl Display for DreamCoderOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            DreamCoderOp::Lambda => "lambda",
            DreamCoderOp::App => "@",
            DreamCoderOp::Lib => "lib",
            DreamCoderOp::Shift => "shift",
            DreamCoderOp::Var(index) => return write!(f, "${}", index),
            DreamCoderOp::Inlined(expr) => return write!(f, "#{}", expr),
            DreamCoderOp::Symbol(symbol) => return write!(f, "{}", symbol),
        };
        f.write_str(s)
    }
}

impl FromStr for DreamCoderOp {
    type Err = ParseExprError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "lambda" => Ok(DreamCoderOp::Lambda),
            "@" => Ok(DreamCoderOp::App),
            "lib" => Ok(DreamCoderOp::Lib),
            "shift" => Ok(DreamCoderOp::Shift),
            _ => {
                return s
                    .parse()
                    .map(|DeBruijnIndex(index)| DreamCoderOp::Var(index))
                    .or_else(|_| match s.strip_prefix('#') {
                        Some(s) => RawExpr(s.into())
                            .try_into()
                            .map(|expr| DreamCoderOp::Inlined(ArcIntern::new(expr))),
                        None => Ok(DreamCoderOp::Symbol(s.into())),
                    })
            }
        }
    }
}

impl Display for DcExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expr = &self.0.into_inner();
        match expr.as_parts() {
            (
                op @ (DreamCoderOp::Var(_) | DreamCoderOp::Inlined(_) | DreamCoderOp::Symbol(_)),
                [],
            ) => write!(f, "{}", op),
            (DreamCoderOp::Lambda, [body]) => {
                write!(f, "(lambda {:.1})", &Self(*body))
            }
            (DreamCoderOp::App, [fun, arg]) => {
                parens(0, f, |f| write!(f, "{:.0} {:.1}", &Self(*fun), &Self(*arg)))
            }
            (op, args) => {
                write!(f, "({}", op)?;
                for arg in args {
                    write!(f, " {}", &Self(*arg))?;
                }
                f.write_str(")")
            }
        }
    }
}

/// An error produced when a string can't be parsed as a valid [`Expr`].
#[derive(Debug, Clone)]
pub struct ParseExprError {
    message: String,
}

impl Display for ParseExprError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl FromStr for DcExpr {
    type Err = ParseExprError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse::parse(s).map_err(|e| ParseExprError {
            message: convert_error(s, e),
        })
    }
}

impl DcExpr {
    fn lambda(body: Self) -> Self {
        Self(AstNode::new(DreamCoderOp::Lambda, [body.0]).into())
    }

    fn app(fun: Self, arg: Self) -> Self {
        Self(AstNode::new(DreamCoderOp::App, [fun.0, arg.0]).into())
    }

    fn symbol(name: &str) -> Self {
        Self(AstNode::leaf(DreamCoderOp::Symbol(name.into())).into())
    }

    fn var(index: usize) -> Self {
        Self(AstNode::leaf(DreamCoderOp::Var(index)).into())
    }

    fn inlined(expr: Self) -> Self {
        Self(AstNode::leaf(DreamCoderOp::Inlined(ArcIntern::new(expr))).into())
    }
}

#[cfg(test)]
mod tests {
    use super::DcExpr;

    #[test]
    fn parser_test() {
        let input = "(lambda (map #(lambda (+ $0 1)) $0))";
        let expr = DcExpr::lambda(DcExpr::app(
            DcExpr::app(
                DcExpr::symbol("map"),
                DcExpr::inlined(DcExpr::lambda(DcExpr::app(
                    DcExpr::app(DcExpr::symbol("+"), DcExpr::var(0)),
                    DcExpr::symbol("1"),
                ))),
            ),
            DcExpr::var(0),
        ));

        let parsed: DcExpr = input.parse().unwrap();

        assert_eq!(parsed, expr);
        assert_eq!(expr.to_string(), input);
    }
}
