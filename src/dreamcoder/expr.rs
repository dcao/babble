//! The language of Dream&shy;Coder expressions.

use super::{parse, util::parens};
use crate::{
    ast_node::{Arity, AstNode, Expr, Precedence, Printable, Printer},
    learn::{LibId, ParseLibIdError},
    teachable::{BindingExpr, DeBruijnIndex, Teachable},
};
use egg::{Analysis, RecExpr, Rewrite, Symbol};
use internment::ArcIntern;
use nom::error::convert_error;
use ref_cast::RefCast;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    convert::{Infallible, TryFrom},
    fmt::{self, Display, Formatter, Write},
    ops::{Deref, DerefMut},
    str::FromStr,
};

/// A wrapper around a string, used as an intermediary for serializing other
/// types as strings.
#[allow(single_use_lifetimes)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
struct RawStr<'a>(Cow<'a, str>);

/// An expression in Dream&shy;Coder's generic programming language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, RefCast)]
#[serde(try_from = "RawStr<'_>")]
#[serde(into = "RawStr<'_>")]
#[repr(transparent)]
pub struct DcExpr(Expr<DreamCoderOp>);

impl Deref for DcExpr {
    type Target = Expr<DreamCoderOp>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for DcExpr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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

impl From<Expr<DreamCoderOp>> for DcExpr {
    fn from(expr: Expr<DreamCoderOp>) -> Self {
        Self(expr)
    }
}

impl From<DcExpr> for RawStr<'static> {
    fn from(expr: DcExpr) -> Self {
        Self(expr.to_string().into())
    }
}

impl<'a> TryFrom<RawStr<'a>> for DcExpr {
    type Error = ParseExprError;

    fn try_from(raw_expr: RawStr<'a>) -> Result<Self, Self::Error> {
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
    Inlined(ArcIntern<Expr<Self>>),

    /// An anonymous function.
    Lambda,

    /// An application of a function to a variable. Dream&shy;Coder allows
    /// applying a function to multiple arguments, we translate these to nested
    /// applications. That is, `(foo bar baz quux)`, is interpreted as
    /// `(((foo bar) baz) quux)`.
    App,

    Lib(LibId),
    LibVar(LibId),

    Shift,

    /// A utility operation that allows us to do extraction taking into account
    /// all programs.
    Combine,
}

impl DreamCoderOp {
    pub fn to_rewrite<A>(self) -> Option<Rewrite<AstNode<Self>, A>>
    where
        A: Analysis<AstNode<Self>>,
    {
        let body = match self {
            Self::Inlined(expr) => expr,
            _ => return None,
        };

        // We don't know the arity of the lib function DC learned. We know it's
        // at most the number of leading lambdas, so we could assume the arity
        // is the max possible? The json file also has an "arity" field, but I
        // think that's the max arity, not the exact arity. If so, we know the
        // arity is at most min(nm_leading_lambdas, json_arity_field).
        let arity = todo!();

        // Now we have to do the reverse process of `reify` in `babble::learn`:
        // 1. Remove `arity` leading lambdas
        // 2. Convert all free de Bruijn indices into pattern variables

        // Finally, convert that pattern back into a rewrite. This will end up
        // reifying the pattern again, which should return the original
        // expression. Not sure if that's necessarily true though, because we
        // add extra arguments for every variable in scope?
        let rewrite = todo!();

        Some(rewrite)
    }
}

impl Arity for DreamCoderOp {
    fn min_arity(&self) -> usize {
        match self {
            DreamCoderOp::Var(_)
            | DreamCoderOp::Symbol(_)
            | DreamCoderOp::Inlined(_)
            | DreamCoderOp::LibVar(_) => 0,
            DreamCoderOp::Lambda | DreamCoderOp::Shift | DreamCoderOp::Combine => 1,
            DreamCoderOp::App | DreamCoderOp::Lib(_) => 2,
        }
    }

    fn max_arity(&self) -> Option<usize> {
        match self {
            DreamCoderOp::Combine => None,
            x => Some(x.min_arity()),
        }
    }
}

impl FromStr for DreamCoderOp {
    type Err = Infallible;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let op = match input {
            "shift" => Self::Shift,
            "apply" | "@" => Self::App,
            "lambda" | "λ" => Self::Lambda,
            input => input
                .parse()
                .map(Self::Var)
                .or_else(|_| input.parse().map(Self::Var))
                .or_else(|_| input.parse().map(Self::LibVar))
                .or_else(|_| {
                    input
                        .strip_prefix("lib ")
                        .ok_or(ParseLibIdError::NoLeadingL)
                        .and_then(|x| x.parse().map(Self::Lib))
                })
                .unwrap_or_else(|_| Self::Symbol(input.into())),
        };

        Ok(op)
    }
}

impl Teachable for DreamCoderOp {
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T> {
        match binding_expr {
            BindingExpr::Var(DeBruijnIndex(index)) => AstNode::leaf(DreamCoderOp::Var(index)),
            BindingExpr::Lambda(body) => AstNode::new(DreamCoderOp::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => AstNode::new(DreamCoderOp::App, [fun, arg]),
            BindingExpr::Lib(ix, def, body) => AstNode::new(DreamCoderOp::Lib(ix), [def, body]),
            BindingExpr::LibVar(ix) => AstNode::leaf(DreamCoderOp::LibVar(ix)),
            BindingExpr::Shift(expr) => AstNode::new(DreamCoderOp::Shift, [expr]),
        }
    }

    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
        let binding_expr = match node.as_parts() {
            (DreamCoderOp::Var(index), []) => BindingExpr::Var(DeBruijnIndex(*index)),
            (DreamCoderOp::Lambda, [body]) => BindingExpr::Lambda(body),
            (DreamCoderOp::App, [fun, arg]) => BindingExpr::Apply(fun, arg),
            (DreamCoderOp::Lib(ix), [def, body]) => BindingExpr::Lib(*ix, def, body),
            (DreamCoderOp::LibVar(ix), []) => BindingExpr::LibVar(*ix),
            (DreamCoderOp::Shift, [expr]) => BindingExpr::Shift(expr),
            _ => return None,
        };
        Some(binding_expr)
    }

    fn list() -> Self {
        Self::Combine
    }
}

impl Display for DreamCoderOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            DreamCoderOp::Lambda => "lambda",
            DreamCoderOp::App => "@",
            DreamCoderOp::Lib(ix) => return write!(f, "lib {}", ix),
            DreamCoderOp::LibVar(ix) => return write!(f, "{}", ix),
            DreamCoderOp::Shift => "shift",
            DreamCoderOp::Var(index) => return write!(f, "${}", index),
            DreamCoderOp::Inlined(expr) => return write!(f, "#{}", DcExpr::ref_cast(expr)),
            DreamCoderOp::Symbol(symbol) => return write!(f, "{}", symbol),
            DreamCoderOp::Combine => "combine",
        };
        f.write_str(s)
    }
}

impl Printable for DreamCoderOp {
    fn precedence(&self) -> Precedence {
        match self {
            Self::Symbol(_) | Self::Var(_) | Self::LibVar(_) => 60,
            Self::Combine => 50,
            Self::App | Self::Shift => 40,
            Self::Lambda | Self::Lib(_) | Self::Inlined(_) => 10,
        }
    }

    fn print_naked<W: Write>(expr: &Expr<Self>, printer: &mut Printer<W>) -> fmt::Result {
        match (expr.0.operation(), expr.0.args()) {
            (&Self::Symbol(s), []) => {
                write!(printer.writer, "{}", s)
            }
            (&Self::Combine, ts) => {
                let elem = |p: &mut Printer<W>, i: usize| {
                    p.print_in_context(&ts[i], 0) // children do not need parens
                };
                printer.in_brackets(|p| p.indented(|p| p.vsep(elem, ts.len(), ",")))
            }
            (op, _) => write!(printer.writer, "{} ???", op),
        }
    }
}

impl Display for DcExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let node: &AstNode<_, _> = self.as_ref();
        match node.as_parts() {
            (DreamCoderOp::Symbol(name), []) => write!(f, "{}", name),
            (DreamCoderOp::Var(index), []) => {
                write!(f, "${}", index)
            }
            (DreamCoderOp::Inlined(expr), []) => {
                write!(f, "#{}", Self::ref_cast(expr))
            }
            (DreamCoderOp::Lambda, [body]) => {
                write!(f, "(lambda {:.1})", Self::ref_cast(body))
            }
            (DreamCoderOp::App, [fun, arg]) => parens(0, f, |f| {
                write!(f, "{:.0} {:.1}", Self::ref_cast(fun), Self::ref_cast(arg))
            }),
            (op, args) => {
                write!(f, "({}", op)?;
                for arg in args {
                    write!(f, " {}", Self::ref_cast(arg))?;
                }
                f.write_str(")")
            }
        }
    }
}

/// An error produced when a string can't be parsed as a valid [`DcExpr`].
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
        parse::parse(s).map(DcExpr).map_err(|e| ParseExprError {
            message: convert_error(s, e),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::ast_node::AstNode;
    use internment::ArcIntern;

    use super::{DcExpr, DeBruijnIndex, DreamCoderOp};

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
            Self(AstNode::leaf(DreamCoderOp::Inlined(ArcIntern::new(expr.0))).into())
        }
    }

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
