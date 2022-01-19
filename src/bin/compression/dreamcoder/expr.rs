//! The language of Dream&shy;Coder expressions.

use std::{
    borrow::Cow,
    convert::{TryFrom, TryInto},
    fmt::{self, Display, Formatter},
    slice,
    str::FromStr,
};

use babble::{
    ast_node::{Arity, AstNode},
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
pub struct Expr {
    pub context: RecExpr<AstNode<DreamCoderOp>>,
    pub index: Id,
}

impl<'a> From<&'a Expr> for RecExpr<AstNode<DreamCoderOp>> {
    fn from(expr: &'a Expr) -> Self {
        expr.context.as_ref()[..=expr.index.into()].to_vec().into()
    }
}

impl From<Expr> for RawExpr<'_> {
    fn from(expr: Expr) -> Self {
        Self(expr.to_string().into())
    }
}

impl<'a> TryFrom<RawExpr<'a>> for Expr {
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
    Inlined(ArcIntern<Expr>),

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

impl Expr {
    /// Create a variable expression with de Bruijn index `index`.
    #[must_use]
    pub fn var(index: usize) -> Self {
        let mut context = RecExpr::default();
        let index = context.add(AstNode::leaf(DreamCoderOp::Var(index)));
        Self { context, index }
    }

    /// Create a symbol representing `name`.
    #[must_use]
    pub fn symbol<T: Into<Symbol>>(symbol: T) -> Self {
        let mut context = RecExpr::default();
        let index = context.add(AstNode::leaf(DreamCoderOp::Symbol(symbol.into())));
        Self { context, index }
    }

    /// Create an expression inlining `expr`.
    #[must_use]
    pub fn inlined(expr: Self) -> Self {
        let expr = ArcIntern::new(expr);
        let mut context = RecExpr::default();
        let index = context.add(AstNode::leaf(DreamCoderOp::Inlined(expr)));
        Self { context, index }
    }

    /// Create a lambda expression with body `body`.
    #[must_use]
    pub fn lambda(body: Self) -> Self {
        let Self {
            mut context,
            index: body,
        } = body;
        let index = context.add(AstNode::new(DreamCoderOp::Lambda, [body]));
        Self { context, index }
    }

    /// Create an expression applying `fun` to `arg`.
    #[must_use]
    pub fn app(fun: Self, arg: Self) -> Self {
        let Self {
            context: fun_context,
            index: fun_index,
        } = fun;
        let Self {
            context: arg_context,
            index: arg_index,
        } = arg;
        let offset = fun_context.as_ref().len();
        let context: Vec<_> = fun_context
            .as_ref()
            .iter()
            .cloned()
            .chain(
                arg_context
                    .as_ref()
                    .iter()
                    .cloned()
                    .map(|node| match node.as_parts() {
                        (DreamCoderOp::Lambda, &[body]) => AstNode::new(
                            DreamCoderOp::Lambda,
                            [(usize::from(body) + offset).into()],
                        ),
                        (DreamCoderOp::App, &[fun, arg]) => AstNode::new(
                            DreamCoderOp::App,
                            [
                                (usize::from(fun) + offset).into(),
                                (usize::from(arg) + offset).into(),
                            ],
                        ),
                        _ => node,
                    }),
            )
            .collect();
        let mut context: RecExpr<_> = context.into();
        let index = context.add(AstNode::new(
            DreamCoderOp::App,
            [fun_index, (usize::from(arg_index) + offset).into()],
        ));
        Self { context, index }
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

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        struct ExprRef<'a> {
            context: &'a RecExpr<AstNode<DreamCoderOp>>,
            index: Id,
        }

        impl Display for ExprRef<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                let Self { context, index } = *self;
                match context[index].as_parts() {
                    (
                        op
                        @
                        (DreamCoderOp::Var(_)
                        | DreamCoderOp::Inlined(_)
                        | DreamCoderOp::Symbol(_)),
                        [],
                    ) => write!(f, "{}", op),
                    (DreamCoderOp::Lambda, [body]) => {
                        let body = Self {
                            context,
                            index: *body,
                        };
                        write!(f, "(lambda {:.1})", body)
                    }
                    (DreamCoderOp::App, [fun, arg]) => {
                        let fun = Self {
                            context,
                            index: *fun,
                        };
                        let arg = Self {
                            context,
                            index: *arg,
                        };
                        parens(0, f, |f| write!(f, "{:.0} {:.1}", fun, arg))
                    }
                    (op, args) => {
                        write!(f, "({}", op)?;
                        for arg in args {
                            let arg = Self {
                                context,
                                index: *arg,
                            };
                            write!(f, " {}", arg)?;
                        }
                        f.write_str(")")
                    }
                }
            }
        }

        let expr_ref = ExprRef {
            context: &self.context,
            index: self.index,
        };

        write!(f, "{:.1}", expr_ref)
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

impl FromStr for Expr {
    type Err = ParseExprError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse::parse(s).map_err(|e| ParseExprError {
            message: convert_error(s, e),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::Expr;

    #[test]
    fn parser_test() {
        let input = "(lambda (map #(lambda (+ $0 1)) $0))";
        let expr = Expr::lambda(Expr::app(
            Expr::app(
                Expr::symbol("map"),
                Expr::inlined(Expr::lambda(Expr::app(
                    Expr::app(Expr::symbol("+"), Expr::var(0)),
                    Expr::symbol("1"),
                ))),
            ),
            Expr::var(0),
        ));
        let parsed: Expr = input.parse().unwrap();

        assert_eq!(parsed, expr);
        assert_eq!(expr.to_string(), input);
    }
}
