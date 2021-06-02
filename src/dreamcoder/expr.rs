//! The language of Dream&shy;Coder expressions.

use std::{
    borrow::Cow,
    convert::{TryFrom, TryInto},
    fmt::{self, Display, Formatter},
    slice,
    str::FromStr,
};

use egg::{FromOp, FromOpError, Id, Language, RecExpr, Symbol};
use internment::ArcIntern;
use nom::error::convert_error;
use serde::{Deserialize, Serialize};

use super::{
    parse,
    util::{parens, DeBruijnIndex},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
struct RawExpr<'a>(Cow<'a, str>);

/// An expression in Dream&shy;Coder's generic programming language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "RawExpr<'_>")]
#[serde(into = "RawExpr<'_>")]
pub struct Expr {
    context: RecExpr<AstNode>,
    index: Id,
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

/// An AST node in the Dream&shy;Coder language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AstNode {
    /// A variable.
    Var(DeBruijnIndex),

    /// A symbol, typically one of the language's primitives.
    Symbol(Symbol),

    /// An "inlined" expression. This is how Dream&shy;Coder represents learned
    /// functions.
    Inlined(ArcIntern<Expr>),

    /// An anonymous function.
    Lambda(Id),

    /// An application of a function to a variable. Dream&shy;Coder allows
    /// applying a function to multiple arguments, we translate these to nested
    /// applications. That is, `(foo bar baz quux)`, is interpreted as
    /// `(((foo bar) baz) quux)`.
    App([Id; 2]),
}

impl Expr {
    /// Create a variable expression with de Bruijn index `index`.
    #[must_use]
    pub fn var(n: usize) -> Self {
        let mut context = RecExpr::default();
        let index = context.add(AstNode::Var(DeBruijnIndex::new(n)));
        Self { context, index }
    }

    /// Create a symbol representing `name`.
    #[must_use]
    pub fn symbol<T: Into<Symbol>>(symbol: T) -> Self {
        let mut context = RecExpr::default();
        let index = context.add(AstNode::Symbol(symbol.into()));
        Self { context, index }
    }

    /// Create an expression inlining `expr`.
    #[must_use]
    pub fn inlined(expr: Self) -> Self {
        let expr = ArcIntern::new(expr);
        let mut context = RecExpr::default();
        let index = context.add(AstNode::Inlined(expr));
        Self { context, index }
    }

    /// Create a lambda expression with body `body`.
    #[must_use]
    pub fn lambda(body: Self) -> Self {
        let Self {
            mut context,
            index: body,
        } = body;
        let index = context.add(AstNode::Lambda(body));
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
            .chain(arg_context.as_ref().iter().cloned().map(|node| match node {
                AstNode::Lambda(body) => AstNode::Lambda((usize::from(body) + offset).into()),
                AstNode::App([fun, arg]) => AstNode::App([
                    (usize::from(fun) + offset).into(),
                    (usize::from(arg) + offset).into(),
                ]),
                other => other,
            }))
            .collect();
        let mut context: RecExpr<_> = context.into();
        let index = context.add(AstNode::App([
            fun_index,
            (usize::from(arg_index) + offset).into(),
        ]));
        Self { context, index }
    }
}

impl Language for AstNode {
    fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (AstNode::Var(i1), AstNode::Var(i2)) => i1 == i2,
            (AstNode::Symbol(s1), AstNode::Symbol(s2)) => s1 == s2,
            (AstNode::Inlined(e1), AstNode::Inlined(e2)) => e1 == e2,
            (AstNode::Lambda(_), AstNode::Lambda(_)) | (AstNode::App(_), AstNode::App(_)) => true,
            _ => false,
        }
    }

    fn children(&self) -> &[Id] {
        match self {
            AstNode::Lambda(body) => slice::from_ref(body),
            AstNode::App(children) => children,
            _ => &[],
        }
    }

    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            AstNode::Lambda(body) => slice::from_mut(body),
            AstNode::App(children) => children,
            _ => &mut [],
        }
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AstNode::Var(i) => write!(f, "{}", i),
            AstNode::Symbol(symbol) => write!(f, "{}", symbol),
            AstNode::Inlined(expr) => write!(f, "#{}", expr),
            AstNode::Lambda(_) => f.write_str("lambda"),
            AstNode::App(_) => f.write_str("@"),
        }
    }
}

impl FromOp for AstNode {
    type Error = FromOpError;

    fn from_op(op: &str, children: Vec<Id>) -> Result<Self, Self::Error> {
        let node = match (op, &children[..]) {
            ("lambda", [body]) => Some(AstNode::Lambda(*body)),
            ("@", [fun, arg]) => Some(AstNode::App([*fun, *arg])),
            (_, []) if op.starts_with('$') => op.parse().map(AstNode::Var).ok(),
            (_, []) if op.starts_with('#') => RawExpr(op[1..].into())
                .try_into()
                .map(|expr| AstNode::Inlined(ArcIntern::new(expr)))
                .ok(),
            (_, []) => Some(AstNode::Symbol(op.into())),
            _ => None,
        };
        node.ok_or_else(|| FromOpError::new(op, children))
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        struct ExprRef<'a> {
            context: &'a RecExpr<AstNode>,
            index: Id,
        }

        impl Display for ExprRef<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                let Self { context, index } = *self;
                match &context[index] {
                    AstNode::Var(i) => write!(f, "{}", i),
                    AstNode::Symbol(s) => write!(f, "{}", s),
                    AstNode::Inlined(e) => write!(f, "#{}", e),
                    AstNode::Lambda(body) => {
                        let body = Self {
                            context,
                            index: *body,
                        };
                        write!(f, "(lambda {:.1})", body)
                    }
                    AstNode::App([fun, arg]) => {
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
        assert_eq!(expr.to_string(), input)
    }
}
