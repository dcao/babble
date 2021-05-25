//! Types and utilities for interacting with Dream&shy;Coder.

// Note: We write Dream&shy;Coder instead of DreamCoder to avoid a false
// positive from a clippy lint.

use core::slice;
use egg::{FromOp, FromOpError, Id, Language, RecExpr, Symbol};
use internment::ArcIntern;
use itertools::Itertools;
use nom::error::convert_error;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    convert::{TryFrom, TryInto},
    fmt::{self, Display, Formatter},
    num::ParseIntError,
    str::FromStr,
};
use thiserror::Error;

/// Emulate Dream&shy;Coder's compression tool.
#[must_use]
pub fn run(input: CompressionInput) -> CompressionOutput {
    eprintln!("Emulating DreamCoder's `compression` tool.");

    eprintln!(
        "Primitives:\n    {}",
        &input
            .dsl
            .productions
            .iter()
            .map(|p| &p.expression)
            .chunks(10)
            .into_iter()
            .map(|mut exprs| exprs.join(", "))
            .join(",\n    ")
    );

    eprintln!("Tasks:");
    for frontier in &input.frontiers {
        eprintln!(
            "    {}: {}",
            frontier.task.as_ref().map_or("<unnamed>", |s| s.as_ref()),
            frontier.request
        );
    }
    CompressionOutput {
        dsl: input.dsl,
        frontiers: input.frontiers,
    }
}

/// The input format of the `compression` tool.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompressionInput {
    #[serde(rename = "CPUs")]
    pub cpus: u32,
    pub arity: u32,
    pub verbose: bool,

    #[serde(rename = "collect_data")]
    pub collect_data: bool,
    pub bs: u32, // what is this
    pub aic: u32,
    pub structure_penalty: u32,
    pub top_k: u32,

    #[serde(rename = "DSL")]
    pub dsl: Dsl,
    pub frontiers: Vec<Frontier>,
}

/// The output format of the `compression` tool.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
pub struct CompressionOutput {
    #[serde(rename = "DSL")]
    pub dsl: Dsl,
    pub frontiers: Vec<Frontier>,
}

/// The primitives and learned functions for the language.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Dsl {
    pub log_variable: f64,
    pub productions: Vec<Production>,
}

/// A primitive or learned function.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Production {
    pub log_probability: f64,
    pub expression: Expr,
}

/// A particular task for `compression` to examine.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Frontier {
    pub task: Option<String>,
    pub request: Type,
    pub programs: Vec<Program>,
}

/// The type of a program.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(from = "RawType<'_>")]
#[serde(into = "RawType<'_>")]
pub enum Type {
    /// A base type, e.g. `int`, `bool`, etc.
    Simple {
        /// The name of the type.
        name: Symbol
    },

    /// A compound type (e.g. `list int`), formed from a named constructor and its arguments.
    Compound {
        /// The name of the constructor.
        constructor: Symbol,

        /// The arguments to the type constructor.
        arguments: Vec<Type>,
    },

    /// A function type, e.g. `int -> int`.
    Function {
        /// The type of the function's argument.
        from: Box<Type>,

        /// The return type of the function.
        to: Box<Type>
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
struct RawType<'a> {
    constructor: &'a str,
    arguments: Vec<RawType<'a>>,
}

fn parens<F>(precedence: usize, formatter: &mut Formatter<'_>, body: F) -> fmt::Result
where
    F: FnOnce(&mut Formatter<'_>) -> fmt::Result,
{
    let level = formatter.precision().unwrap_or(0);
    if level > precedence {
        formatter.write_str("(")?;
    };
    body(formatter)?;
    if level > precedence {
        formatter.write_str(")")?;
    };
    Ok(())
}

impl Type {
    /// Create a new simple type named `name`.
    #[must_use]
    pub fn simple<T: Into<Symbol>>(name: T) -> Self {
        let name = name.into();
        Self::Simple { name }
    }

    /// Create a new compound type with constructor `constructor` and arguments `arguments`.
    #[must_use]
    pub fn compound<T: Into<Symbol>>(constructor: T, arguments: Vec<Type>) -> Self {
        let constructor = constructor.into();
        Self::Compound {
            constructor,
            arguments,
        }
    }

    /// Create a new function type with argument type `from` and return type `to`.
    #[must_use]
    pub fn function(from: Type, to: Type) -> Self {
        let from = from.into();
        let to = to.into();
        Self::Function { from, to }
    }
}

impl<'a> From<RawType<'a>> for Type {
    fn from(raw_type: RawType<'a>) -> Self {
        let RawType {
            constructor,
            mut arguments,
        } = raw_type;

        if arguments.is_empty() {
            Self::simple(constructor)
        } else if constructor == "->" && arguments.len() == 2 {
            let to = arguments.pop().unwrap();
            let from = arguments.pop().unwrap();
            Self::function(from.into(), to.into())
        } else {
            let arguments = arguments.into_iter().map_into().collect();
            Self::compound(constructor, arguments)
        }
    }
}

impl From<Type> for RawType<'_> {
    fn from(r#type: Type) -> Self {
        match r#type {
            Type::Simple { name } => Self {
                constructor: name.as_str(),
                arguments: Vec::new(),
            },
            Type::Compound {
                constructor,
                arguments,
            } => {
                let constructor = constructor.as_str();
                let arguments = arguments.into_iter().map_into().collect();
                Self {
                    constructor,
                    arguments,
                }
            }
            Type::Function { from, to } => Self {
                constructor: "->",
                arguments: vec![(*from).into(), (*to).into()],
            },
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Simple { name } => write!(f, "{}", name),
            Type::Compound {
                constructor,
                arguments,
            } => parens(1, f, |f| {
                write!(f, "{}", constructor)?;
                for argument in arguments {
                    write!(f, " {:.2}", argument)?;
                }
                Ok(())
            }),
            Type::Function { from, to } => parens(0, f, |f| write!(f, "{:.1} -> {:.0}", from, to)),
        }
    }
}

/// A particular program that `compression` will try to compress.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Program {
    pub log_likelihood: f64,
    pub program: Expr,
}

/// An expression in Dream&shy;Coder's generic programming language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "RawExpr<'_>")]
#[serde(into = "RawExpr<'_>")]
pub struct Expr {
    context: RecExpr<AstNode>,
    index: Id,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
struct RawExpr<'a>(Cow<'a, str>);

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

/// A de Bruijn index.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DeBruijnIndex(usize);

impl Display for DeBruijnIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

/// Error type for parsing a de Bruijn index from a string.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ParseDeBruijnIndexError {
    /// The string didn't start with '$'.
    #[error("expected de Bruijn index to start with '$'")]
    MissingSigil,

    /// There was an error parsing the index.
    #[error(transparent)]
    ParseIntError(#[from] ParseIntError),
}

impl FromStr for DeBruijnIndex {
    type Err = ParseDeBruijnIndexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use ParseDeBruijnIndexError::{MissingSigil, ParseIntError};

        s.strip_prefix('$')
            .ok_or(MissingSigil)
            .and_then(|s| s.parse().map_err(ParseIntError))
            .map(Self)
    }
}

impl Expr {
    /// Create a variable expression with de Bruijn index `index`.
    #[must_use]
    pub fn var(n: usize) -> Self {
        let mut context = RecExpr::default();
        let index = context.add(AstNode::Var(DeBruijnIndex(n)));
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

pub(crate) mod parse {
    use std::str::FromStr;

    use super::Expr;

    use nom::{
        branch::alt,
        bytes::complete::{tag, take_till1},
        character::complete::{char, digit1, multispace0, multispace1},
        combinator::{all_consuming, cut, flat_map, map, map_res},
        error::{context, VerboseError},
        multi::fold_many1,
        sequence::{delimited, preceded},
        Finish, IResult, Parser,
    };

    type ParseResult<'a, Output> = IResult<&'a str, Output, VerboseError<&'a str>>;

    fn from_str<I, O1, O2, P>(parser: P) -> impl FnMut(I) -> IResult<I, O2, VerboseError<I>>
    where
        I: Clone,
        O1: AsRef<str>,
        O2: FromStr,
        P: Parser<I, O1, VerboseError<I>>,
    {
        map_res(parser, |s| s.as_ref().parse())
    }

    fn parenthesized<'a, O, P>(parser: P) -> impl FnMut(&'a str) -> ParseResult<'a, O>
    where
        P: Parser<&'a str, O, VerboseError<&'a str>>,
    {
        delimited(
            char('('),
            delimited(multispace0, parser, multispace0),
            char(')'),
        )
    }

    fn var(s: &str) -> ParseResult<'_, Expr> {
        context(
            "variable",
            map(preceded(char('$'), cut(from_str(digit1))), Expr::var),
        )(s)
    }

    fn symbol(s: &str) -> ParseResult<'_, Expr> {
        context(
            "symbol",
            map(
                take_till1(|c: char| c.is_whitespace() || "()$#".find(c).is_some()),
                Expr::symbol,
            ),
        )(s)
    }

    fn inlined(s: &str) -> ParseResult<'_, Expr> {
        context(
            "inlined",
            map(preceded(char('#'), cut(expr)), Expr::inlined),
        )(s)
    }

    fn lambda(s: &str) -> ParseResult<'_, Expr> {
        context(
            "lambda",
            map(
                parenthesized(preceded(tag("lambda"), preceded(multispace1, cut(expr)))),
                Expr::lambda,
            ),
        )(s)
    }

    fn app(s: &str) -> ParseResult<'_, Expr> {
        context(
            "app",
            parenthesized(flat_map(expr, |fun| {
                fold_many1(preceded(multispace1, expr), fun, Expr::app)
            })),
        )(s)
    }

    fn expr(s: &str) -> ParseResult<'_, Expr> {
        alt((var, inlined, symbol, lambda, app))(s)
    }

    pub(crate) fn parse(s: &str) -> Result<Expr, VerboseError<&str>> {
        all_consuming(expr)(s).finish().map(|(_, e)| e)
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
    use super::{Expr, Type};

    #[test]
    fn type_display() {
        let r#type = Type::function(
            Type::function(
                Type::simple("int"),
                Type::compound("list", vec![Type::simple("int")]),
            ),
            Type::function(
                Type::compound(
                    "list",
                    vec![Type::compound("list", vec![Type::simple("bool")])],
                ),
                Type::compound(
                    "result",
                    vec![
                        Type::simple("int"),
                        Type::compound("list", vec![Type::simple("string")]),
                    ],
                ),
            ),
        );
        let expected = "(int -> list int) -> list (list bool) -> result int (list string)";

        assert_eq!(r#type.to_string(), expected)
    }

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
