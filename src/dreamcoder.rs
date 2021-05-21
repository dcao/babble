#![allow(clippy::clippy::doc_markdown)]
//! Types and utilities for interacting with DreamCoder.

use nom::error::convert_error;
use serde::{
    de::{self, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

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
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
pub struct Frontier {
    pub task: Option<String>,
    pub request: Type,
    pub programs: Vec<Program>,
}

/// The type of a program.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize)]
pub struct Type {
    pub arguments: Vec<Type>,
    pub constructor: String,
}

/// A particular program that `compression` will try to compress.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Program {
    pub log_likelihood: f64,
    pub program: Expr,
}

/// An expression in DreamCoder's generic programming language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    /// A de Bruijn-indexed variable.
    Var(u32),

    /// A symbol, typically one of the language's primitives.
    Symbol(egg::Symbol),

    /// An "inlined" expression. This is how DreamCoder represents learned
    /// functions.
    Inlined(Box<Expr>),

    /// An anonymous function.
    Lambda(Box<Expr>),

    /// An application of a function to a variable. DreamCoder allows applying a
    /// function to multiple arguments, we translate these to nested
    /// applications. That is, `(foo bar baz quux)`, is interpreted as
    /// `(((foo bar) baz) quux)`.
    App(Box<Expr>, Box<Expr>),
}

impl Expr {
    /// Create a variable expression with de Bruijn index `index`.
    #[must_use]
    pub fn var(index: u32) -> Self {
        Self::Var(index)
    }

    /// Create a symbol representing `name`.
    #[must_use]
    pub fn symbol<T: Into<egg::Symbol>>(name: T) -> Self {
        Self::Symbol(name.into())
    }

    /// Create an expression inlining `expr`.
    #[must_use]
    pub fn inlined(expr: Self) -> Self {
        Self::Inlined(Box::new(expr))
    }

    /// Create a lambda expression with body `body`.
    #[must_use]
    pub fn lambda(body: Self) -> Self {
        Self::Lambda(Box::new(body))
    }

    /// Create an expression applying `fun` to `arg`.
    #[must_use]
    pub fn app(fun: Self, arg: Self) -> Self {
        Self::App(Box::new(fun), Box::new(arg))
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(index) => write!(f, "${}", index),
            Expr::Symbol(name) => write!(f, "{}", name),
            Expr::Inlined(expr) => write!(f, "#{}", expr),
            Expr::Lambda(body) => write!(f, "(lambda {})", body),
            Expr::App(fun, arg) => {
                // Use the alternate flag to determine whether the expression
                // should be enclosed in parentheses
                if f.alternate() {
                    write!(f, "{:#} {}", fun, arg)
                } else {
                    write!(f, "({:#} {})", fun, arg)
                }
            }
        }
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
        map(
            take_till1(|c: char| c.is_whitespace() || "()$#".find(c).is_some()),
            Expr::symbol,
        )(s)
    }

    fn inlined(s: &str) -> ParseResult<'_, Expr> {
        context(
            "operator",
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
            parenthesized(flat_map(expr, |e| {
                fold_many1(preceded(multispace1, expr), e, Expr::app)
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

impl Serialize for Expr {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct ParseExpr;

impl Visitor<'_> for ParseExpr {
    type Value = Expr;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "an expression as a string")
    }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        s.parse().map_err(de::Error::custom)
    }
}

impl<'de> Deserialize<'de> for Expr {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_str(ParseExpr)
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
