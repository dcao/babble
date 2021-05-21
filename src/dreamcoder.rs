use nom::error::convert_error;
use serde::{
    de::{self, Unexpected, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::{
    borrow::Cow,
    fmt::{self, Display, Formatter, Write},
    ops::RangeFrom,
    str::FromStr,
};

#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompressionInput {
    #[serde(rename = "CPUs")]
    cpus: u32,
    arity: u32,
    verbose: bool,

    #[serde(rename = "collect_data")]
    collect_data: bool,
    bs: u32, // what is this
    aic: u32,
    structure_penalty: u32,
    top_k: u32,

    #[serde(rename = "DSL")]
    dsl: Dsl,
    frontiers: Vec<Frontier>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
pub struct CompressionOutput {
    #[serde(rename = "DSL")]
    dsl: Dsl,
    frontiers: Vec<Frontier>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Dsl {
    log_variable: f64,
    productions: Vec<Production>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Production {
    log_probability: f64,
    expression: Expr,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
pub struct Frontier {
    task: Option<String>,
    request: Type,
    programs: Vec<Program>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize)]
pub struct Type {
    arguments: Vec<Type>,
    constructor: String,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Program {
    log_likelihood: f64,
    program: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    Var(u32),
    Symbol(egg::Symbol),
    Inlined(Box<Expr>),
    Lambda(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn var(index: u32) -> Self {
        Self::Var(index)
    }

    fn symbol<T: Into<egg::Symbol>>(name: T) -> Self {
        Self::Symbol(name.into())
    }

    fn inlined(expr: Self) -> Self {
        Self::Inlined(Box::new(expr))
    }

    fn lambda(body: Self) -> Self {
        Self::Lambda(Box::new(body))
    }

    fn app(fun: Self, arg: Self) -> Self {
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
