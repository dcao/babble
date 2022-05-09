use super::expr::DreamCoderOp;
use crate::ast_node::{AstNode, Expr};
use egg::Symbol;
use internment::ArcIntern;
use std::str::FromStr;

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

fn var(s: &str) -> ParseResult<'_, Expr<DreamCoderOp>> {
    context(
        "variable",
        map(preceded(char('$'), cut(from_str(digit1))), |index| {
            AstNode::leaf(DreamCoderOp::Var(index)).into()
        }),
    )(s)
}

fn symbol(s: &str) -> ParseResult<'_, Expr<DreamCoderOp>> {
    context(
        "symbol",
        map(
            take_till1(|c: char| c.is_whitespace() || "()$#".find(c).is_some()),
            |symbol| AstNode::leaf(DreamCoderOp::Symbol(Symbol::from(symbol))).into(),
        ),
    )(s)
}

fn inlined(s: &str) -> ParseResult<'_, Expr<DreamCoderOp>> {
    context(
        "inlined",
        map(preceded(char('#'), cut(expr)), |expr| {
            AstNode::leaf(DreamCoderOp::Inlined(ArcIntern::new(expr.into()))).into()
        }),
    )(s)
}

fn lambda(s: &str) -> ParseResult<'_, Expr<DreamCoderOp>> {
    context(
        "lambda",
        map(
            parenthesized(preceded(tag("lambda"), preceded(multispace1, cut(expr)))),
            |body| AstNode::new(DreamCoderOp::Lambda, [body]).into(),
        ),
    )(s)
}

fn app(s: &str) -> ParseResult<'_, Expr<DreamCoderOp>> {
    context(
        "app",
        parenthesized(flat_map(expr, |fun| {
            fold_many1(
                preceded(multispace1, expr),
                move || fun.clone(),
                |fun, arg| AstNode::new(DreamCoderOp::App, [fun, arg]).into(),
            )
        })),
    )(s)
}

fn expr(s: &str) -> ParseResult<'_, Expr<DreamCoderOp>> {
    alt((var, inlined, symbol, lambda, app))(s)
}

pub(crate) fn parse(s: &str) -> Result<Expr<DreamCoderOp>, VerboseError<&str>> {
    all_consuming(expr)(s).finish().map(|(_, e)| e)
}
