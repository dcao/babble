//! A parser for programs represented as s-expressions.

use std::{
    convert::TryFrom,
    fmt::{self, Debug, Display, Formatter},
};

/// S-expressions
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Sexp<'a> {
    /// An atomic s-expression
    Atom(&'a str),
    /// A list starting with an atomic s-expression
    List(&'a str, Vec<Self>),
}

/// A program consisting of several s-expressions
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Program<'a>(pub Vec<Sexp<'a>>);

impl<'a> Sexp<'a> {
    /// Parses a string as an s-expression, which may contain comments.
    ///
    /// # Errors
    ///
    /// Returns an error if the string is not a valid s-expression.
    pub fn parse(s: &'a str) -> Result<Self, ParseError> {
        parse::parse_sexp(s)
    }
}

impl<'a> Program<'a> {
    /// Parses a program consisting of s-expressions, whitespace, and comments.
    ///
    /// # Errors
    ///
    /// Returns an error if the program contains an invalid s-expression.
    pub fn parse(s: &'a str) -> Result<Self, ParseError> {
        parse::parse_program(s).map(Self)
    }
}

impl<'a> TryFrom<&'a str> for Sexp<'a> {
    type Error = ParseError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        Self::parse(s)
    }
}

impl<'a> TryFrom<&'a str> for Program<'a> {
    type Error = ParseError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        Self::parse(s)
    }
}

impl<'a> From<Program<'a>> for Vec<Sexp<'a>> {
    fn from(program: Program<'a>) -> Self {
        program.0
    }
}

pub use parse::ParseError;

mod parse {
    use super::Sexp;
    use nom::{
        branch::alt,
        character::complete::{char, multispace1, none_of, not_line_ending},
        combinator::{all_consuming, map, opt, recognize},
        error::{convert_error, VerboseError},
        multi::{many0, many1_count},
        sequence::{delimited, pair, preceded, terminated},
        Finish, IResult, Parser,
    };
    use std::fmt::{self, Debug, Formatter};
    use thiserror::Error;

    type ParseResult<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

    /// An error while parsing an s-expression
    #[allow(clippy::module_name_repetitions)]
    #[derive(Clone, Error, PartialEq, Eq)]
    #[error("{0}")]
    pub struct ParseError(String);

    impl Debug for ParseError {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            f.write_str(&self.0)
        }
    }

    fn comment(input: &str) -> ParseResult<'_, &str> {
        recognize(preceded(char(';'), not_line_ending))(input)
    }

    fn whitespace(input: &str) -> ParseResult<'_, &str> {
        recognize(many1_count(alt((comment, multispace1))))(input)
    }

    fn symbol(input: &str) -> ParseResult<'_, &str> {
        recognize(many1_count(none_of(";() \t\n\r")))(input)
    }

    fn atom(input: &str) -> ParseResult<'_, Sexp<'_>> {
        map(symbol, Sexp::Atom)(input)
    }

    fn list(input: &str) -> ParseResult<'_, Sexp<'_>> {
        map(
            delimited(
                pair(char('('), opt(whitespace)),
                pair(symbol, many0(preceded(whitespace, sexp))),
                pair(opt(whitespace), char(')')),
            ),
            |(op, args)| Sexp::List(op, args),
        )(input)
    }

    fn sexp(input: &str) -> ParseResult<'_, Sexp<'_>> {
        alt((list, atom))(input)
    }

    fn program(input: &str) -> ParseResult<'_, Vec<Sexp<'_>>> {
        preceded(opt(whitespace), many0(terminated(sexp, opt(whitespace))))(input)
    }

    fn complete<'a, O, F>(mut f: F) -> impl FnMut(&'a str) -> Result<O, ParseError>
    where
        F: Parser<&'a str, O, VerboseError<&'a str>>,
    {
        move |input| match all_consuming(|s| f.parse(s))(input).finish() {
            Ok(("", val)) => Ok(val),
            Err(e) => Err(ParseError(convert_error(input, e))),
            _ => unreachable!(),
        }
    }

    pub(super) fn parse_sexp(input: &str) -> Result<Sexp<'_>, ParseError> {
        complete(sexp)(input)
    }

    pub(super) fn parse_program(input: &str) -> Result<Vec<Sexp<'_>>, ParseError> {
        complete(program)(input)
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_comment() {
            assert!(matches!(comment(""), Err(_)));
            assert_eq!(comment(";"), Ok(("", ";")));
            assert_eq!(comment(";\n"), Ok(("\n", ";")));
            assert_eq!(comment("; abc"), Ok(("", "; abc")));
            assert_eq!(comment("; abc\n"), Ok(("\n", "; abc")));
        }

        #[test]
        fn test_whitespace() {
            assert!(matches!(whitespace(""), Err(_)));
            assert_eq!(whitespace(" abc"), Ok(("abc", " ")));
            assert_eq!(
                whitespace(" ; foo\n \t; bar\nbaz"),
                Ok(("baz", " ; foo\n \t; bar\n"))
            );
        }

        #[test]
        fn test_symbol() {
            assert!(matches!(symbol(""), Err(_)));
            assert_eq!(symbol("abc()"), Ok(("()", "abc")));
            assert_eq!(symbol("foo;bar\nbaz"), Ok((";bar\nbaz", "foo")));
            assert_eq!(symbol("@λ "), Ok((" ", "@λ")));
        }

        #[test]
        fn test_list() {
            assert!(matches!(list(""), Err(_)));
            assert!(matches!(list("()"), Err(_)));
            assert_eq!(list("(abc) "), Ok((" ", Sexp::List("abc", vec![]))));
            assert_eq!(
                list("(foo (bar) baz)"),
                Ok((
                    "",
                    Sexp::List("foo", vec![Sexp::List("bar", vec![]), Sexp::Atom("baz")])
                ))
            );
        }

        #[test]
        fn test_sexp() {
            assert!(matches!(sexp(""), Err(_)));
            assert!(matches!(sexp("()"), Err(_)));
            assert_eq!(sexp("(abc) "), Ok((" ", Sexp::List("abc", vec![]))));
            assert_eq!(
                sexp("(foo (bar) baz)"),
                Ok((
                    "",
                    Sexp::List("foo", vec![Sexp::List("bar", vec![]), Sexp::Atom("baz")])
                ))
            );
            assert_eq!(sexp("foo)"), Ok((")", Sexp::Atom("foo"))));
        }

        #[test]
        fn test_program() {
            assert_eq!(program(""), Ok(("", vec![])));

            assert_eq!(program(";foo\n"), Ok(("", vec![])));

            assert!(matches!(all_consuming(program)("()"), Err(_)));
            assert_eq!(
                program(";foo\n(abc)\n"),
                Ok(("", vec![Sexp::List("abc", vec![])]))
            );

            assert_eq!(
                program("\n(abc)\n(foo (bar baz)) ;quux"),
                Ok((
                    "",
                    vec![
                        Sexp::List("abc", vec![]),
                        Sexp::List("foo", vec![Sexp::List("bar", vec![Sexp::Atom("baz")])])
                    ]
                ))
            );

            assert_eq!(
                program("a b c"),
                Ok(("", vec![Sexp::Atom("a"), Sexp::Atom("b"), Sexp::Atom("c")]))
            );
        }
    }
}

impl<'a> Debug for Sexp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("`")?;
        Display::fmt(self, f)?;
        f.write_str("`")
    }
}

impl<'a> Debug for Program<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("```\n")?;
        Display::fmt(self, f)?;
        f.write_str("\n```")
    }
}

impl<'a> Display for Sexp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Atom(atom) => write!(f, "{}", atom),
            Self::List(op, args) => {
                f.write_str("(")?;
                Display::fmt(op, f)?;
                for arg in args {
                    f.write_str(" ")?;
                    Display::fmt(arg, f)?;
                }
                f.write_str(")")
            }
        }
    }
}

impl<'a> Display for Program<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            f.write_str("; this program is empty")
        } else {
            Display::fmt(&self.0[0], f)?;
            for sexp in &self.0[1..] {
                f.write_str("\n")?;
                Display::fmt(sexp, f)?;
            }
            Ok(())
        }
    }
}
