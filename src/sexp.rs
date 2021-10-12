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

impl<'a> Sexp<'a> {
    /// Parses a string as an s-expression.
    ///
    /// # Errors
    ///
    /// Returns an error if the string is not a valid s-expression.
    pub fn parse(s: &'a str) -> Result<Self, ParseSexpError> {
        parse::parse_sexp(s)
    }
}

impl<'a> TryFrom<&'a str> for Sexp<'a> {
    type Error = ParseSexpError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        Self::parse(s)
    }
}

pub use parse::ParseSexpError;

mod parse {
    use super::Sexp;
    use nom::{
        branch::alt,
        character::complete::{char, multispace1, none_of, not_line_ending},
        combinator::{all_consuming, map, opt, recognize},
        error::{convert_error, VerboseError},
        multi::{many0, many1_count},
        sequence::{delimited, pair, preceded},
        Finish, IResult,
    };
    use std::fmt::{self, Debug, Formatter};
    use thiserror::Error;

    type ParseResult<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

    /// An error while parsing an s-expression
    #[allow(clippy::module_name_repetitions)]
    #[derive(Clone, Error)]
    #[error("{0}")]
    pub struct ParseSexpError(String);

    impl Debug for ParseSexpError {
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

    fn program(input: &str) -> ParseResult<'_, Sexp<'_>> {
        all_consuming(delimited(opt(whitespace), sexp, opt(whitespace)))(input)
    }

    pub(super) fn parse_sexp(input: &str) -> Result<Sexp<'_>, ParseSexpError> {
        match program(input).finish() {
            Ok(("", sexp)) => Ok(sexp),
            Err(e) => Err(ParseSexpError(convert_error(input, e))),
            _ => unreachable!(),
        }
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
            assert!(matches!(program(""), Err(_)));
            assert!(matches!(program("()"), Err(_)));
            assert!(matches!(program(";foo\n"), Err(_)));
            assert_eq!(
                program(";foo\n(abc)\n"),
                Ok(("", Sexp::List("abc", vec![])))
            );
        }
    }
}

impl<'a> Debug for Sexp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Atom(atom) => write!(f, "{:?}", atom),
            Self::List(op, args) => {
                f.write_str("(")?;
                Debug::fmt(op, f)?;
                for arg in args {
                    f.write_str(" ")?;
                    Debug::fmt(arg, f)?;
                }
                f.write_str(")")
            }
        }
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
