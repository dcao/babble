use std::{
    fmt::{self, Display, Formatter},
    num::ParseIntError,
    str::FromStr,
};
use thiserror::Error;

pub(crate) fn parens<F>(precedence: usize, formatter: &mut Formatter<'_>, body: F) -> fmt::Result
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

/// A de Bruijn index.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub(crate) struct DeBruijnIndex(pub(crate) usize);

impl Display for DeBruijnIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

/// Error type for parsing a de Bruijn index from a string.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub(crate) enum ParseDeBruijnIndexError {
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
