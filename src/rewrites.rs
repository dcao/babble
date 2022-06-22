//! Utilities for loading and parsing rewrites files.
//!
//! # Examples
//!
//! A rewrites file might look like this:
//!
//! ```text
//! one_plus_one: (+ 1 1) => 2
//! commutative: (+ ?x ?y) => (+ ?y ?x)
//! ```

use anyhow::anyhow;
use egg::{Analysis, FromOp, Language, Pattern, Rewrite};
use std::{error::Error, fs, io::ErrorKind, path::Path};

/// Returns all the rewrites in the specified file.
///
/// # Errors
/// This function will return an error if the file doesn't exist or can't be opened.
///
/// It will also return an error if it could not parse the file.
pub fn from_file<L, A, P>(path: P) -> anyhow::Result<Vec<Rewrite<L, A>>>
where
    L: Language + FromOp + Sync + Send + 'static,
    A: Analysis<L>,
    P: AsRef<Path>,
    L::Error: Send + Sync + Error,
{
    let contents = fs::read_to_string(path)?;
    Ok(parse(&contents)?)
}


/// If the file specified by `path` exists, parse the file and return the
/// resulting rewrites. If the file does not exist, return `None`.
///
/// # Errors
/// This function will return an error if the file exists but can't be opened.
///
/// It will also return an error if it could not parse the file.
pub fn try_from_file<L, A, P>(path: P) -> anyhow::Result<Option<Vec<Rewrite<L, A>>>>
where
    L: Language + FromOp + Sync + Send + 'static,
    A: Analysis<L>,
    P: AsRef<Path>,
    L::Error: Send + Sync + Error,
{
    Ok(match fs::read_to_string(path) {
        Ok(contents) => Some(parse(&contents)?),
        Err(e) => match e.kind() {
            ErrorKind::NotFound => None,
            _ => Err(e)?,
        },
    })
}

/// Parse a rewrites file.
pub fn parse<L, A>(file: &str) -> anyhow::Result<Vec<Rewrite<L, A>>>
where
    L: Language + FromOp + Sync + Send + 'static,
    A: Analysis<L>,
    L::Error: Send + Sync + Error,
{
    let mut rewrites = Vec::new();
    for line in file
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
    {
        let (name, rewrite) = line.split_once(':').ok_or(anyhow!("missing colon"))?;
        let (lhs, rhs) = rewrite.split_once("=>").ok_or(anyhow!("missing arrow"))?;
        let name = name.trim();
        let lhs = lhs.trim();
        let rhs = rhs.trim();
        let lhs: Pattern<L> = lhs.parse()?;
        let rhs: Pattern<L> = rhs.parse()?;
        rewrites.push(Rewrite::new(name, lhs, rhs).map_err(|e| anyhow!("{}", e))?);
    }
    Ok(rewrites)
}
