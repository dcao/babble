//! The [`Expr`] type and various related constructs. An [`Expr`] is a pair of a
//! "kind" and a list of children. This type implements [`egg::Language`] when
//! the children have type `Id`, but can be simpler to implement and more
//! flexible.

use egg::{FromOp, Id, Language};
use std::{
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    iter::FromIterator,
    str::FromStr,
    vec,
};
use thiserror::Error;

/// An expression representing an operation of type `K` applied to arguments of
/// type `T`.
///
///  Typically `T` is a type whose value corresponds to another [`Expr<K, T>`],
///  like an index into an array or a key for a hash table.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Expr<K, T = Id> {
    kind: K,
    children: Vec<T>,
}

/// A trait for operations which have an arity.
pub trait Arity {
    /// The arity of this operation. For example, the addition operator `+` has
    /// an arity of 2, whereas the negation operator `!` has an arity of 1.
    ///
    /// Operations which take no arguments (i.e., constants) are considered to
    /// have an arity of 0.
    #[must_use]
    fn arity(&self) -> usize;
}

impl<K, T> Expr<K, T> {
    /// The kind of this expression.
    #[must_use]
    pub fn kind(&self) -> &K {
        &self.kind
    }

    /// Returns an iterator over the children of this expression.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.children.iter()
    }

    /// Returns an iterator over mutable references to the children of this expression.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.children.iter_mut()
    }

    /// Turn an expression into its component parts.
    ///
    /// See also [`Expr::from_parts`].
    #[must_use]
    pub fn into_parts(self) -> (K, Vec<T>) {
        (self.kind, self.children)
    }
}

impl<K: Arity, T> Expr<K, T> {
    /// Create an expression with kind `kind` and children `children`.
    ///
    /// See also [`Expr::into_parts`].
    ///
    /// # Panics
    ///
    /// Panics if the arity of `kind` does not match the length of `children`.
    #[must_use]
    pub fn from_parts<I>(kind: K, children: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let children = Vec::from_iter(children);
        assert_eq!(kind.arity(), children.len());

        Self { kind, children }
    }
}

impl<K, T> IntoIterator for Expr<K, T> {
    type Item = T;

    type IntoIter = vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.children.into_iter()
    }
}

impl<K> Language for Expr<K>
where
    K: Ord + Debug + Clone + Hash,
{
    fn matches(&self, other: &Self) -> bool {
        self.kind == other.kind
    }

    fn children(&self) -> &[Id] {
        &self.children
    }

    // Default methods

    fn children_mut(&mut self) -> &mut [Id] {
        &mut self.children
    }

    fn for_each<F: FnMut(Id)>(&self, f: F) {
        self.children.iter().copied().for_each(f);
    }

    fn for_each_mut<F: FnMut(&mut Id)>(&mut self, f: F) {
        self.children.iter_mut().for_each(f);
    }

    fn try_for_each<E, F>(&self, f: F) -> Result<(), E>
    where
        F: FnMut(Id) -> Result<(), E>,
        E: Clone,
    {
        self.children.iter().copied().try_for_each(f)
    }

    fn len(&self) -> usize {
        self.children.len()
    }

    fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }

    fn fold<F, T>(&self, init: T, f: F) -> T
    where
        F: FnMut(T, Id) -> T,
        T: Clone,
    {
        self.children.iter().copied().fold(init, f)
    }

    fn all<F>(&self, f: F) -> bool
    where
        F: FnMut(Id) -> bool,
    {
        self.children.iter().copied().all(f)
    }

    fn any<F>(&self, f: F) -> bool
    where
        F: FnMut(Id) -> bool,
    {
        self.children.iter().copied().all(f)
    }
}

/// An error while attempting to parse an expression from a string.
#[derive(Debug, Error)]
pub enum ParseError<K>
where
    K: FromStr + Debug,
    K::Err: Error,
{
    /// An error while attempting to parse an operator.
    #[error(transparent)]
    ParseError(<K as FromStr>::Err),

    /// An operator was applied to the wrong number of arguments.
    #[error("wrong number of arguments: expected {expected} but received {actual}")]
    ArityError {
        /// The expected number of arguments.
        expected: usize,
        /// The given number of arguments.
        actual: usize,
    },
}

impl<K> FromOp for Expr<K>
where
    K: Arity + FromStr + Debug + Clone + Ord + Hash + 'static,
    K::Err: Error,
{
    type Error = ParseError<K>;

    fn from_op(op: &str, children: Vec<Id>) -> Result<Self, Self::Error> {
        let kind: K = op.parse().map_err(ParseError::ParseError)?;
        if kind.arity() == children.len() {
            Ok(Self::from_parts(kind, children))
        } else {
            Err(ParseError::ArityError {
                expected: kind.arity(),
                actual: children.len(),
            })
        }
    }
}

impl<K: Display> Display for Expr<K> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}
