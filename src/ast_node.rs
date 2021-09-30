//! Abstract syntax trees.
use egg::{FromOp, Id, Language};
use std::{
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    iter::FromIterator,
    ops::{Bound, RangeBounds},
    slice,
    str::FromStr,
    vec,
};
use thiserror::Error;

/// An abstract syntax tree node with operation of type `Op` and children of
/// type `T`.
///
/// This type implements [`Language`] for children of type [`Id`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstNode<Op, T = Id> {
    operation: Op,
    children: Vec<T>,
}

pub use expr::Expr;
pub use partial_expr::PartialExpr;

mod expr;
mod partial_expr;

/// A trait for operations which take a specific number of arguments.
pub trait Arity {
    /// Returns the minimum number of arguments this operation can take.
    fn min_arity(&self) -> usize;

    /// Returns the maximum number of arguments this operation can take, or
    /// [`None`] if there is no maximum.
    fn max_arity(&self) -> Option<usize> {
        Some(self.min_arity())
    }

    /// Returns a pair of range bounds representing the minimum and maximum
    /// number of arguments.
    fn arity(&self) -> (Bound<usize>, Bound<usize>) {
        let lower_bound = Bound::Included(self.min_arity());
        let upper_bound = match self.max_arity() {
            Some(max_arity) => Bound::Included(max_arity),
            None => Bound::Unbounded,
        };

        (lower_bound, upper_bound)
    }
}

impl<Op, T> AstNode<Op, T> {
    /// Returns the operation the node represents.
    #[must_use]
    pub fn operation(&self) -> &Op {
        &self.operation
    }

    /// Returns a slice containing the node's children.
    #[must_use]
    pub fn children(&self) -> &[T] {
        &self.children
    }

    /// Returns a slice which allows modifying the node's children.
    #[must_use]
    pub fn children_mut(&mut self) -> &mut [T] {
        &mut self.children
    }

    /// Returns `true` if the node has no children.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }

    /// Returns the number of children the node has.
    #[must_use]
    pub fn len(&self) -> usize {
        self.children.len()
    }

    /// Converts an `AstNode<Op, T>` into an `AstNode<Op, U>` by applying a
    /// function to each of its children.
    #[must_use]
    pub fn map<U, F>(self, f: F) -> AstNode<Op, U>
    where
        F: FnMut(T) -> U,
    {
        AstNode {
            operation: self.operation,
            children: self.children.into_iter().map(f).collect(),
        }
    }

    /// Returns an iterator over the node's children.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.into_iter()
    }

    /// Returns an iterator that allows modifying the node's children.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.into_iter()
    }

    /// Returns a reference to the node's operation and a slice of its children.
    #[must_use]
    pub fn as_parts(&self) -> (&Op, &[T]) {
        (&self.operation, &self.children)
    }

    /// Decomposes the node into its operation and children.
    #[must_use]
    pub fn into_parts(self) -> (Op, Vec<T>) {
        (self.operation, self.children)
    }
}

impl<Op: Arity, T> AstNode<Op, T> {
    /// Creates a node with the given operation and children.
    ///
    /// See also [`AstNode::into_parts`].
    ///
    /// # Panics
    ///
    /// Panics if the number of children does not match the
    /// [`arity`](Arity::arity) of the operation.
    #[must_use]
    pub fn new<I>(operation: Op, children: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let children = Vec::from_iter(children);
        assert!(operation.arity().contains(&children.len()));
        Self {
            operation,
            children,
        }
    }

    /// Creates a leaf node with the given operation.
    ///
    /// # Panics
    ///
    /// Panics if the [`arity`](Arity::arity) of the operation is not zero.
    #[must_use]
    pub fn leaf(operation: Op) -> Self {
        Self::new(operation, [])
    }
}

impl<Op, T> AsRef<[T]> for AstNode<Op, T> {
    /// Returns a reference to the node's children.
    fn as_ref(&self) -> &[T] {
        self.children()
    }
}

impl<Op, T> AsMut<[T]> for AstNode<Op, T> {
    /// Returns a reference which allows modifying the node's children.
    fn as_mut(&mut self) -> &mut [T] {
        self.children_mut()
    }
}

impl<'a, Op, T> IntoIterator for &'a AstNode<Op, T> {
    type Item = &'a T;

    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.children.iter()
    }
}

impl<'a, Op, T> IntoIterator for &'a mut AstNode<Op, T> {
    type Item = &'a mut T;

    type IntoIter = slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.children.iter_mut()
    }
}

impl<Op, T> IntoIterator for AstNode<Op, T> {
    type Item = T;

    type IntoIter = vec::IntoIter<T>;

    /// Converts the node into an iterator over its children.
    fn into_iter(self) -> Self::IntoIter {
        self.children.into_iter()
    }
}

impl<Op> Language for AstNode<Op>
where
    Op: Ord + Debug + Clone + Hash,
{
    fn matches(&self, other: &Self) -> bool {
        self.operation == other.operation
    }

    fn children(&self) -> &[Id] {
        self.children()
    }

    // Default methods

    fn children_mut(&mut self) -> &mut [Id] {
        self.children_mut()
    }

    fn for_each<F: FnMut(Id)>(&self, f: F) {
        self.iter().copied().for_each(f);
    }

    fn for_each_mut<F: FnMut(&mut Id)>(&mut self, f: F) {
        self.iter_mut().for_each(f);
    }

    fn try_for_each<E, F>(&self, f: F) -> Result<(), E>
    where
        F: FnMut(Id) -> Result<(), E>,
        E: Clone,
    {
        self.iter().copied().try_for_each(f)
    }

    fn len(&self) -> usize {
        self.len()
    }

    fn is_leaf(&self) -> bool {
        self.is_empty()
    }

    fn fold<F, T>(&self, init: T, f: F) -> T
    where
        F: FnMut(T, Id) -> T,
        T: Clone,
    {
        self.iter().copied().fold(init, f)
    }

    fn all<F>(&self, f: F) -> bool
    where
        F: FnMut(Id) -> bool,
    {
        self.iter().copied().all(f)
    }

    fn any<F>(&self, f: F) -> bool
    where
        F: FnMut(Id) -> bool,
    {
        self.iter().copied().any(f)
    }
}

/// An error which can be returned when parsing an expression using [`FromOp`].
#[derive(Debug, Error)]
pub enum ParseNodeError<Op>
where
    Op: FromStr + Debug,
    <Op as FromStr>::Err: Error,
{
    /// The operator failed to parse.
    #[error(transparent)]
    ParseError(<Op as FromStr>::Err),

    /// The operator was given the wrong number of arguments.
    #[error(
        "the operation `{operation:?}` takes {expected:?} argument(s) but was applied to {actual}"
    )]
    ArityError {
        /// The operation.
        operation: Op,
        /// The expected number of arguments.
        expected: (Bound<usize>, Bound<usize>),
        /// The given number of arguments.
        actual: usize,
    },
}

impl<Op> FromOp for AstNode<Op>
where
    Op: Arity + FromStr + Debug + Clone + Ord + Hash + 'static,
    <Op as FromStr>::Err: Error,
{
    type Error = ParseNodeError<Op>;

    fn from_op(op: &str, children: Vec<Id>) -> Result<Self, Self::Error> {
        let op: Op = op.parse().map_err(ParseNodeError::ParseError)?;
        let arity = op.arity();
        if arity.contains(&children.len()) {
            Ok(Self::new(op, children))
        } else {
            Err(ParseNodeError::ArityError {
                operation: op,
                expected: arity,
                actual: children.len(),
            })
        }
    }
}

/// [Egg][egg] expects the [`Display`] implementation of a [`Language`] to
/// display only a node's operation, not its children. This implementation is
/// unexpected, so we only implement [`Display`] for the concrete type
/// [`AstNode<Op, Id>`].
impl<Op: Display> Display for AstNode<Op> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.operation.fmt(f)
    }
}
