//! The [`AstNode`] type and various related constructs. An [`AstNode`] is a pair of a
//! "kind" and a list of children. This type implements [`Language`] when
//! the children have type [`Id`], but can be simpler to implement and more
//! flexible.

use egg::{FromOp, Id, Language};
use std::{
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    iter::{self, FromIterator},
    slice,
    str::FromStr,
    vec,
};
use thiserror::Error;

/// An abstract syntax tree (AST) node with operation of type `Op` and children
/// of type `T`.
///
/// Typically `T` is a type used to identify another AST node, such as an index
/// into an array or key to a hash table. Several methods rely on `T` being
/// [`Copy`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstNode<Op, T = Id> {
    operation: Op,
    children: Vec<T>,
}

/// A trait for types whose values represent operations which take a specific
/// number of arguments.
pub trait Arity {
    /// The number of arguments this operation takes. For example, the addition
    /// operator has an arity of 2, whereas the logical negation operator has an
    /// arity of 1.
    ///
    /// Constants are considered operations which take no arguments, and so have
    /// an arity of 0.
    #[must_use]
    fn arity(&self) -> usize;
}

impl<Op, T> AstNode<Op, T> {
    /// The operation this AST node represents.
    #[must_use]
    pub fn operation(&self) -> &Op {
        &self.operation
    }

    /// Does this AST node have any children?
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }

    /// The number of children this AST node has.
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

    /// Returns an iterator over the children of this AST node.
    pub fn iter(&self) -> impl Iterator<Item = T> + '_
    where
        T: Copy,
    {
        self.into_iter()
    }

    /// Returns an iterator over mutable references to the children of this AST
    /// node.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.into_iter()
    }

    /// Convert a reference to this AST node into a reference to its operation
    /// and a slice of its children.
    ///
    /// Se also [`AstNode::into_parts`].
    #[must_use]
    pub fn as_parts(&self) -> (&Op, &[T]) {
        (&self.operation, &self.children)
    }

    /// Convert this AST node into its operation and a [`Vec`] of its
    /// children.
    ///
    /// See also [`AstNode::from_parts`].
    #[must_use]
    pub fn into_parts(self) -> (Op, Vec<T>) {
        (self.operation, self.children)
    }
}

impl<Op: Arity, T> AstNode<Op, T> {
    /// Create an AST node with the given operation and children.
    ///
    /// See also [`AstNode::into_parts`].
    ///
    /// # Panics
    ///
    /// Panics if the [`arity`](Arity::arity) of the operation does not match
    /// the number of children.
    #[must_use]
    pub fn from_parts<I>(operation: Op, children: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let children = Vec::from_iter(children);
        assert_eq!(operation.arity(), children.len());
        Self {
            operation,
            children,
        }
    }
}

impl<Op, T> AsRef<[T]> for AstNode<Op, T> {
    fn as_ref(&self) -> &[T] {
        &self.children
    }
}

impl<Op, T> AsMut<[T]> for AstNode<Op, T> {
    fn as_mut(&mut self) -> &mut [T] {
        &mut self.children
    }
}

impl<'a, Op, T: Copy> IntoIterator for &'a AstNode<Op, T> {
    type Item = T;

    type IntoIter = iter::Copied<slice::Iter<'a, T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.children.iter().copied()
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
        self.as_ref()
    }

    // We also implement most of the default methods for better performance.

    fn children_mut(&mut self) -> &mut [Id] {
        self.as_mut()
    }

    fn for_each<F: FnMut(Id)>(&self, f: F) {
        self.iter().for_each(f);
    }

    fn for_each_mut<F: FnMut(&mut Id)>(&mut self, f: F) {
        self.iter_mut().for_each(f);
    }

    fn try_for_each<E, F>(&self, f: F) -> Result<(), E>
    where
        F: FnMut(Id) -> Result<(), E>,
        E: Clone,
    {
        self.iter().try_for_each(f)
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
        self.iter().fold(init, f)
    }

    fn all<F>(&self, f: F) -> bool
    where
        F: FnMut(Id) -> bool,
    {
        self.iter().all(f)
    }

    fn any<F>(&self, f: F) -> bool
    where
        F: FnMut(Id) -> bool,
    {
        self.iter().any(f)
    }
}

/// An error while attempting to parse an expression from a string.
#[derive(Debug, Error)]
pub enum ParseError<Op>
where
    Op: FromStr + Debug,
    <Op as FromStr>::Err: Error,
{
    /// An error while attempting to parse an operator.
    #[error(transparent)]
    ParseError(<Op as FromStr>::Err),

    /// An operator was applied to the wrong number of arguments.
    #[error("wrong number of arguments: expected {expected} but received {actual}")]
    ArityError {
        /// The expected number of arguments.
        expected: usize,
        /// The given number of arguments.
        actual: usize,
    },
}

impl<Op> FromOp for AstNode<Op>
where
    Op: Arity + FromStr + Debug + Clone + Ord + Hash + 'static,
    <Op as FromStr>::Err: Error,
{
    type Error = ParseError<Op>;

    fn from_op(op: &str, children: Vec<Id>) -> Result<Self, Self::Error> {
        let kind: Op = op.parse().map_err(ParseError::ParseError)?;
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

/// The [`Display`] implementation must match what [`egg`] is expecting. For
/// that reason, we only bother to implement it when the children have type
/// [`Id`].
impl<Op: Display> Display for AstNode<Op> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.operation.fmt(f)
    }
}
