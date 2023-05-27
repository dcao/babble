//! Defines the [`Teachable`] trait for languages that support library learning.

use crate::{ast_node::AstNode, learn::LibId};
use std::{
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    num::ParseIntError,
    ops::{Deref, DerefMut},
    str::FromStr,
};
use thiserror::Error;

/// A trait for languages which support library learning.
pub trait Teachable
where
    Self: Sized,
{
    /// Converts a [`BindingExpr`] into an [`AstNode`] in the language.
    #[must_use]
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T>;

    /// Attempts to convert a reference to an [`AstNode`] in the language into a
    /// [`BindingExpr`] which references the node's children, returning [`None`]
    /// if the AST node does not correspond to a [`BindingExpr`].
    #[must_use]
    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>>;

    /// Returns the equivalent of a "list" operation in the language, used internally
    /// to combine multiple expressions when reporting lib learning results.
    #[must_use]
    fn list() -> Self;

    /// Creates an AST node representing a de Bruijn-indexed lambda with body `body`.
    #[must_use]
    fn lambda<T>(body: T) -> AstNode<Self, T> {
        Self::from_binding_expr(BindingExpr::Lambda(body))
    }

    /// Creates an AST node representing an application of the function `fun` to
    /// an argument `arg`.
    #[must_use]
    fn apply<T>(fun: T, arg: T) -> AstNode<Self, T> {
        Self::from_binding_expr(BindingExpr::Apply(fun, arg))
    }

    /// Creates a de Bruijn-indexed variable.
    #[must_use]
    fn var<T>(index: usize) -> AstNode<Self, T> {
        Self::from_binding_expr(BindingExpr::Var(DeBruijnIndex(index)))
    }

    /// Creates an expression defining the library function `name` as `value` in `body`.
    #[must_use]
    fn lib<T>(name: LibId, value: T, body: T) -> AstNode<Self, T> {
        Self::from_binding_expr(BindingExpr::Lib(name, value, body))
    }

    /// Creates a named variable referencing a library function.
    #[must_use]
    fn lib_var<T>(name: LibId) -> AstNode<Self, T> {
        Self::from_binding_expr(BindingExpr::LibVar(name))
    }
}

/// A simplified language containing just the constructs necessary for library
/// learning: functions, applications, let-expressions, and both named and de
/// Bruijn-indexed variables.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BindingExpr<T> {
    /// A de Bruijn index
    Var(DeBruijnIndex),
    /// A reference to a named library function
    LibVar(LibId),
    /// A lambda
    Lambda(T),
    /// An application of a function to an argument
    Apply(T, T),
    /// An expression defining a named library function within a certain scope
    Lib(LibId, T, T),
}

impl<Op, T> From<BindingExpr<T>> for AstNode<Op, T>
where
    Op: Teachable,
{
    fn from(binding_expr: BindingExpr<T>) -> Self {
        Op::from_binding_expr(binding_expr)
    }
}

impl<Op, T> AstNode<Op, T>
where
    Op: Teachable,
{
    /// Attempts to convert a reference to the AST node into the corresponding
    /// [`BindingExpr`] referencing the node's children, returning [`None`] if
    /// there is no corresponding construct.
    pub fn as_binding_expr(&self) -> Option<BindingExpr<&T>> {
        Op::as_binding_expr(self)
    }
}

/// A newtype wrapper for [`usize`] representing a de Bruijn index. The string
/// representation of the de Bruijn index is a dollar sign ($) followed by the
/// integer index, (e.g. `$12`) and its [`Debug`], [`Display`], and [`FromStr`]
/// implementations reflect that.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DeBruijnIndex(pub usize);

impl Deref for DeBruijnIndex {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for DeBruijnIndex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Debug for DeBruijnIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        <Self as Display>::fmt(self, f)
    }
}

impl Display for DeBruijnIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl From<usize> for DeBruijnIndex {
    fn from(index: usize) -> Self {
        Self(index)
    }
}

impl From<DeBruijnIndex> for usize {
    fn from(index: DeBruijnIndex) -> Self {
        index.0
    }
}

/// An error when parsing a de Bruijn index.
#[derive(Clone, Debug, Error)]
pub enum ParseDeBruijnIndexError {
    /// The string did not start with "$"
    #[error("expected de Bruijn index to start with '$")]
    NoLeadingDollar,
    /// The index is not a valid unsigned integer
    #[error(transparent)]
    InvalidIndex(ParseIntError),
}

impl FromStr for DeBruijnIndex {
    type Err = ParseDeBruijnIndexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(n) = s.strip_prefix('$') {
            let n = n.parse().map_err(ParseDeBruijnIndexError::InvalidIndex)?;
            Ok(DeBruijnIndex(n))
        } else {
            Err(ParseDeBruijnIndexError::NoLeadingDollar)
        }
    }
}
