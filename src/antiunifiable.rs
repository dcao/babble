//! Defines the [`Antiunifiable`] trait for languages that can be antiunified.

use std::{fmt::Debug, hash::Hash};

use crate::ast_node::{Arity, AstNode};
use egg::{Language, Symbol};

/// A trait for languages that contain the constructs needed to introduce
/// learned library functions.
///
/// Rather than being implemented on a [`Language`] directly, this should be
/// implemented by the type of operations `Op` in that language, such that
/// [`AstNode<Op>`] implements [`Language`].
pub trait Antiunifiable: Arity + Debug + Clone + Ord + Hash + Send + Sync + 'static
where
    // This bound should always be satisfied, but its meaning is clearer than
    // all the supertraits.
    AstNode<Self>: Language,
{
    /// Return an AST node representing a de Bruijn-indexed lambda with body
    /// `body`.
    fn lambda<T>(body: T) -> AstNode<Self, T>;

    /// Return an AST node representing an application of the function `fun` to
    /// an expression `arg`.
    fn apply<T>(fun: T, arg: T) -> AstNode<Self, T>;

    /// Return an AST node representing a de Bruijn-indexed variable.
    fn var<T>(index: usize) -> AstNode<Self, T>;

    /// Return an AST node representing a named identifier.
    fn ident<T>(name: Symbol) -> AstNode<Self, T>;

    /// Return an AST node representing a let-expression defining a new learned
    /// library function named `name` with definition `fun` within an expression
    /// `body`.
    fn lib<T>(name: T, fun: T, body: T) -> AstNode<Self, T>;
}
