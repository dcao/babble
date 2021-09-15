//! Defines the [`Eval`] trait for evaluating expressions.

use crate::ast_node::AstNode;
use egg::{Id, RecExpr};
use std::ops::Index;

/// A trait for evaluating expressions. This should be implemented by a type
/// representing an evaluation context (e.g. a map from identifiers to values).
pub trait Eval<T = Id>: Sized {
    /// The operations in the language being evaluated.
    type Op;
    /// The result of evaluation.
    type Value;
    /// An error encountered while attempting to evaluate an expression.
    type Error;

    /// Evaluate the AST node `node`. Generally this method will be recursive on
    /// the node's children, which can be looked up in the provided `node_map`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the node can't be evaluated for some reason (e.g. a
    /// type error).
    fn eval_node<M>(
        &self,
        node: &AstNode<Self::Op, T>,
        node_map: &M,
    ) -> Result<Self::Value, Self::Error>
    where
        M: for<'a> Index<&'a T, Output = AstNode<Self::Op, T>>;
}

/// Evaluate the expression `expr` in the context `context`.
///
/// # Errors
///
/// Returns `Err` if the expression can't be evaluated for some reason, such as
/// a type error.
///
/// # Panics
///
/// Panics if `expr` is empty.
pub fn eval<Context: Eval>(
    context: &Context,
    expr: &RecExpr<AstNode<Context::Op>>,
) -> Result<Context::Value, Context::Error> {
    let root = expr.as_ref().last().unwrap();
    let node_map = NodeMap(expr);
    context.eval_node(root, &node_map)
}

/// A wrapper around [`RecExpr`] which allows indexing by [`&Id`].
struct NodeMap<'a, Op>(&'a RecExpr<AstNode<Op>>);

impl<'a, 'k, Op> Index<&'k Id> for NodeMap<'a, Op> {
    type Output = AstNode<Op>;

    fn index(&self, index: &'k Id) -> &Self::Output {
        &self.0.as_ref()[usize::from(*index)]
    }
}
