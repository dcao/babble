//! Defines the [`Eval`] trait for evaluating expressions.

use crate::ast_node::AstNode;
use egg::{Id, Language, RecExpr};
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
        M: Index<T, Output = AstNode<Self::Op, T>>;
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
) -> Result<Context::Value, Context::Error>
where
    AstNode<Context::Op>: Language,
{
    let root = expr.as_ref().last().unwrap();
    context.eval_node(root, expr)
}
