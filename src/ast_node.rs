//! Abstract syntax trees.
use egg::{FromOp, Id, Language};
use std::{convert::Infallible, fmt::Debug, hash::Hash, str::FromStr};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstNode<Op> {
    operation: Op,
    args: Vec<Id>,
}

/// A trait for operations which take a specific number of arguments.
pub trait Arity {
    /// Returns the minimum number of arguments the operation can take.
    fn min_arity(&self) -> usize;

    /// Returns the maximum number of arguments the operation can take, or
    /// [`None`] if there is no maximum.
    fn max_arity(&self) -> Option<usize> {
        Some(self.min_arity())
    }

    /// Returns `true` if the operation can take the given number of arguments.
    fn has_arity(&self, num_args: usize) -> bool {
        num_args >= self.min_arity() && self.max_arity().map_or(true, |max| num_args <= max)
    }
}

impl<Op: Arity> AstNode<Op> {
    /// Creates a new node with the provided operation and arguments.
    ///
    /// # Errors
    ///
    /// Returns an error if the [`Arity`] of the operation doesn't match the
    /// number of arguments.
    pub fn try_new<I>(operation: Op, args: I) -> Result<Self, ()>
    where
        I: IntoIterator<Item = Id>,
    {
        let args: Vec<_> = args.into_iter().collect();
        if operation.has_arity(args.len()) {
            Ok(Self { operation, args })
        } else {
            Err(())
        }
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
        &self.args
    }

    fn children_mut(&mut self) -> &mut [Id] {
        &mut self.args
    }
}

impl<Op> FromOp for AstNode<Op>
where
    Op: Debug + Arity + FromStr + Clone + Ord + Hash + 'static,
{
    type Error = Infallible;

    fn from_op(operation: &str, args: Vec<Id>) -> Result<Self, Self::Error> {
        if let Ok(operation) = operation.parse() {
            Ok(Self::try_new(operation, args).unwrap_or_else(|_| unreachable!()))
        } else {
            unreachable!()
        }
    }
}
