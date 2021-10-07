use super::{Arity, AstNode, ParseNodeError};
use crate::sexp::Sexp;
use egg::{Id, RecExpr};
use std::{convert::TryFrom, str::FromStr};

/// An abstract syntax tree with operations `Op`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Expr<Op>(pub AstNode<Op, Self>);

impl<Op> Expr<Op> {
    /// Converts `self` into its underlying [`AstNode`].
    #[must_use]
    pub fn into_inner(self) -> AstNode<Op, Self> {
        self.0
    }
}

impl<'a, Op: FromStr + Arity> TryFrom<Sexp<'a>> for Expr<Op> {
    type Error = ParseNodeError<Op, Expr<Op>, <Op as FromStr>::Err>;

    fn try_from(sexp: Sexp<'a>) -> Result<Self, Self::Error> {
        let (op, args) = match sexp {
            Sexp::Atom(atom) => (atom, Vec::new()),
            Sexp::List(op, args) => (op, args),
        };
        let op: Op = op.parse().map_err(ParseNodeError::ParseError)?;
        let args = args
            .into_iter()
            .map(Self::try_from)
            .collect::<Result<Vec<_>, _>>()?;
        let node = AstNode::try_new(op, args).map_err(ParseNodeError::ArityError)?;
        Ok(Self(node))
    }
}

impl<Op> From<Expr<Op>> for RecExpr<AstNode<Op>> {
    fn from(expr: Expr<Op>) -> Self {
        fn build<Op>(rec_expr: &mut Vec<AstNode<Op>>, expr: Expr<Op>) {
            let (operation, children) = expr.0.into_parts();
            let mut child_ids = Vec::with_capacity(children.len());
            for child in children {
                build(rec_expr, child);
                child_ids.push(Id::from(rec_expr.len() - 1));
            }
            rec_expr.push(AstNode {
                operation,
                args: child_ids,
            });
        }

        let mut rec_expr = Vec::new();
        build(&mut rec_expr, expr);
        rec_expr.into()
    }
}

impl<Op: Clone> From<RecExpr<AstNode<Op>>> for Expr<Op> {
    fn from(rec_expr: RecExpr<AstNode<Op>>) -> Self {
        fn build<Op: Clone>(rec_expr: &[AstNode<Op>]) -> Expr<Op> {
            let node = (*rec_expr.last().unwrap()).clone();
            let node = node.map(|id| {
                let child_index = usize::from(id);
                build(&rec_expr[..=child_index])
            });
            Self(node)
        }

        build(rec_expr.as_ref())
    }
}

impl<Op> From<AstNode<Op, Self>> for Expr<Op> {
    fn from(node: AstNode<Op, Self>) -> Self {
        Self(node)
    }
}

impl<Op> From<Expr<Op>> for AstNode<Op, Expr<Op>> {
    fn from(expr: Expr<Op>) -> Self {
        expr.0
    }
}
