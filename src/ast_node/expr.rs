use super::{Arity, AstNode, ParseNodeError};
use crate::{sexp::Sexp, teachable::Teachable};
use egg::{Id, Language, RecExpr};
use serde::{Deserialize, Serialize};
use std::{convert::TryFrom, str::FromStr};

/// An abstract syntax tree with operations `Op`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Expr<Op>(pub AstNode<Op, Self>);

#[allow(clippy::len_without_is_empty)]
impl<Op> Expr<Op> {
    /// Converts `self` into its underlying [`AstNode`].
    #[must_use]
    pub fn into_inner(self) -> AstNode<Op, Self> {
        self.0
    }

    /// Returns the number of AST nodes in the expression. There is no
    /// corresponding `is_empty` method because `len` is always greater than
    /// zero.
    #[must_use]
    pub fn len(&self) -> usize {
        self.0.iter().map(Expr::len).sum::<usize>() + 1
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
            let (operation, args) = expr.0.into_parts();
            let mut arg_ids = Vec::with_capacity(args.len());
            for arg in args {
                build(rec_expr, arg);
                arg_ids.push(Id::from(rec_expr.len() - 1));
            }
            rec_expr.push(AstNode {
                operation,
                args: arg_ids,
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

/// Convert a list of exprs into a single recexpr, combining them using the list node
#[must_use]
pub fn combine_exprs<Op>(exprs: Vec<Expr<Op>>) -> RecExpr<AstNode<Op>>
where
    Op: Teachable + std::fmt::Debug + Clone + Arity + std::hash::Hash + Ord,
{
    let mut res: Vec<AstNode<Op>> = Vec::new();
    let mut roots: Vec<egg::Id> = Vec::new();

    for expr in exprs {
        // Turn the expr into a RecExpr
        let recx: RecExpr<_> = expr.into();

        // Then turn the RecExpr into a Vec
        let mut nodes: Vec<AstNode<Op>> = recx.as_ref().to_vec();

        // For each node, increment the children by the current size of the accum expr
        for node in &mut nodes {
            node.update_children(|x| (usize::from(x) + res.len()).into());
        }

        // Then push everything into the accum expr
        res.extend(nodes);
        roots.push((res.len() - 1).into());
    }

    // Add the root node
    res.push(AstNode::new(Op::list(), roots));

    // Turn res back into a recexpr!
    res.into()
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

impl<Op> AsRef<AstNode<Op, Self>> for Expr<Op> {
    fn as_ref(&self) -> &AstNode<Op, Self> {
        &self.0
    }
}
