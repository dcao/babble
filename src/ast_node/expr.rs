use super::{Arity, AstNode};
use egg::{Id, RecExpr};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Expr<Op>(AstNode<Op, Self>);

impl<Op> Expr<Op> {
    #[must_use]
    pub fn into_inner(self) -> AstNode<Op, Self> {
        self.0
    }
}

impl<Op: Arity> Expr<Op> {
    #[must_use]
    pub fn new<I>(operation: Op, children: I) -> Self
    where
        I: IntoIterator<Item = Self>,
    {
        let children: Vec<_> = children.into_iter().collect();
        Self(AstNode::new(operation, children))
    }

    #[must_use]
    pub fn new_leaf(leaf: Op) -> Self {
        Self(AstNode::new_leaf(leaf))
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
                children: child_ids,
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
