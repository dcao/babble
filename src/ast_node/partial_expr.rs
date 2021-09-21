use super::{Arity, AstNode, Expr};
use egg::{ENodeOrVar, Id, Language, Pattern, RecExpr, Var};
use std::{
    cell::RefCell,
    convert::{TryFrom, TryInto},
    error::Error,
    fmt::{self, Debug, Display, Formatter},
};

/// An expression with holes. This is essentially an [`Expr`], but with some of
/// the nodes replaced by "holes" of type `T`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PartialExpr<Op, T> {
    Node(AstNode<Op, Self>),
    Hole(T),
}

impl<Op, T> PartialExpr<Op, T> {
    #[must_use]
    pub fn is_node(&self) -> bool {
        matches!(self, Self::Node(_))
    }

    #[must_use]
    pub fn is_hole(&self) -> bool {
        matches!(self, Self::Hole(_))
    }

    #[must_use]
    pub fn is_expr(&self) -> bool {
        match self {
            PartialExpr::Node(node) => node.iter().all(Self::is_expr),
            PartialExpr::Hole(_) => false,
        }
    }

    #[must_use]
    pub fn fill<U, F>(self, f: F) -> PartialExpr<Op, U>
    where
        F: FnMut(T) -> PartialExpr<Op, U>,
    {
        // TODO: There should be a way to write this without `RefCell`.
        fn do_fill<Op, T, U, F>(
            partial_expr: PartialExpr<Op, T>,
            f: &RefCell<F>,
        ) -> PartialExpr<Op, U>
        where
            F: FnMut(T) -> PartialExpr<Op, U>,
        {
            match partial_expr {
                PartialExpr::Node(node) => {
                    // Nested closures create ownership headaches.
                    let node = node.map(|child| do_fill(child, f));
                    PartialExpr::Node(node)
                }
                PartialExpr::Hole(contents) => f.borrow_mut()(contents),
            }
        }

        do_fill(self, &RefCell::new(f))
    }
}

impl<Op: Arity, T> PartialExpr<Op, T> {
    #[must_use]
    pub fn node<I>(operation: Op, children: I) -> Self
    where
        I: IntoIterator<Item = Self>,
    {
        let children: Vec<_> = children.into_iter().collect();
        Self::Node(AstNode::new(operation, children))
    }

    #[must_use]
    pub fn leaf(operation: Op) -> Self {
        Self::Node(AstNode::new_leaf(operation))
    }
}

impl<Op> From<PartialExpr<Op, Var>> for Pattern<AstNode<Op>>
where
    AstNode<Op>: Language,
{
    fn from(partial_expr: PartialExpr<Op, Var>) -> Self {
        fn build<Op>(
            pattern: &mut Vec<ENodeOrVar<AstNode<Op>>>,
            partial_expr: PartialExpr<Op, Var>,
        ) {
            match partial_expr {
                PartialExpr::Node(node) => {
                    let (operation, children) = node.into_parts();
                    let mut child_ids = Vec::with_capacity(children.len());
                    for child in children {
                        build(pattern, child);
                        child_ids.push(Id::from(pattern.len() - 1));
                    }
                    pattern.push(ENodeOrVar::ENode(AstNode {
                        operation,
                        children: child_ids,
                    }));
                }
                PartialExpr::Hole(contents) => pattern.push(ENodeOrVar::Var(contents)),
            }
        }

        let mut pattern = Vec::new();
        build(&mut pattern, partial_expr);
        RecExpr::from(pattern).into()
    }
}

impl<Op: Clone> From<Pattern<AstNode<Op>>> for PartialExpr<Op, Var> {
    fn from(pattern: Pattern<AstNode<Op>>) -> Self {
        fn build<Op: Clone>(pattern: &[ENodeOrVar<AstNode<Op>>]) -> PartialExpr<Op, Var> {
            match &pattern[pattern.len() - 1] {
                &ENodeOrVar::Var(var) => PartialExpr::Hole(var),
                ENodeOrVar::ENode(node) => {
                    let node = node.clone().map(|id| {
                        let child_index = usize::from(id);
                        build(&pattern[..=child_index])
                    });
                    PartialExpr::Node(node)
                }
            }
        }
        build(pattern.ast.as_ref())
    }
}

#[derive(Debug, Clone)]
pub struct UnexpectedHoleError<T>(T);

impl<T: Debug> Error for UnexpectedHoleError<T> {}
impl<T: Debug> Display for UnexpectedHoleError<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "found unexpected hole containing {:?}", self.0)
    }
}

impl<Op, T> TryFrom<PartialExpr<Op, T>> for Expr<Op> {
    type Error = UnexpectedHoleError<T>;

    fn try_from(partial_expr: PartialExpr<Op, T>) -> Result<Self, Self::Error> {
        match partial_expr {
            PartialExpr::Node(AstNode {
                operation,
                children,
            }) => {
                let mut new_children = Vec::with_capacity(children.len());
                for child in children {
                    let new_child = child.try_into()?;
                    new_children.push(new_child);
                }
                let node = AstNode {
                    operation,
                    children: new_children,
                };
                Ok(node.into())
            }
            PartialExpr::Hole(contents) => Err(UnexpectedHoleError(contents)),
        }
    }
}

impl<Op, T> TryFrom<PartialExpr<Op, T>> for AstNode<Op, PartialExpr<Op, T>> {
    type Error = UnexpectedHoleError<T>;

    fn try_from(partial_expr: PartialExpr<Op, T>) -> Result<Self, Self::Error> {
        match partial_expr {
            PartialExpr::Node(node) => Ok(node),
            PartialExpr::Hole(contents) => Err(UnexpectedHoleError(contents)),
        }
    }
}

impl<Op, T> From<AstNode<Op, Self>> for PartialExpr<Op, T> {
    fn from(node: AstNode<Op, Self>) -> Self {
        Self::Node(node)
    }
}

impl<Op, T> From<Expr<Op>> for PartialExpr<Op, T> {
    fn from(expr: Expr<Op>) -> Self {
        Self::Node(AstNode::from(expr).map(Self::from))
    }
}
