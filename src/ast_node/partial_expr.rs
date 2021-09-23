use super::{AstNode, Expr};
use egg::{ENodeOrVar, Id, Language, Pattern, RecExpr, Var};
use std::{
    convert::{TryFrom, TryInto},
    error::Error,
    fmt::{self, Debug, Display, Formatter},
};

/// A partial expression. This is a generalization of an abstract syntax tree
/// where subexpressions can be replaced by "holes", i.e., values of type `T`.
/// The type [`Expr<Op>`] is isomorphic to `PartialExpr<Op, !>`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PartialExpr<Op, T> {
    /// A node in the abstract syntax tree.
    Node(AstNode<Op, Self>),
    /// A hole containing a value of type `T`.
    Hole(T),
}

impl<Op, T> PartialExpr<Op, T> {
    /// Is this partial expression a node?
    #[must_use]
    pub fn is_node(&self) -> bool {
        matches!(self, Self::Node(_))
    }

    /// Is this partial expression a hole?
    #[must_use]
    pub fn is_hole(&self) -> bool {
        matches!(self, Self::Hole(_))
    }

    /// Unwraps the [`Node`](Self::Node) `self` to produce the underlying
    /// [`AstNode`]. If `self` is a hole, produces [`None`].
    #[must_use]
    pub fn node(self) -> Option<AstNode<Op, Self>> {
        match self {
            PartialExpr::Node(node) => Some(node),
            PartialExpr::Hole(_) => None,
        }
    }

    /// Unwraps the [`Hole`](Self::Hole) `self` to produce the underlying value.
    /// if `self` is an AST node, produces [`None`].
    #[must_use]
    pub fn hole(self) -> Option<T> {
        match self {
            PartialExpr::Hole(hole) => Some(hole),
            PartialExpr::Node(_) => None,
        }
    }

    /// Returns `true` if `self` is a complete expression containing no holes.
    #[must_use]
    pub fn has_holes(&self) -> bool {
        match self {
            PartialExpr::Node(node) => node.iter().any(Self::has_holes),
            PartialExpr::Hole(_) => true,
        }
    }

    /// Replaces the holes in a partial expression of type `PartialExpr<Op, T>`
    /// with partial expressions of type `PartialExpr<Op, U>` to produce a new
    /// partial expression of type `PartialExpr<Op, U>`. Each hole's replacement
    /// partial expression is determined by applying a function to its value.
    #[must_use]
    pub fn fill<U, F>(self, mut f: F) -> PartialExpr<Op, U>
    where
        F: FnMut(T) -> PartialExpr<Op, U>,
    {
        self.fill_mut(&mut f)
    }

    /// Helper for [`Self::fill`] which takes its closure by mutable reference.
    #[must_use]
    fn fill_mut<U, F>(self, f: &mut F) -> PartialExpr<Op, U>
    where
        F: FnMut(T) -> PartialExpr<Op, U>,
    {
        match self {
            PartialExpr::Node(node) => {
                let node = node.map(|child| child.fill_mut(f));
                PartialExpr::Node(node)
            }
            PartialExpr::Hole(hole) => f(hole),
        }
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

/// An error which can be returned when attempting to convert a [`PartialExpr`]
/// to an [`Expr`], indicating that the partial expression is incomplete.
#[derive(Debug, Clone)]
pub struct IncompleteExprError<T> {
    /// The hole encountered.
    hole: T,
}

impl<T: Debug> Error for IncompleteExprError<T> {}
impl<T: Debug> Display for IncompleteExprError<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "expected expression but found hole {:?}", self.hole)
    }
}

impl<Op, T> TryFrom<PartialExpr<Op, T>> for Expr<Op> {
    type Error = IncompleteExprError<T>;

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
            PartialExpr::Hole(hole) => Err(IncompleteExprError { hole }),
        }
    }
}

impl<Op, T> TryFrom<PartialExpr<Op, T>> for AstNode<Op, PartialExpr<Op, T>> {
    type Error = IncompleteExprError<T>;

    fn try_from(partial_expr: PartialExpr<Op, T>) -> Result<Self, Self::Error> {
        match partial_expr {
            PartialExpr::Node(node) => Ok(node),
            PartialExpr::Hole(hole) => Err(IncompleteExprError { hole }),
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
