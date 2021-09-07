//! Defines the [`Antiunification`] type representing expressions containing
//! variables.

use crate::{
    antiunifiable::Antiunifiable,
    ast_node::{Arity, AstNode},
};
use egg::{ENodeOrVar, Id, Pattern, RecExpr, Symbol, Var};
use std::{
    fmt::Debug,
    hash::Hash,
    ops::{Add, AddAssign},
};

type State = (Id, Id);

/// A newtype wrapper around [`usize`] indicating that the value represents an
/// index into a slice. This serves both to distinguish indices from [`Id`]s and
/// to allow us to implement addition for [`AstNode<Op, Index>`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Index(pub usize);

impl From<usize> for Index {
    fn from(index: usize) -> Self {
        Index(index)
    }
}

impl From<Index> for usize {
    fn from(index: Index) -> Self {
        index.0
    }
}

impl From<Id> for Index {
    fn from(id: Id) -> Self {
        Index(id.into())
    }
}

impl From<Index> for Id {
    fn from(index: Index) -> Self {
        index.0.into()
    }
}

impl AddAssign<usize> for Index {
    fn add_assign(&mut self, offset: usize) {
        self.0 += offset;
    }
}

impl Add<usize> for Index {
    type Output = Self;

    fn add(mut self, offset: usize) -> Self::Output {
        self += offset;
        self
    }
}

impl<Op> AddAssign<usize> for AstNode<Op, Index> {
    fn add_assign(&mut self, offset: usize) {
        for child in self {
            *child += offset;
        }
    }
}

/// Convert a [`Dfta`] state (a pair of [`Id`]s) into a pattern variable.
#[must_use]
pub fn state_to_var(state: State) -> Var {
    format!("?x_{}_{}", state.0, state.1)
        .parse()
        .unwrap_or_else(|_| unreachable!())
}

/// Either an [`AstNode`] with operation of type `Op` and children of type `T`, or
/// a named metavariable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AstNodeOrVar<Op, T> {
    /// An AST node.
    AstNode(AstNode<Op, T>),
    /// A metavariable.
    Var(State),
}

impl<Op, T> AstNodeOrVar<Op, T> {
    /// Is this an AST node?
    #[must_use]
    pub fn is_ast_node(&self) -> bool {
        matches!(self, Self::AstNode(_))
    }

    /// Is this a metavariable?
    #[must_use]
    pub fn is_metavar(&self) -> bool {
        matches!(self, Self::Var(_))
    }
}

impl<Op, T> From<AstNode<Op, T>> for AstNodeOrVar<Op, T> {
    fn from(ast_node: AstNode<Op, T>) -> Self {
        Self::AstNode(ast_node)
    }
}

impl<Op, T> From<State> for AstNodeOrVar<Op, T> {
    fn from(state: State) -> Self {
        Self::Var(state)
    }
}

impl<Op> AddAssign<usize> for AstNodeOrVar<Op, Index> {
    fn add_assign(&mut self, offset: usize) {
        if let Self::AstNode(ast_node) = self {
            *ast_node += offset;
        }
    }
}

impl<Op> Add<usize> for AstNodeOrVar<Op, Index> {
    type Output = Self;

    fn add(mut self, offset: usize) -> Self::Output {
        self += offset;
        self
    }
}

/// An `Op` antiunification, i.e., an expression with operations of type `Op`
/// with some subexpressions replaced by metavariables.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Antiunification<Op> {
    /// The expression, represented as a list of metavariables and [`AstNode`]s
    /// whose children are the indices of nodes that precede them in the list.
    /// The last element in this list is taken to be the root node of the
    /// expression.
    ///
    /// For example, the expression `(concat "a" (concat "b" "c"))` might be
    /// represented as the list
    ///
    /// ```text
    /// ["c", "b", (concat 1 0), "a", (concat 3 2)]
    /// ```
    ///
    /// This representation is not unique: nodes may be duplicated or in a
    /// different order, and not all nodes in the list necessarily appear in the
    /// expression.
    expr: Vec<AstNodeOrVar<Op, Index>>,

    /// A sorted list of the metavariables that appear in this antiunification.
    metavars: Vec<State>,
}

impl<Op: Arity> Antiunification<Op> {
    /// Construct an antiunification consisting of a single leaf AST node.
    ///
    /// # Panics
    ///
    /// Panics if the [`arity`] of `leaf` is not zero.
    #[must_use]
    pub fn leaf(leaf: Op) -> Self {
        Self {
            expr: vec![AstNode::from_parts(leaf, []).into()],
            metavars: Vec::new(),
        }
    }

    /// Create an antiunification representing an AST node whose children are
    /// themselves antiunifications.
    ///
    /// # Panics
    ///
    /// Panics if the [`arity`] of `operation` does not match the number of
    /// children.
    #[must_use]
    pub fn expr<I>(operation: Op, children: I) -> Self
    where
        I: IntoIterator<Item = Self>,
    {
        let mut children = children.into_iter();
        if let Some(child) = children.next() {
            let mut indices = vec![child.index()];
            let mut result = child;
            for child in children {
                let index = result.append(child);
                indices.push(index);
            }
            result.push_ast_node(AstNode::from_parts(operation, indices));
            result
        } else {
            Self::leaf(operation)
        }
    }
}

impl<Op> Antiunification<Op> {
    /// Construct an antiunification consisting of a single metavariable
    /// representing the state `state`.
    #[must_use]
    pub fn metavar(state: State) -> Self {
        Self {
            expr: vec![state.into()],
            metavars: vec![state],
        }
    }

    /// A trivial anti-unification is one that either has no metavariables or is
    /// only metavariables.
    #[must_use]
    pub fn is_trivial(&self) -> bool {
        self.metavars.is_empty() || self.expr.iter().all(AstNodeOrVar::is_metavar)
    }

    /// Get the index of the last AST node or metavariable added to this
    /// antiunification.
    #[must_use]
    pub fn index(&self) -> Index {
        // It shouldn't be possible to construct an empty antiunification.
        debug_assert!(!self.expr.is_empty());
        Index(self.expr.len() - 1)
    }

    /// Append two antiunifications, and return the index of the last AST node
    /// or metavariable added from `other`.
    ///
    /// # Note
    ///
    /// Appending changes this antiunification so that it represents the same
    /// expression as `other`. To actually use both antiunifications, you should
    /// then add an [`AstNode`] using the index returned by this method and the
    /// previous value of `self.index()`.
    pub fn append(&mut self, other: Self) -> Index {
        let other = other + self.expr.len();
        for var in other.metavars {
            self.add_metavar(var);
        }
        self.expr.extend(other.expr);
        self.index()
    }

    /// Add an AST node to this antiunification and return its index.
    ///
    /// # Panics
    ///
    /// Panics if any of the children of `ast_node` has an index greater than
    /// the index of the last thing added to this antiunification.
    pub fn push_ast_node(&mut self, ast_node: AstNode<Op, Index>) -> Index {
        let max_index = self.index();
        assert!(ast_node.iter().all(|index| *index <= max_index));
        self.expr.push(AstNodeOrVar::AstNode(ast_node));
        self.index()
    }

    /// Add a metavariable representing `state` to this antiunification and
    /// return its index.
    pub fn push_metavar(&mut self, state: State) -> Index {
        self.expr.push(AstNodeOrVar::Var(state));
        self.add_metavar(state);
        self.index()
    }

    /// Add `state` to the list of metavariables if it isn't already present.
    fn add_metavar(&mut self, state: State) {
        if let Err(index) = self.metavars.binary_search(&state) {
            self.metavars.insert(index, state);
        }
    }
}

impl<Op: Antiunifiable> Antiunification<Op> {
    /// Create a new anti-unification from this one by introducing a named
    /// library function and applying it to each of the metavariables.
    ///
    /// This transforms an anti-unification like `(+ ?X ?Y)` into the
    /// anti-unification
    /// ```text
    /// (lib f0 (lambda (lambda (+ $0 $1)))
    ///   (apply (apply f0 ?Y) ?X))
    /// ```
    #[must_use]
    pub fn into_library_fun(mut self, name: Symbol) -> Self {
        // We start by replacing every metavariable in this antiunification with
        // a de Bruijn-indexed variable whose index is the position of the
        // metavariable in `metavars`.
        for node in &mut self.expr {
            if let AstNodeOrVar::Var(state) = node {
                let index = self
                    .metavars
                    .binary_search(state)
                    .unwrap_or_else(|_| unreachable!());
                *node = AstNodeOrVar::AstNode(Op::var(index));
            }
        }

        let mut fun_index = self.index();
        // Now wrap this antiunification in lambdas, one for each metavariable.
        for _ in 0..self.metavars.len() {
            fun_index = self.push_ast_node(Op::lambda(fun_index));
        }

        // We generate a name for the library function.
        let ident_index = self.push_ast_node(Op::ident(name));

        // We apply that named library function to each of the metavariables, in
        // reverse order of their de Bruijn indices.
        let mut body_index = ident_index;
        let mut reversed_metavars = self.metavars.clone();
        reversed_metavars.reverse();
        for state in reversed_metavars {
            let arg_index = self.push_metavar(state);
            body_index = self.push_ast_node(Op::apply(body_index, arg_index));
        }

        // Finally, we generate a lib-binding.
        self.push_ast_node(Op::lib(ident_index, fun_index, body_index));
        self
    }
}

impl<Op> From<Antiunification<Op>> for Pattern<AstNode<Op>>
where
    Op: Debug + Clone + Ord + Hash,
{
    fn from(antiunification: Antiunification<Op>) -> Self {
        let pattern: Vec<_> = antiunification
            .expr
            .into_iter()
            .map(|node| match node {
                AstNodeOrVar::AstNode(ast_node) => ENodeOrVar::ENode(ast_node.map(Id::from)),
                AstNodeOrVar::Var(state) => ENodeOrVar::Var(state_to_var(state)),
            })
            .collect();
        RecExpr::from(pattern).into()
    }
}

impl<Op> AddAssign<usize> for Antiunification<Op> {
    fn add_assign(&mut self, offset: usize) {
        for node in &mut self.expr {
            *node += offset;
        }
    }
}

impl<Op> Add<usize> for Antiunification<Op> {
    type Output = Self;

    fn add(mut self, offset: usize) -> Self::Output {
        self += offset;
        self
    }
}
