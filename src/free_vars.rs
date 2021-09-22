//! Defines an [`Analysis`] tracking free variables.

use crate::ast_node::AstNode;
use egg::{Analysis, Condition, EClass, EGraph, Id, Language, Subst, Symbol};
use std::{cmp::Ordering, collections::HashSet, marker::PhantomData};

/// Analysis which maintains a set of potentially free variables for each
/// eclass. For example, the set of potentially free variables for an eclass
/// representing the expressions `(app f circle)`, `(scale 1 x)`, and `line`
/// will be `{f, x}`.
#[derive(Clone, Copy, Debug)]
pub struct FreeVarAnalysis<Op> {
    phantom: PhantomData<*const Op>,
}

impl<Op> FreeVarAnalysis<Op> {
    /// Create a new free-variable analysis.
    #[must_use]
    pub fn new() -> Self {
        Self {
            phantom: PhantomData,
        }
    }
}

impl<Op> Default for FreeVarAnalysis<Op> {
    fn default() -> Self {
        Self::new()
    }
}

/// A trait for getting the free variables in an expression.
pub trait FreeVars: Sized {
    /// If `self` is an identifier, return its name. Otherwise, return `None`.
    fn ident_symbol(&self) -> Option<Symbol>;

    /// Get the free variables of an expression, given the free variables in
    /// that expression's subexpressions.
    fn free_vars(&self, children: &[&HashSet<Symbol>]) -> HashSet<Symbol>;
}

impl<Op> Analysis<AstNode<Op>> for FreeVarAnalysis<Op>
where
    Op: FreeVars,
    AstNode<Op>: Language,
{
    type Data = HashSet<Symbol>;

    /// Set `a` to the union of `a` and `b`.
    fn merge(&self, a: &mut Self::Data, b: Self::Data) -> Option<Ordering> {
        if a.is_subset(&b) {
            if a.is_superset(&b) {
                Some(Ordering::Equal)
            } else {
                *a = b;
                Some(Ordering::Less)
            }
        } else if a.is_superset(&b) {
            Some(Ordering::Greater)
        } else {
            a.extend(b.into_iter());
            None
        }
    }

    /// Return all variables potentially free in `enode`.
    fn make(egraph: &EGraph<AstNode<Op>, Self>, enode: &AstNode<Op>) -> Self::Data {
        let children: Vec<_> = enode.iter().map(|&id| &egraph[id].data).collect();
        enode.operation().free_vars(&children)
    }
}

/// Produces a condition which is true if and only if the conditions `p` and
/// `q` are both true. If `p` is false, this condition short-circuits and does
/// not check `q`.
#[must_use]
pub fn and<L, A, P, Q>(p: P, q: Q) -> impl Condition<L, A>
where
    L: Language,
    A: Analysis<L>,
    P: Condition<L, A>,
    Q: Condition<L, A>,
{
    move |egraph: &mut EGraph<L, A>, id: Id, subst: &Subst| {
        p.check(egraph, id, subst) && q.check(egraph, id, subst)
    }
}

/// Produces a [`Condition`] which is true if and only if the variable matched
/// by `var` is not potentially free in the expression matched by `expr`. Both
/// `expr` and `var` must be pattern variables (e.g. "?e" and "?x").
///
/// # Panics
///
/// Panics if `expr` or `var` is not a valid [`Pattern`](egg::Pattern).
///
/// The resulting condition panics if `var` matches anything other than an
/// identifier.
#[must_use]
pub fn not_free_in<Op>(
    expr: &'static str,
    var: &'static str,
) -> impl Condition<AstNode<Op>, FreeVarAnalysis<Op>>
where
    Op: FreeVars,
    AstNode<Op>: Language,
{
    fn ident_symbol<Op: FreeVars, D>(eclass: &EClass<AstNode<Op>, D>) -> Option<Symbol> {
        if eclass.nodes.len() == 1 {
            let var_sym = eclass.nodes[0].operation().ident_symbol()?;
            Some(var_sym)
        } else {
            None
        }
    }

    let expr_metavar = expr.parse().unwrap();
    let var_metavar = var.parse().unwrap();
    move |egraph: &mut EGraph<AstNode<Op>, FreeVarAnalysis<Op>>, _, subst: &Subst| {
        let var_eclass = &egraph[subst[var_metavar]];
        let var_sym = ident_symbol(var_eclass).expect("not a variable");
        let free_vars = &egraph[subst[expr_metavar]].data;
        !free_vars.contains(&var_sym)
    }
}
