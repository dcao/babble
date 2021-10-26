//! Rewrites which hoist let-expressions.

use crate::{
    ast_node::{Arity, AstNode},
    teachable::{BindingExpr, Teachable},
};
use egg::{
    Analysis, Applier, EGraph, Id, Language, PatternAst, Rewrite, SearchMatches, Searcher, Subst,
    Symbol, Var,
};
use lazy_static::lazy_static;
use std::{collections::HashMap, fmt::Debug};

lazy_static! {
    static ref BOUND_VALUE: Var = "?bound_value".parse().unwrap();
}

/// A `LiftLib` implements a rewrite which takes a let-expression within an
/// operation to a let-expression containing that operation. For example, a
/// `LiftLib` can be used to produce a rewrite
///
/// ```text
/// (+ (let ?ident ?value ?body) ?expr) => (let ?ident ?value (+ ?body ?expr))
/// ```
///
/// The rewrite is only applied if lifting the let-expression won't capture any
/// free variables. If an operation contains multiple let-expressions with the
/// same identifier and value, they are merged during lifting to produce a
/// single let-expression containing the operation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LiftLib<Op> {
    /// The operation to lift over.
    operation: Op,
}

impl<Op> LiftLib<Op> {
    /// Creates a new `LiftLib` lifting over the given operation. The resulting
    /// rewrite is only valid for operations which do not modify the
    /// environment, so it is unsuitable for lifting over e.g. let-expressions.
    #[must_use]
    pub fn new(operation: Op) -> Self {
        Self { operation }
    }

    fn lift(index: usize) -> Var {
        format!("?lift{}", index)
            .parse()
            .unwrap_or_else(|_| unreachable!())
    }

    fn shift(index: usize) -> Var {
        format!("?shift{}", index)
            .parse()
            .unwrap_or_else(|_| unreachable!())
    }
}

// (+ (lib 2 $0) (lib 2 $0)) => (lib 2 (+ $0 $0))
// (+ (lib 2 $0) $0) => (lib 2 (+ $0 $1))
// (+ (lib $0 $0) $1) => (lib $0 (+ $0 $2))
// (+ (lib 1 $0) (lib 2 $0)) => (lib 1 (+ $0 (lib 2 $0)))
// "                       " => (lib 2 (+ (lib 1 $0) $0))
// (+ (lib $1 $1) $0) => (lib $1 (+ $1 $1))

// 1 => 0
// 2 => 1
// (shift)
impl<Op> LiftLib<Op>
where
    Op: Teachable + Arity + Eq + Clone + Send + Sync + Debug + 'static,
    AstNode<Op>: Language,
{
    /// Creates a new [`Rewrite`] named `name` which lifts over the given
    /// operation. `LiftLib` is used as both the [`Searcher`] and [`Applier`] of
    /// the rewrite.
    #[must_use]
    pub fn rewrite<A>(name: &str, operation: Op) -> Rewrite<AstNode<Op>, A>
    where
        A: Analysis<AstNode<Op>>,
    {
        let lift_lib = Self::new(operation);
        Rewrite::new(name, lift_lib.clone(), lift_lib).unwrap_or_else(|_| unreachable!())
    }
}

impl<Op, A> Searcher<AstNode<Op>, A> for LiftLib<Op>
where
    Op: Debug + Teachable + Arity + Eq + Clone + Send + Sync + 'static,
    A: Analysis<AstNode<Op>>,
    AstNode<Op>: Language,
{
    fn search_eclass(
        &self,
        egraph: &EGraph<AstNode<Op>, A>,
        eclass: Id,
    ) -> Option<SearchMatches<'_, AstNode<Op>>> {
        let mut substs = Vec::new();
        for node in egraph[eclass].iter() {
            if node.operation() == &self.operation {
                let mut bound_values: HashMap<Id, HashMap<usize, Id>> = HashMap::new();
                for (arg_index, &arg_id) in node.iter().enumerate() {
                    for arg_node in egraph[arg_id].iter() {
                        if let Some(BindingExpr::Let(&bound_value, &body)) =
                            arg_node.as_binding_expr()
                        {
                            bound_values
                                .entry(bound_value)
                                .or_default()
                                .insert(arg_index, body);
                        }
                    }
                }

                for (bound_value, lifted_args) in bound_values {
                    let mut subst = Subst::with_capacity(node.len() + 1);
                    for (arg_index, &arg_id) in node.iter().enumerate() {
                        if let Some(&body) = lifted_args.get(&arg_index) {
                            subst.insert(Self::lift(arg_index), body);
                        } else {
                            subst.insert(Self::shift(arg_index), arg_id);
                        }
                    }
                    subst.insert(*BOUND_VALUE, bound_value);
                    substs.push(subst);
                }
            }
        }

        if substs.is_empty() {
            None
        } else {
            Some(SearchMatches {
                eclass,
                substs,
                ast: None,
            })
        }
    }

    fn vars(&self) -> Vec<Var> {
        vec![*BOUND_VALUE]
    }
}

impl<Op, A> Applier<AstNode<Op>, A> for LiftLib<Op>
where
    Op: Teachable + Arity + Clone + Debug,
    A: Analysis<AstNode<Op>>,
    AstNode<Op>: Language,
{
    fn apply_one(
        &self,
        egraph: &mut EGraph<AstNode<Op>, A>,
        eclass: Id,
        subst: &Subst,
        _searcher_ast: Option<&PatternAst<AstNode<Op>>>,
        _rule_name: Symbol,
    ) -> Vec<Id> {
        let bound_value = *subst.get(*BOUND_VALUE).unwrap();
        let mut args = Vec::new();
        let mut shifts = Vec::new();
        for i in 0.. {
            let (lift, shift) = (
                format!("?lift{}", i).parse().unwrap(),
                format!("?shift{}", i).parse().unwrap(),
            );
            if let Some(&arg) = subst.get(lift) {
                args.push(arg);
            } else if let Some(&arg) = subst.get(shift) {
                let shift = egraph.add(BindingExpr::Shift(arg).into());
                shifts.push(shift);
                args.push(shift);
            } else {
                break;
            }
        }

        let body = egraph.add(AstNode::new(self.operation.clone(), args));
        let lib = egraph.add(BindingExpr::Let(bound_value, body).into());
        let were_unioned = egraph.union(eclass, lib);

        if were_unioned {
            shifts.extend([body, lib, eclass]);
            shifts
        } else {
            vec![]
        }
    }

    fn vars(&self) -> Vec<Var> {
        vec![*BOUND_VALUE]
    }
}
