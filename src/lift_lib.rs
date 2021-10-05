//! Rewrites which hoist let-expressions.

use crate::{
    ast_node::{Arity, AstNode},
    free_vars::{FreeVarAnalysis, FreeVars},
    teachable::{BindingExpr, Teachable},
};
use egg::{
    Applier, EGraph, Id, Language, PatternAst, Rewrite, SearchMatches, Searcher, Subst, Symbol, Var,
};
use lazy_static::lazy_static;
use std::{collections::HashMap, fmt::Debug, sync::Mutex};

lazy_static! {
    static ref IDENT_VAR: Var = "?ident".parse().unwrap();
    static ref VALUE_VAR: Var = "?value".parse().unwrap();
    static ref ARG_VARS: ArgVars = ArgVars::with_capacity(10);
}

#[derive(Debug)]
struct ArgVars(Mutex<Vec<Var>>);

impl ArgVars {
    fn with_capacity(capacity: usize) -> Self {
        let arg_vars = Self(Mutex::new(Vec::with_capacity(capacity)));
        let _ = arg_vars.get(capacity - 1);
        arg_vars
    }

    fn make_var(arg: usize) -> Var {
        format!("?arg{}", arg).parse().unwrap()
    }

    fn get(&self, arg: usize) -> Var {
        let mut arg_vars = self.0.lock().unwrap();
        arg_vars.get(arg).copied().unwrap_or_else(|| {
            let len = arg_vars.len();
            arg_vars.extend((len..=arg).into_iter().map(Self::make_var));
            arg_vars[arg]
        })
    }
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
}

impl<Op> LiftLib<Op>
where
    Op: FreeVars + Teachable + Arity + Eq + Clone + Send + Sync + Debug + 'static,
    AstNode<Op>: Language,
{
    /// Creates a new [`Rewrite`] named `name` which lifts over the given
    /// operation. `LiftLib` is used as both the [`Searcher`] and [`Applier`] of
    /// the rewrite.
    #[must_use]
    pub fn rewrite(name: &str, operation: Op) -> Rewrite<AstNode<Op>, FreeVarAnalysis<Op>> {
        let lift_lib = Self::new(operation);
        Rewrite::new(name, lift_lib.clone(), lift_lib).unwrap_or_else(|_| unreachable!())
    }

    fn search_operation(
        egraph: &EGraph<AstNode<Op>, FreeVarAnalysis<Op>>,
        args: &[Id],
    ) -> Vec<Subst> {
        let mut idents = HashMap::new();
        let mut arg_maps = Vec::new();
        for &arg in args.iter() {
            let mut arg_map = HashMap::new();
            for node in egraph[arg].iter() {
                if let Some(BindingExpr::Lib { ident, value, body }) = node.as_binding_expr() {
                    idents.insert(*ident, *value);
                    arg_map.insert(*ident, *body);
                }
            }
            arg_maps.push(arg_map);
        }

        let mut substs = Vec::new();

        for (&ident_id, &fun) in &idents {
            let mut subst = Subst::with_capacity(args.len() + 2);
            subst.insert(*IDENT_VAR, ident_id);
            subst.insert(*VALUE_VAR, fun);

            let ident = egraph[ident_id].nodes[0].operation().get_ident().unwrap();
            let mut can_rewrite = true;
            for (i, &arg) in args.iter().enumerate() {
                if egraph[arg].data.contains(&ident) {
                    can_rewrite = false;
                    break;
                }

                let arg_map = &arg_maps[i];
                let var = ARG_VARS.get(i);
                let id = *arg_map.get(&ident_id).unwrap_or(&ident_id);
                subst.insert(var, id);
                if let Some(&value) = arg_map.get(&ident_id) {
                    subst.insert(var, value);
                } else {
                    subst.insert(var, arg);
                }
            }

            if can_rewrite {
                substs.push(subst);
            }
        }

        substs
    }
}

impl<Op> Searcher<AstNode<Op>, FreeVarAnalysis<Op>> for LiftLib<Op>
where
    Op: Debug + Teachable + FreeVars + Arity + Eq + Clone + Send + Sync + 'static,
    AstNode<Op>: Language,
{
    fn search_eclass(
        &self,
        egraph: &EGraph<AstNode<Op>, FreeVarAnalysis<Op>>,
        eclass: Id,
    ) -> Option<SearchMatches<'_, AstNode<Op>>> {
        let mut substs = Vec::new();
        for enode in egraph[eclass].iter() {
            if enode.operation() == &self.operation {
                let list_substs = Self::search_operation(egraph, enode.args());
                substs.extend(list_substs);
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
        vec![*IDENT_VAR, *VALUE_VAR]
    }
}

impl<Op> Applier<AstNode<Op>, FreeVarAnalysis<Op>> for LiftLib<Op>
where
    Op: Teachable + FreeVars + Arity + Clone + Debug,
    AstNode<Op>: Language,
{
    fn apply_one(
        &self,
        egraph: &mut EGraph<AstNode<Op>, FreeVarAnalysis<Op>>,
        eclass: Id,
        subst: &Subst,
        _searcher_ast: Option<&PatternAst<AstNode<Op>>>,
        _rule_name: Symbol,
    ) -> Vec<Id> {
        let ident = *subst.get(*IDENT_VAR).unwrap();
        let value = *subst.get(*VALUE_VAR).unwrap();
        let mut items = Vec::new();

        for i in 0.. {
            if let Some(&id) = subst.get(ARG_VARS.get(i)) {
                items.push(id);
            } else {
                break;
            }
        }

        let body = egraph.add(AstNode::new(self.operation.clone(), items));
        let lib = egraph.add(BindingExpr::Lib { ident, value, body }.into());
        let were_unioned = egraph.union(eclass, lib);

        if were_unioned {
            vec![body, lib, eclass]
        } else {
            Vec::new()
        }
    }

    fn vars(&self) -> Vec<Var> {
        vec![*IDENT_VAR, *VALUE_VAR]
    }
}
