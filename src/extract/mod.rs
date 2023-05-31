//! Extracting expressions with learned libs out of egraphs

pub mod beam;

use std::collections::HashMap;

use egg::{Analysis, EGraph, Id, Language, RecExpr, Rewrite, Runner};

use crate::{
    ast_node::{Arity, AstNode},
    learn::LibId,
    teachable::{BindingExpr, Teachable},
};

/// Given an `egraph` that contains the original expression at `roots`,
/// and a set of library `rewrites`, extract the programs rewritten using the library.
pub fn apply_libs<Op, A>(
    egraph: EGraph<AstNode<Op>, A>,
    roots: &[Id],
    rewrites: &[Rewrite<AstNode<Op>, A>],
) -> RecExpr<AstNode<Op>>
where
    Op: Clone
        + Teachable
        + Ord
        + std::fmt::Debug
        + std::fmt::Display
        + std::hash::Hash
        + Arity
        + Send
        + Sync,
    A: Analysis<AstNode<Op>> + Default + Clone,
{
    let mut fin = Runner::<_, _, ()>::new(Default::default())
        .with_egraph(egraph)
        .run(rewrites.iter())
        .egraph;
    let root = fin.add(AstNode::new(Op::list(), roots.iter().copied()));

    let mut extractor = beam::LibExtractor::new(&fin);
    let best = extractor.best(root);
    lift_libs(&best)
}

fn build<Op: Clone + Teachable + std::fmt::Debug>(
    orig: &[AstNode<Op>],
    cur: Id,
    mut seen: impl FnMut(LibId, Id),
) -> AstNode<Op> {
    match orig[Into::<usize>::into(cur)].as_binding_expr() {
        Some(BindingExpr::Lib(id, lam, c)) => {
            seen(id, *lam);
            build(orig, *c, seen)
        }
        _ => orig[Into::<usize>::into(cur)].clone(),
    }
}

/// Given an expression `expr` containing library function definitions, move
/// those definitions to the top.
#[must_use]
pub fn lift_libs<Op>(expr: &RecExpr<AstNode<Op>>) -> RecExpr<AstNode<Op>>
where
    Op: Clone + Teachable + Ord + std::fmt::Debug + std::hash::Hash,
{
    let orig: Vec<AstNode<Op>> = expr.as_ref().to_vec();
    let mut seen = HashMap::new();

    let rest = orig[orig.len() - 1].build_recexpr(|id| {
        build(&orig, id, |k, v| {
            seen.insert(k, v);
        })
    });
    let mut res = rest.as_ref().to_vec();

    // Work queue for functions we still have to do
    let mut q: Vec<(LibId, Id)> = seen.iter().map(|(k, v)| (*k, *v)).collect();

    // TODO: order based on libs dependency w each other?
    while let Some((lib, expr)) = q.pop() {
        let body = res.len() - 1;
        let value: Vec<_> = orig[Into::<usize>::into(expr)]
            .build_recexpr(|id| {
                build(&orig, id, |k, v| {
                    if seen.insert(k, v).is_none() {
                        q.push((k, v));
                    }
                })
            })
            .as_ref()
            .iter()
            .cloned()
            .map(|x| x.map_children(|x| (usize::from(x) + res.len()).into()))
            .collect();
        res.extend(value);
        res.push(Teachable::lib(lib, Id::from(res.len() - 1), Id::from(body)));
    }

    res.into()
}
