pub mod beam;

#[cfg(feature = "grb")]
pub mod ilp;

use std::collections::HashMap;

use egg::{Id, Language, RecExpr};

use crate::{
    ast_node::{AstNode, Expr},
    learn::LibId,
    teachable::{BindingExpr, Teachable},
};

/// Lifts libs
pub fn lift_libs<Op>(expr: RecExpr<AstNode<Op>>) -> RecExpr<AstNode<Op>>
where
    Op: Clone + Teachable + Ord + std::fmt::Debug + std::hash::Hash,
{
    let orig: Vec<AstNode<Op>> = expr.as_ref().to_vec();
    let mut seen = HashMap::new();

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
                    if let None = seen.insert(k, v) {
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

/// Get the true cost of an expr
pub fn true_cost<Op: Clone>(expr: RecExpr<AstNode<Op>>) -> usize {
    Expr::len(&expr.into())
}
