//! extract::partial implements a non-ILP-based extractor based on partial
//! orderings of learned library sets.
use egg::{Analysis, CostFunction, DidMerge, EGraph, Id, Language, RecExpr};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::{
    ast_node::{Arity, AstNode},
    learn::LibId,
    teachable::{BindingExpr, Teachable},
};

/// A `CostSet` is a set of pairs; each pair contains a set of library
/// functions paired with the cost of the current expression/eclass
/// without the lib fns, and the cost of the lib fns themselves.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CostSet {
    // Invariant: always sorted in ascending order of
    // expr_cost
    pub set: Vec<LibSel>,
}

impl CostSet {
    pub fn intro_op() -> CostSet {
        // println!("intro_op");
        let mut set = Vec::with_capacity(10);
        set.push(LibSel::intro_op());
        CostSet { set }
    }

    pub fn cross(&self, other: &CostSet) -> CostSet {
        // println!("cross");
        let mut set = Vec::new();

        for ls1 in &self.set {
            for ls2 in &other.set {
                let ls = ls1.combine(ls2);

                match set.binary_search_by_key(&ls.expr_cost, |ls: &LibSel| ls.expr_cost) {
                    Ok(pos) => set.insert(pos, ls),
                    Err(pos) => set.insert(pos, ls),
                }
            }
        }

        CostSet { set }
    }

    // Combination without unification
    pub fn combine(&mut self, other: CostSet) {
        // println!("combine");
        for elem in other.set {
            match self
                .set
                .binary_search_by_key(&elem.expr_cost, |ls| ls.expr_cost)
            {
                Ok(pos) => self.set.insert(pos, elem),
                Err(pos) => self.set.insert(pos, elem),
            }
        }
    }

    pub fn unify(&mut self) {
        // println!("unify");
        let mut i = 0;

        while i < self.set.len() {
            let mut j = i + 1;

            while j < self.set.len() {
                let ls1 = &self.set[i];
                let ls2 = &self.set[j];

                if ls1
                    .libs
                    .iter()
                    .all(|(k, _)| ls2.libs.binary_search_by_key(k, |(elem, _)| *elem).is_ok())
                {
                    self.set.remove(j);
                } else {
                    j += 1;
                }
            }
            i += 1;
        }
    }

    pub fn inc_cost(&mut self) {
        // println!("inc_cost");
        for ls in &mut self.set {
            ls.inc_cost();
        }
    }

    pub fn add_lib(&self, lib: LibId, cost: &CostSet) -> CostSet {
        // println!("add_lib");
        // To add a lib, we do a modified cross.
        let mut set = Vec::new();

        for ls1 in &cost.set {
            for ls2 in &self.set {
                let ls = ls2.add_lib(lib, ls1);

                match set.binary_search_by_key(&ls.expr_cost, |ls: &LibSel| ls.expr_cost) {
                    Ok(pos) => set.insert(pos, ls),
                    Err(pos) => set.insert(pos, ls),
                }
            }
        }

        CostSet { set }
    }

    pub fn prune(&mut self, n: usize) {
        // println!("prune");
        // Only preserve the n best `LibSel`s in the set.
        if self.set.len() > n {
            self.set.sort_unstable_by_key(|elem| elem.full_cost);
            self.set.drain(n..);
            self.set.sort_unstable_by_key(|elem| elem.expr_cost);
        }
    }
}

/// A `LibSel` is a selection of library functions, paired with two
/// corresponding cost values: the cost of the expression without the library
/// functions, and the cost of the library functions themselves
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LibSel {
    pub libs: Vec<(LibId, usize)>,
    pub expr_cost: usize,
    // Memoized expr_cost + sum({ l.1 for l in libs })
    pub full_cost: usize,
}

impl LibSel {
    pub fn intro_op() -> LibSel {
        LibSel {
            libs: Vec::new(),
            expr_cost: 1,
            full_cost: 1,
        }
    }

    /// Combines two `LibSel`s. Unions the lib sets, adds
    /// the expr
    pub fn combine(&self, other: &LibSel) -> LibSel {
        let mut res = self.clone();

        for (k, v) in &other.libs {
            match res.libs.binary_search_by_key(k, |(id, _)| *id) {
                Ok(ix) => {
                    if v < &res.libs[ix].1 {
                        res.full_cost -= res.libs[ix].1 - *v;
                        res.libs[ix].1 = *v
                    }
                }
                Err(ix) => {
                    res.full_cost += *v;
                    res.libs.insert(ix, (*k, *v))
                }
            }
        }

        res.expr_cost = self.expr_cost + other.expr_cost;
        res.full_cost += other.expr_cost;

        res
    }

    pub fn add_lib(&self, lib: LibId, cost: &LibSel) -> LibSel {
        let mut res = self.clone();
        let v = cost.expr_cost;
        let mut full_cost = res.full_cost;

        match res.libs.binary_search_by_key(&lib, |(id, _)| *id) {
            Ok(ix) => {
                if v < res.libs[ix].1 {
                    full_cost -= res.libs[ix].1 - v;
                    res.libs[ix].1 = v
                }
            }
            Err(ix) => {
                full_cost += v;
                res.libs.insert(ix, (lib, v))
            }
        }

        res.full_cost = full_cost;
        res
    }

    pub fn inc_cost(&mut self) {
        self.expr_cost += 1;
        self.full_cost += 1;
    }
}

// --------------------------------
// --- The actual Analysis part ---
// --------------------------------

#[derive(Debug, Clone, Copy)]
pub struct PartialLibCost {
    /// The number of `LibSel`s to keep per EClass.
    beam_size: usize,
    inter_beam: usize,
}

impl PartialLibCost {
    pub fn new(beam_size: usize, inter_beam: usize) -> PartialLibCost {
        PartialLibCost {
            beam_size,
            inter_beam,
        }
    }
}

impl<Op> Analysis<AstNode<Op>> for PartialLibCost
where
    Op: Ord + std::hash::Hash + Debug + Teachable + Arity + Eq + Clone + Send + Sync + 'static,
{
    type Data = CostSet;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        // println!("merge");
        // println!("{:?}", to);
        // println!("{:?}", &from);
        let a0 = to.clone();

        // Merging consists of combination, followed by unification and beam
        // pruning.
        to.combine(from.clone());
        to.unify();
        to.prune(self.beam_size);

        // TODO: be more efficient with how we do this
        DidMerge(&a0 != to, to != &from)
        // DidMerge(false, false)
    }

    fn make(&self, egraph: &EGraph<AstNode<Op>, Self>, enode: &AstNode<Op>) -> Self::Data {
        // println!("make");
        let x = |i: &Id| &egraph[*i].data;

        match Teachable::as_binding_expr(enode) {
            Some(BindingExpr::Let(id, f, b)) => {
                // This is a lib binding!
                // cross e1, e2 and introduce a lib!
                let mut e = x(b).add_lib(id, x(f));
                e.unify();
                // TODO: don't hardcode this
                e.prune(self.beam_size);
                e.inc_cost();
                e
            }
            Some(_) | None => {
                // This is some other operation of some kind.
                // We test the arity of the function

                if enode.is_empty() {
                    // 0 args. Return intro.
                    CostSet::intro_op()
                } else if enode.args().len() == 1 {
                    // 1 arg. Get child cost set, inc, and return.
                    let mut e = x(&enode.args()[0]).clone();
                    e.inc_cost();
                    e
                } else {
                    // 2+ args. Cross/unify time!
                    let mut e = x(&enode.args()[0]).clone();

                    for cs in &enode.args()[1..] {
                        e = e.cross(x(cs));
                        // Intermediate prune.
                        e.unify();
                        e.prune(self.inter_beam);
                    }

                    // TODO: intermediate unify/beam size reduction for each crossing step?
                    // do perf testing on this
                    // e.unify();
                    e.prune(self.beam_size);
                    e.inc_cost();
                    e
                }
            }
        }
    }

    // For debugging
    // fn modify(egraph: &mut EGraph<AstNode<Op>, Self>, id: Id) {
    //     println!("merge {}", id);
    //     println!("{:?}", &egraph[id].data)
    // }
}

/// A slightly less simple extractor that prioritizes choosing libs where it can
/// while avoiding cycles.
pub fn less_dumb_extractor<
    Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + Teachable + std::fmt::Display,
    N: Analysis<AstNode<Op>>,
>(
    egraph: &EGraph<AstNode<Op>, N>,
    root: Id,
) -> RecExpr<AstNode<Op>> {
    let mut expr = Vec::new();
    let mut stack = Vec::new();

    // For each eclass id.
    fn go<
        Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + Teachable + std::fmt::Display,
        N: Analysis<AstNode<Op>>,
    >(expr: &mut Vec<AstNode<Op>>, stack: &mut Vec<Id>, egraph: &EGraph<AstNode<Op>, N>, id: Id) -> Option<Id> {
        // If we've hit a cycle, return immediately.
        // Otherwise, add this Id to the stack.

        // TODO: More granular stack?
        // If we have an id and it's in the stack, theoretically we could use the id twice. just use the
        // next best option for the expr.
        if stack.contains(&id) {
            return None;
        } else {
            stack.push(id);
        }

        // Partition the nodes by whether they're libs or not.
        // We want to always use libs so
        let (mut all, nons): (Vec<AstNode<Op>>, Vec<AstNode<Op>>) =
            egraph[id].nodes.iter().cloned().partition(|x| {
                if let Some(BindingExpr::Let(_, _, _)) = x.as_binding_expr() {
                    true
                } else {
                    false
                }
            });
        all.extend(nons);

        // Go through each node
        'outer: for mut lib in all {
            // Try for each child
            for child in lib.iter_mut() {
                if let Some(id) = go(expr, stack, egraph, *child) {
                    *child = id;
                } else {
                    continue 'outer;
                }
            }

            // If we make it to this point; success!
            // Add to expr. Pop off stack. Return Id.
            expr.push(lib);
            stack.pop();
            return Some((expr.len() - 1).into());
        }

        // If we made it here, we never reached success.
        // Pop off stack, return None
        stack.pop();
        None
    }

    go(&mut expr, &mut stack, &egraph, root);

    expr.into()
}

/// A simple extractor that always chooses libs where it can.
pub fn dumb_extractor<
    Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + Teachable + std::fmt::Display,
    N: Analysis<AstNode<Op>>,
>(
    egraph: &EGraph<AstNode<Op>, N>,
    root: Id,
) -> RecExpr<AstNode<Op>> {
    let mut seen = HashMap::new();

    fn sel<
        Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + Teachable + std::fmt::Display,
        N: Analysis<AstNode<Op>>,
    >(
        egraph: &EGraph<AstNode<Op>, N>,
        seen: &mut HashMap<Id, usize>,
        id: Id,
    ) -> AstNode<Op> {
        println!("{}", id);

        let v = seen.entry(id).or_insert(0);
        *v += 1;

        for node in &egraph[id].nodes {
            if let Some(BindingExpr::Let(_, _, _)) = node.as_binding_expr() {
                return node.clone();
            }
        }

        *v -= 1;

        egraph[id].nodes[*v].clone()
    }

    sel(&egraph, &mut seen, root).build_recexpr(|id| sel(&egraph, &mut seen, id))
}

pub struct NoLibCost;
impl<Op> CostFunction<AstNode<Op>> for NoLibCost
where
    Op: Ord
        + std::hash::Hash
        + Debug
        + Teachable
        + Arity
        + Eq
        + Clone
        + Send
        + Sync
        + std::fmt::Display
        + 'static,
{
    type Cost = f64;

    fn cost<C>(&mut self, enode: &AstNode<Op>, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        match enode.as_binding_expr() {
            Some(BindingExpr::Let(_, _, body)) => costs(*body),
            _ => enode.fold(1.0, |sum, id| sum + costs(id)),
        }
    }
}