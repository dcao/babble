//! extract::partial implements a non-ILP-based extractor based on partial
//! orderings of learned library sets.
use egg::{Analysis, CostFunction, DidMerge, EGraph, Id, Language, RecExpr};
use log::debug;
use std::{collections::HashMap, fmt::Debug};

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

                match set.binary_search(&ls) {
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
        let mut cix = 0;

        for elem in other.set {
            while cix < self.set.len() && &elem >= &self.set[cix] {
                cix += 1;
            }

            self.set.insert(cix, elem);
            cix += 1;
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

                if ls1.is_subset(&ls2) {
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

                match set.binary_search(&ls) {
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
            self.set.sort_unstable();
        }
    }
}

/// A `LibSel` is a selection of library functions, paired with two
/// corresponding cost values: the cost of the expression without the library
/// functions, and the cost of the library functions themselves
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LibSel {
    // We place this first so that it has prio in the Ord impl
    pub expr_cost: usize,
    // Memoized expr_cost + sum({ l.1 for l in libs })
    pub full_cost: usize,
    pub libs: Vec<(LibId, usize)>,
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

        // Add all libs that the lib uses, then add the lib itself.
        for (lib, v) in &cost.libs {
            let lib = *lib;
            let v = *v;

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
        }

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

    /// O(n) subset check
    pub fn is_subset(&self, other: &LibSel) -> bool {
        let mut oix = 0;

        // For every element in this LibSel, we want to see
        // if it exists in other.
        'outer: for (lib, _) in &self.libs {
            loop {
                // If oix is beyond the length of other, return false.
                if oix >= other.libs.len() {
                    return false;
                }

                if &other.libs[oix].0 == lib {
                    // If other[oix] is equal to lib, continue in the outer loop and increment oix
                    oix += 1;
                    continue 'outer;
                } else if &other.libs[oix].0 > lib {
                    // Otherwise if it's larger, there was no element equal. Not subset, ret false.
                    return false;
                } else {
                    // Increment oix by default
                    oix += 1;
                }
            }
        }

        // We made it! ret true
        true
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

        // println!("{:?}", to);
        // println!("{} {}", &a0 != to, to != &from);
        // TODO: be more efficient with how we do this
        DidMerge(&a0 != to, to != &from)
        // DidMerge(false, false)
    }

    fn make(&self, egraph: &EGraph<AstNode<Op>, Self>, enode: &AstNode<Op>) -> Self::Data {
        // println!("make");
        let x = |i: &Id| &egraph[*i].data;

        match Teachable::as_binding_expr(enode) {
            Some(BindingExpr::Lib(id, f, b)) => {
                // This is a lib binding!
                // cross e1, e2 and introduce a lib!
                let mut e = x(b).add_lib(id, x(f));
                e.unify();
                e.prune(self.beam_size);
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
                    e.unify();
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

// TODO: optimize this code maybe. It's been cleaned up but idk
/// A less simple extractor which avoids cycles.
pub fn less_dumb_extractor<
    Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + Teachable + std::fmt::Display,
    N: Analysis<AstNode<Op>>,
>(
    egraph: &EGraph<AstNode<Op>, N>,
    root: Id,
) -> RecExpr<AstNode<Op>> {
    let mut expr = Vec::new();
    let mut stack = Vec::new();
    let mut lib_stack = Vec::new();

    // Pre-compute node vecs.
    let mut node_vecs = HashMap::new();
    for eclass in egraph.classes() {
        let (mut all, nons): (Vec<AstNode<Op>>, Vec<AstNode<Op>>) =
            eclass.nodes.iter().cloned().partition(|x| {
                if let Some(BindingExpr::Lib(_, _, _)) = x.as_binding_expr() {
                    true
                } else {
                    false
                }
            });
        all.extend(nons);
        node_vecs.insert(eclass.id, all);
    }

    // For each eclass id.
    fn go<
        Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + Teachable + std::fmt::Display,
        N: Analysis<AstNode<Op>>,
    >(
        expr: &mut Vec<AstNode<Op>>,
        stack: &mut Vec<(Id, usize)>,
        lib_stack: &mut Vec<LibId>,
        node_vecs: &HashMap<Id, Vec<AstNode<Op>>>,
        egraph: &EGraph<AstNode<Op>, N>,
        id: Id,
    ) -> Id {
        let cur_ix = if let Some((_, prev_ix)) = stack.iter().rev().find(|x| x.0 == id) {
            let new_ix = if prev_ix + 1 < node_vecs.get(&id).unwrap().len() {
                prev_ix + 1
            } else {
                *prev_ix
            };

            stack.push((id, new_ix));

            &mut stack.last_mut().unwrap().1
        } else {
            stack.push((id, 0));
            &mut stack.last_mut().unwrap().1
        };

        // First, skip through each repeated lib.
        let mut node = &node_vecs[&id][*cur_ix];
        let mut is_lib = false;
        loop {
            if let Some(BindingExpr::Lib(lib_id, _, _)) = node.as_binding_expr() {
                if lib_stack.contains(&lib_id) {
                    *cur_ix += 1;
                    node = &node_vecs[&id][*cur_ix];
                } else {
                    lib_stack.push(lib_id);
                    is_lib = true;
                    break;
                }
            } else {
                break;
            }
        }

        // Now we know that node is either a new lib or a non-lib.
        // For each child, run this procedure.
        let mut this_node = node.clone();
        for child in this_node.iter_mut() {
            *child = go(expr, stack, lib_stack, node_vecs, egraph, *child);
        }

        // Success!
        // Add to expr. Pop off stack. Return Id.
        if is_lib {
            lib_stack.pop();
        }
        expr.push(this_node);
        stack.pop();

        // println!("SUCCESS {:?}", stack);
        (expr.len() - 1).into()
    }

    go(
        &mut expr,
        &mut stack,
        &mut lib_stack,
        &node_vecs,
        &egraph,
        root,
    );

    expr.into()
}

/// Extractor that minimizes AST size but ignores the cost of library definitions
/// (which will be later lifted to the top)
#[derive(Debug)]
pub struct LibExtractor<
    Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + Teachable + std::fmt::Display,
> {
    /// Remembers the best expression so far for each class id;
    /// if a class id is absent, we haven't visited it yet;
    /// if a class id maps to None, it's currently under processing, but we have no results for it yet;
    /// if a class id maps to Some(_), we have found an expression for it (but it might still be improved).
    memo: HashMap<Id, Option<RecExpr<AstNode<Op>>>>,
    /// Representation of the egraph
    /// where in each eclass, all non-lib nodes come before all lib nodes
    node_vecs: HashMap<Id, Vec<AstNode<Op>>>,
}

impl<Op> LibExtractor<Op>
where
    Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + Teachable + std::fmt::Display + Arity,
{
    /// Create a lib extractor for the given egraph
    pub fn new<N: Analysis<AstNode<Op>>>(egraph: &EGraph<AstNode<Op>, N>) -> Self {
        let mut node_vecs = HashMap::new();
        for eclass in egraph.classes() {
            debug!("{}: {:?}", eclass.id, eclass.nodes);
            // Sort nodes so that non-lib nodes come first
            let (libs, mut non_libs): (Vec<AstNode<Op>>, Vec<AstNode<Op>>) = eclass
                .nodes
                .iter()
                .cloned()
                .partition(|x| matches!(x.as_binding_expr(), Some(BindingExpr::Lib(_, _, _))));
            non_libs.extend(libs);
            node_vecs.insert(eclass.id, non_libs);
        }

        Self {
            memo: HashMap::new(),
            node_vecs,
        }
    }

    /// Extract the smallest expression from the eclass id
    /// # Panics
    /// Panics if extraction fails
    /// (but this should never happen because the e-graph must contain a non-cyclic expression)
    pub fn best(&mut self, id: Id) -> RecExpr<AstNode<Op>> {
        // Populate the memo:
        self.extract(id);
        // Get the best expression from the memo:
        self.memo.get(&id).unwrap().clone().unwrap()
    }

    /// Expression cost used by this extractor (which is `NoLibCost`)
    fn cost(expr: &RecExpr<AstNode<Op>>) -> usize {
        NoLibCost.cost_rec(expr)
    }

    /// Extract the smallest expression from the eclass id and its descendants
    /// storing results in the memo
    fn extract(&mut self, id: Id) {
        debug!("extracting {}", id);
        if let Some(res) = self.memo.get(&id) {
            debug!("memoized {:?}", res);
        } else {
            // Initialize memo with None to prevent infinite recursion
            // in case of cycles in the egraph
            self.memo.insert(id, None);
            // Extract a candidate expression from each node
            for node in self.node_vecs[&id].clone() {
                match self.extract_node(&node) {
                    None => (), // Extraction for this node failed (must be a cycle)
                    Some(cand) => {
                        // Extraction succeeded: check if cand is better than what we have so far
                        match self.memo.get(&id).unwrap() {
                            // If we already had an expression and it was better, do nothing
                            Some(prev) if Self::cost(prev) <= Self::cost(&cand) => (),
                            // Otherwise, update the memo;
                            // the reason we want to update the memo after each candidate as opposed to once at the end
                            // is because a lib definition could loop back to the same eclass as the lib iteself;
                            // and in that case we want to pick the best non-lib node as the lib definition (as opposed to having it undefined);
                            // this is also why we need to sort the nodes.
                            _ => {
                                debug!("new best for {}: {:?}", id, cand);
                                self.memo.insert(id, Some(cand));
                            }
                        }
                    }
                }
            }
        }
    }

    /// Extract the smallest expression from the node
    fn extract_node(&mut self, node: &AstNode<Op>) -> Option<RecExpr<AstNode<Op>>> {
        let mut child_indexes = vec![];
        self.extract_children(node, 0, vec![], &mut child_indexes)
    }

    /// Process the children of `node` starting from index `current`
    /// and accumulate results in `partial expr`;
    /// `child_indexes` stores the indexes of already processed children within `partial_expr`,
    /// so that we can use them in the `AstNode` at the end.
    fn extract_children(
        &mut self,
        node: &AstNode<Op>,
        current: usize,
        mut partial_expr: Vec<AstNode<Op>>,
        child_indexes: &mut Vec<usize>,
    ) -> Option<RecExpr<AstNode<Op>>> {
        if current == node.children().len() {
            // Done with children: add ourselves to the partial expression and return
            let child_ids: Vec<Id> = child_indexes.iter().map(|x| (*x).into()).collect();
            let root = AstNode::new(node.operation().clone(), child_ids);
            partial_expr.push(root);
            Some(partial_expr.into())
        } else {
            // Recurse on the next child
            let child = &node.children()[current];
            self.extract(*child);
            match self.memo.get(child).unwrap() {
                None => None,
                Some(expr) => {
                    // We need to clone the expr because we're going to offset child indexes,
                    // and we don't want it to affect the memo result for child.
                    let mut new_expr = expr.clone().as_ref().to_vec();
                    for n in &mut new_expr {
                        // Increment all indexes inside `n` by the current expression length;
                        // this is needed to make a well-formed `RecExpr`
                        Self::offset_children(n, partial_expr.len());
                    }
                    partial_expr.extend(new_expr);
                    child_indexes.push(partial_expr.len() - 1);
                    self.extract_children(node, current + 1, partial_expr, child_indexes)
                }
            }
        }
    }

    /// Add `offset` to all children of `node`
    fn offset_children(node: &mut AstNode<Op>, offset: usize) {
        for child in node.children_mut() {
            let child_index: usize = (*child).into();
            *child = (child_index + offset).into();
        }
    }
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
            if let Some(BindingExpr::Lib(_, _, _)) = node.as_binding_expr() {
                return node.clone();
            }
        }

        *v -= 1;

        egraph[id].nodes[*v].clone()
    }

    sel(&egraph, &mut seen, root).build_recexpr(|id| sel(&egraph, &mut seen, id))
}

/// Cost function that does not count library definitions
#[derive(Debug, Clone, Copy)]
pub struct NoLibCost;

impl<Op> CostFunction<AstNode<Op>> for NoLibCost
where
    Op: Ord + std::hash::Hash + Debug + Teachable + Clone,
{
    type Cost = usize;

    fn cost<C>(&mut self, enode: &AstNode<Op>, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        match enode.as_binding_expr() {
            Some(BindingExpr::Lib(_, _, body)) => costs(*body),
            _ => enode.fold(1, |sum, id| sum + costs(id)),
        }
    }
}
