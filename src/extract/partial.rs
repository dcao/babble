//! extract::partial implements a non-ILP-based extractor based on partial
//! orderings of learned library sets.
use egg::{Analysis, DidMerge, EGraph, Id};
use std::{collections::BTreeMap, fmt::Debug};

use crate::{
    ast_node::{Arity, AstNode},
    teachable::{BindingExpr, Teachable},
};

/// A `CostSet` is a set of pairs; each pair contains a set of library
/// functions paired with the cost of the current expression/eclass
/// without the lib fns, and the cost of the lib fns themselves.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CostSet {
    // Invariant: always sorted in ascending order of
    // expr_cost + libs_cost
    pub set: Vec<LibSel>,
}

impl CostSet {
    pub fn intro_op() -> CostSet {
        // println!("intro_op");
        let mut set = Vec::with_capacity(10);
        set.push(LibSel::intro_op());
        CostSet {
            set,
        }
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
                let mut rem = false;

                if ls1.libs.iter().all(|(k, _)| ls2.libs.binary_search_by_key(k, |(elem, _)| *elem).is_ok()) {
                    rem = true;
                } else {
                    j += 1;
                }

                if rem {
                    self.set.remove(j);
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

    pub fn add_lib(&self, lib: Id, cost: &CostSet) -> CostSet {
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
            self.set.sort_by_key(|elem| elem.full_cost);
            self.set.drain(n..);
            self.set.sort_by_key(|elem| elem.expr_cost);
        }
    }
}

/// A `LibSel` is a selection of library functions, paired with two
/// corresponding cost values: the cost of the expression without the library
/// functions, and the cost of the library functions themselves
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LibSel {
    pub libs: Vec<(Id, usize)>,
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
                Ok(ix) => if v < &res.libs[ix].1 { res.full_cost -= res.libs[ix].1 - *v; res.libs[ix].1 = *v },
                Err(ix) => { res.full_cost += *v; res.libs.insert(ix, (*k, *v)) },
            }
        }

        res.expr_cost = self.expr_cost + other.expr_cost;
        res.full_cost += other.expr_cost;

        res
    }

    pub fn add_lib(&self, lib: Id, cost: &LibSel) -> LibSel {
        let mut res = self.clone();
        let v = cost.expr_cost;
        let mut full_cost = res.full_cost;

        match res.libs.binary_search_by_key(&lib, |(id, _)| *id) {
            Ok(ix) => if v < res.libs[ix].1 { full_cost -= res.libs[ix].1 - v; res.libs[ix].1 = v },
            Err(ix) => { full_cost += v; res.libs.insert(ix, (lib, v)) },
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
    // TODO: intermediate beam size for while we cross?
}

impl PartialLibCost {
    pub fn new(beam_size: usize) -> PartialLibCost {
        PartialLibCost { beam_size }
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
        // TODO: don't hardcode size
        to.prune(10);

        // TODO: be more efficient with how we do this
        DidMerge(&a0 != to, to != &from)
        // DidMerge(false, false)
    }

    fn make(egraph: &EGraph<AstNode<Op>, Self>, enode: &AstNode<Op>) -> Self::Data {
        // println!("make");
        let x = |i: &Id| &egraph[*i].data;

        match Teachable::as_binding_expr(enode) {
            Some(BindingExpr::Let(f, b)) => {
                // This is a lib binding!
                // cross e1, e2 and introduce a lib!
                let mut e = x(b).add_lib(*f, x(f));
                e.unify();
                // TODO: don't hardcode this
                e.prune(10);
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
                        // TODO: don't hardcode this
                        e.unify();
                        e.prune(100);
                    }

                    // TODO: intermediate unify/beam size reduction for each crossing step?
                    // do perf testing on this
                    e.unify();
                    // TODO: dont hardcode this
                    e.prune(10);
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

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
