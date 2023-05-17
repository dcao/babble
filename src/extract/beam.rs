//! extract::partial implements a non-ILP-based extractor based on partial
//! orderings of learned library sets.
use egg::{Analysis, CostFunction, DidMerge, EGraph, Id, Language, RecExpr};
use log::debug;
use std::{
    collections::{BinaryHeap, HashMap},
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
    /// The set of library selections and their associated costs.
    /// Invariant: sorted in ascending order of expr_cost, except during
    /// pruning, when it's sorted in order of full_cost.
    pub set: Vec<LibSel>,
}

impl CostSet {
    /// Creates a CostSet corresponding to introducing a nonary operation.
    /// This will always have the value [{} -> (1, 1)].
    pub fn intro_op() -> CostSet {
        let mut set = Vec::with_capacity(10);
        set.push(LibSel::intro_op());
        CostSet { set }
    }

    /// Crosses over two CostSets.
    /// This is essentially a Cartesian product between two CostSets (e.g. if
    /// each CostSet corresponds to an argument of a node) such that paired
    /// LibSels have their libraries combined and costs added.
    pub fn cross(&self, other: &CostSet, lps: usize) -> CostSet {
        let mut set = Vec::new();

        for ls1 in &self.set {
            for ls2 in &other.set {
                match ls1.combine(ls2, lps) {
                    None => continue,
                    Some(ls) => match set.binary_search(&ls) {
                        Ok(_) => {}
                        Err(pos) => set.insert(pos, ls),
                    },
                }
            }
        }

        CostSet { set }
    }

    /// Combines two CostSets by unioning them together.
    /// Used for e.g. different ENodes of an EClass.
    pub fn combine(&mut self, other: CostSet) {
        // println!("combine");
        let mut cix = 0;

        for elem in other.set {
            while cix < self.set.len() && &elem >= &self.set[cix] {
                cix += 1;
            } // Nadia: can't this be done with a binary search?

            self.set.insert(cix, elem);
            cix += 1;
        }
    }

    /// Performs trivial partial order reduction: if CostSet A contains a superset
    /// of the libs of another CostSet B, and A has a higher expr_cost than B, remove A.
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

    /// Increments the expr and full cost of every LibSel in this CostSet.
    /// This is done if we e.g. cross all the args of a node, then have to add
    /// the node itself to the cost.
    pub fn inc_cost(&mut self) {
        // println!("inc_cost");
        for ls in &mut self.set {
            ls.inc_cost();
        }
    }

    pub fn add_lib(&self, lib: LibId, cost: &CostSet, lps: usize) -> CostSet {
        // println!("add_lib");
        // To add a lib, we do a modified cross.
        let mut set = Vec::new();

        for ls1 in &cost.set {
            if ls1.libs.iter().any(|l| l.0 == lib) {
                // If this libsel contains the lib we are defining,
                // we can't use in in the definition.
                continue;
            }
            for ls2 in &self.set {
                match ls2.add_lib(lib, ls1, lps) {
                    None => continue,
                    Some(ls) => match set.binary_search(&ls) {
                        Ok(_pos) => (), // set.insert(pos, ls), // TODO: why did we used to insert it again?
                        Err(pos) => set.insert(pos, ls),
                    },
                }
            }
        }

        CostSet { set }
    }

    /// prune takes care of two different tasks to reduce the number
    /// of functions in a LibSel:
    ///
    /// - If we have an lps limit, we remove anything that has more fns
    ///   than we allow in a LibSel
    /// - Then, if we still have too many LibSels, we prune based on
    ///   beam size.
    ///
    /// Our pruning strategy preserves n LibSels per # of libs, where
    /// n is the beam size. In other words, we preserve n LibSels with
    /// 0 libs, n LibSels with 1 lib, etc.
    pub fn prune(&mut self, n: usize, lps: usize, extra_por: bool) {
        use std::cmp::Reverse;

        if extra_por {
            panic!("extra_por unimplemented for new pruning");
        }

        let old_set = std::mem::take(&mut self.set);

        // First, we create a table from # of libs to a list of LibSels
        let mut table: HashMap<usize, BinaryHeap<Reverse<LibSelFC>>> = HashMap::new();

        // We then iterate over all of the LibSels in this set
        for ls in old_set {
            let num_libs = ls.libs.len();

            // We don't need to do this anymore, because this is happening in cross:
            // If lps is set, if num_libs > lps, give up immediately
            // if num_libs > lps {
            //     panic!("LibSels that are too large should have been filtered out by cross!");
            // }

            let h = table.entry(num_libs).or_insert_with(|| BinaryHeap::new());

            h.push(Reverse(LibSelFC(ls)));
        }

        // From our table, recombine into a sorted vector
        let mut set = Vec::new();
        let beams_per_size = std::cmp::max(1, n / lps);

        for (_sz, mut h) in table {
            // Take the first n items from the heap
            let mut i = 0;
            while i < beams_per_size {
                if let Some(ls) = h.pop() {
                    let ls = ls.0 .0;

                    match set.binary_search(&ls) {
                        Ok(_) => {}
                        Err(pos) => set.insert(pos, ls),
                    }

                    i += 1;
                } else {
                    break;
                }
            }
        }

        self.set = set;
    }

    pub fn unify2(&mut self) {
        // println!("unify");
        let mut i = 0;

        while i < self.set.len() {
            let mut j = i + 1;

            while j < self.set.len() {
                let ls1 = &self.set[i];
                let ls2 = &self.set[j];

                if ls2.is_subset(&ls1) {
                    self.set.remove(j);
                } else {
                    j += 1;
                }
            }
            i += 1;
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
    pub fn combine(&self, other: &LibSel, lps: usize) -> Option<LibSel> {
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
                    res.libs.insert(ix, (*k, *v));
                    if res.libs.len() > lps {
                        return None;
                    }
                }
            }
        }

        res.expr_cost = self.expr_cost + other.expr_cost;
        res.full_cost += other.expr_cost;

        Some(res)
    }

    pub fn add_lib(&self, lib: LibId, cost: &LibSel, lps: usize) -> Option<LibSel> {
        let mut res = self.clone();
        let v = cost.expr_cost + 1; // +1 for the lib node, which you also have to pay for if you use the lib
        let mut full_cost = res.full_cost;

        // Add all nested libs that the lib uses, then add the lib itself.
        for (nested_lib, v) in &cost.libs {
            let nested_lib = *nested_lib;
            let v = *v;

            match res.libs.binary_search_by_key(&nested_lib, |(id, _)| *id) {
                Ok(ix) => {
                    if v < res.libs[ix].1 {
                        full_cost -= res.libs[ix].1 - v;
                        res.libs[ix].1 = v;
                    }
                }
                Err(ix) => {
                    full_cost += v;
                    res.libs.insert(ix, (nested_lib, v));
                    if res.libs.len() > lps {
                        return None;
                    }
                }
            }
        }

        match res.libs.binary_search_by_key(&lib, |(id, _)| *id) {
            Ok(ix) => {
                if v < res.libs[ix].1 {
                    full_cost -= res.libs[ix].1 - v;
                    res.libs[ix].1 = v;
                }
            }
            Err(ix) => {
                full_cost += v;
                res.libs.insert(ix, (lib, v));
                if res.libs.len() > lps {
                    return None;
                }
            }
        }

        res.full_cost = full_cost;
        Some(res)
    }

    pub fn inc_cost(&mut self) {
        self.expr_cost += 1;
        self.full_cost += 1;
    }

    /// O(n) subset check
    #[must_use]
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

/// A wrapper around LibSels that orders based on their full cost.
#[derive(Debug, Clone, PartialEq, Eq)]
struct LibSelFC(pub(crate) LibSel);

impl PartialOrd for LibSelFC {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let mut r = self.0.full_cost.cmp(&other.0.full_cost);
        if !r.is_eq() {
            return Some(r);
        }

        r = self.0.expr_cost.cmp(&other.0.expr_cost);
        if !r.is_eq() {
            return Some(r);
        }

        Some(self.0.libs.cmp(&other.0.libs))
    }
}

impl Ord for LibSelFC {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
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
    /// The maximum number of libs per lib selection. Any lib selections with a larger amount will
    /// be pruned.
    lps: usize,
    extra_por: bool,
}

impl PartialLibCost {
    #[must_use]
    pub fn new(beam_size: usize, inter_beam: usize, lps: usize, extra_por: bool) -> PartialLibCost {
        PartialLibCost {
            beam_size,
            inter_beam,
            lps,
            extra_por,
        }
    }

    #[must_use]
    pub fn empty() -> PartialLibCost {
        PartialLibCost {
            beam_size: 0,
            inter_beam: 0,
            lps: 1,
            extra_por: false,
        }
    }
}

impl Default for PartialLibCost {
    fn default() -> Self {
        PartialLibCost::empty()
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
        to.prune(self.beam_size, self.lps, self.extra_por);

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
                let mut e = x(b).add_lib(id, x(f), self.lps);
                e.unify();
                e.prune(self.beam_size, self.lps, self.extra_por);
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
                        e = e.cross(x(cs), self.lps);
                        // Intermediate prune.
                        e.unify();
                        e.prune(self.inter_beam, self.lps, self.extra_por);
                    }

                    e.unify();
                    e.prune(self.beam_size, self.lps, self.extra_por);
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

/// Library context is a set of library function names.
/// It is used in the extractor to represent the fact that we are extracting
/// inside (nested) library definitions.
#[derive(Debug, Clone, PartialEq, Eq, std::hash::Hash)]
struct LibContext {
    set: Vec<LibId>,
}

impl LibContext {
    fn new() -> Self {
        Self { set: Vec::new() }
    }

    /// Add a new lib to the context if not yet present,
    /// keeping it sorted.
    fn add(&mut self, lib_id: LibId) {
        match self.set.binary_search(&lib_id) {
            Ok(_) => {} // already present
            Err(pos) => self.set.insert(pos, lib_id),
        }
    }

    /// Does the context contain the given lib?
    fn contains(&self, lib_id: LibId) -> bool {
        self.set.binary_search(&lib_id).is_ok()
    }
}

type MaybeExpr<Op> = Option<RecExpr<AstNode<Op>>>;

/// This is here for debugging purposes.
fn display_maybe_expr<Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + std::fmt::Display>(
    maybe_expr: &MaybeExpr<Op>,
) -> String {
    if let Some(expr) = maybe_expr {
        expr.pretty(100)
    } else {
        "<none>".to_string()
    }
}

/// Extractor that minimizes AST size but ignores the cost of library definitions
/// (which will be later lifted to the top).
/// The main difference between this and a standard extractor is that
/// instead of finding the best expression *per eclass*,
/// we need to find the best expression *per eclass and lib context*.
/// This is because when extracting inside library definitions,
/// we are not allowed to use those libraries;
/// so the best expression is different depending on which library defs we are currently inside.
#[derive(Debug)]
pub struct LibExtractor<
    'a,
    Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + Teachable + std::fmt::Display,
    N: Analysis<AstNode<Op>>,
> {
    /// Remembers the best expression so far for each pair of class id and lib context;
    /// if an entry is absent, we haven't visited this class in this context yet;
    /// if an entry is None, it's currently under processing, but we have no results for it yet;
    /// if an entry is Some(_), we have found an expression for it (but it might still be improved).
    memo: HashMap<Id, HashMap<LibContext, MaybeExpr<Op>>>,
    /// Current lib context:
    /// contains all lib ids inside whose definitions we are currently extracting.
    lib_context: LibContext,
    /// The egraph to extract from.
    egraph: &'a EGraph<AstNode<Op>, N>,
    /// This is here for pretty debug messages.
    indent: usize,
}

impl<'a, Op, N> LibExtractor<'a, Op, N>
where
    Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + Teachable + std::fmt::Display + Arity,
    N: Analysis<AstNode<Op>> + Clone,
{
    /// Create a lib extractor for the given egraph
    pub fn new(egraph: &'a EGraph<AstNode<Op>, N>) -> Self {
        Self {
            memo: HashMap::new(),
            lib_context: LibContext::new(),
            egraph,
            indent: 0,
        }
    }

    /// Get best best expression for `id` in the current lib context.
    fn get_from_memo(&self, id: Id) -> Option<&MaybeExpr<Op>> {
        self.memo.get(&id)?.get(&self.lib_context)
    }

    /// Set best best expression for `id` in the current lib context.
    fn insert_into_memo(&mut self, id: Id, val: MaybeExpr<Op>) {
        self.memo
            .entry(id)
            .or_insert_with(HashMap::new)
            .insert(self.lib_context.clone(), val);
    }

    /// Extract the smallest expression for the eclass `id`.
    /// # Panics
    /// Panics if extraction fails
    /// (this should never happen because the e-graph must contain a non-cyclic expression)
    pub fn best(&mut self, id: Id) -> RecExpr<AstNode<Op>> {
        // Populate the memo:
        self.extract(id);
        // Get the best expression from the memo:
        self.get_from_memo(id).unwrap().clone().unwrap()
    }

    /// Expression cost used by this extractor (which is `NoLibCost`)
    fn cost(expr: &RecExpr<AstNode<Op>>) -> usize {
        NoLibCost.cost_rec(expr)
    }

    /// Extract the smallest expression from the eclass id and its descendants
    /// in the current context, storing results in the memo
    fn extract(&mut self, id: Id) {
        self.debug_indented(&format!("extracting eclass {id}"));
        if let Some(res) = self.get_from_memo(id) {
            // This node has already been visited in this context (either done or under processing)
            self.debug_indented(&format!(
                "visited, memoized value: {}",
                display_maybe_expr(res)
            ));
        } else {
            // Initialize memo with None to prevent infinite recursion in case of cycles in the egraph
            self.insert_into_memo(id, None);
            // Extract a candidate expression from each node
            for node in self.egraph[id].iter() {
                match self.extract_node(node) {
                    None => (), // Extraction for this node failed (must be a cycle)
                    Some(cand) => {
                        // Extraction succeeded: check if cand is better than what we have so far
                        match self.get_from_memo(id).unwrap() {
                            // If we already had an expression and it was better, do nothing
                            Some(prev) if Self::cost(prev) <= Self::cost(&cand) => (),
                            // Otherwise, update the memo;
                            // note that updating the memo after each better candidate is found instead of at the end
                            // is slightly suboptimal (because it might cause us to go around some cycles once),
                            // but the code is simpler and it doesn't matter too much.
                            _ => {
                                self.debug_indented(&format!(
                                    "new best for {id}: {} (cost {})",
                                    cand.pretty(100),
                                    Self::cost(&cand)
                                ));
                                self.insert_into_memo(id, Some(cand));
                            }
                        }
                    }
                }
            }
        }
    }

    /// Extract the smallest expression from `node`.
    fn extract_node(&mut self, node: &AstNode<Op>) -> MaybeExpr<Op> {
        self.debug_indented(&format!("extracting node {node:?}"));
        if let Some(BindingExpr::Lib(lid, _, _)) = node.as_binding_expr() {
            if self.lib_context.contains(lid) {
                // This node is a definition of one of the libs, whose definition we are currently extracting:
                // do not go down this road since it leads to lib definitions using themselves
                self.debug_indented(&format!("encountered banned lib: {lid}"));
                return None;
            }
        }
        // Otherwise: extract all children
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
    ) -> MaybeExpr<Op> {
        if current == node.children().len() {
            // Done with children: add ourselves to the partial expression and return
            let child_ids: Vec<Id> = child_indexes.iter().map(|x| (*x).into()).collect();
            let root = AstNode::new(node.operation().clone(), child_ids);
            partial_expr.push(root);
            Some(partial_expr.into())
        } else {
            // If this is the first child of a lib node (i.e. lib definition) add this lib to the context:
            let old_lib_context = self.lib_context.clone();
            if let Some(BindingExpr::Lib(lid, _, _)) = node.as_binding_expr() {
                if current == 0 {
                    self.debug_indented(&format!(
                        "processing first child of {node:?}, adding {lid} to context"
                    ));
                    self.lib_context.add(lid);
                }
            }

            // Process the current child
            let child = &node.children()[current];
            self.indent += 1;
            self.extract(*child);
            self.indent -= 1;
            // We need to get the result before restoring the context
            let child_res = self.get_from_memo(*child).unwrap().clone();
            // Restore lib context
            self.lib_context = old_lib_context;

            match child_res {
                None => None, // Failed to extract a child, so the extraction of this node fails
                Some(expr) => {
                    // We need to clone the expr because we're going to mutate it (offset child indexes),
                    // and we don't want it to affect the memo result for child.
                    let mut new_expr = expr.as_ref().to_vec();
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

    /// Print a debug message with the current indentation
    /// TODO: this should be a macro
    fn debug_indented(&self, msg: &str) {
        debug!("{:indent$}{msg}", "", indent = 2 * self.indent);
    }
}

/// Cost function that ignores library definitions
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
