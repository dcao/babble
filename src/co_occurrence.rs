//! Co-occurrence analysis on e-graphs.
//! Determines whether (the nodes from) two e-classes can co-occur in any extracted expression.
use crate::ast_node::{Arity, AstNode};
use egg::{Analysis, EGraph, Id, Language};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

/// A binary relation on e-class ids,
/// which says whether they can co-occur in a single extracted program.
/// The relation is symmetric but not reflexive or transitive.
/// In particular, an e-class only co-occurs with itself if there are multiple paths to it from the roots.
#[derive(Debug, Clone)]
pub struct CoOccurrences {
    /// Internal representation which is not symmetrically closed.
    data: HashMap<Id, HashSet<Id>>,
}

impl CoOccurrences {
    /// Are `a` and `b` in the relation?
    #[must_use]
    pub fn may_co_occur(&self, a: Id, b: Id) -> bool {
        // Because data is not symmetrically closed, we need to check both directions:
        self.data.get(&a).map_or(false, |set| set.contains(&b))
            || self.data.get(&b).map_or(false, |set| set.contains(&a))
    }
}

/// Builds a co-occurrence relation for a given e-graph.
/// We assume that the e-graph can have multiple roots (that correspond to different initial programs),
/// and hence all these roots can co-occur,
/// as if they were all children of a single e-node.
#[derive(Debug, Clone)]
pub struct COBuilder<
    'a,
    Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + std::fmt::Display,
    N: Analysis<AstNode<Op>>,
> {
    /// The egraph to analyze
    egraph: &'a EGraph<AstNode<Op>, N>,
    /// Root e-classes
    roots: &'a [Id],
    /// For each e-class, the set of all e-classes reachable from it
    reachable: HashMap<Id, HashSet<Id>>,
    /// For each e-class, which other e-classes it can co-occur with
    co_occurs: HashMap<Id, HashSet<Id>>,
}

impl<'a, Op, N> COBuilder<'a, Op, N>
where
    Op: Clone + std::fmt::Debug + std::hash::Hash + Ord + std::fmt::Display + Arity,
    N: Analysis<AstNode<Op>> + Clone,
{
    /// Initialize a builder
    #[must_use]
    pub fn new(egraph: &'a EGraph<AstNode<Op>, N>, roots: &'a [Id]) -> Self {
        Self {
            egraph,
            roots,
            reachable: HashMap::new(),
            co_occurs: HashMap::new(),
        }
    }

    /// Build the co-occurrence relation
    /// The algorithm is as follows:
    /// - As auxiliary data, we also compute the set of e-classes reachable from each e-class.
    /// - When processing an e-class id, mark all e-classes reachable from it (except itself) as co-occurring (because they can be my descendants).
    /// - When processing an e-node with multiple children, mark any two e-classes reachable from different children as co-occurring (because they can be siblings / uncles).
    #[must_use]
    pub fn run(mut self) -> CoOccurrences {
        self.process_children(self.roots);
        CoOccurrences {
            data: self.co_occurs,
        }
    }

    /// Build the co-occurrence relation for a given e-class and its descendants.
    fn run_from_class(&mut self, id: Id) {
        if self.reachable.get(&id).is_some() {
            // Already visited; we must be in a cycle
            return;
        }
        // Mark as visited
        self.reachable.insert(id, HashSet::new());
        self.co_occurs.insert(id, HashSet::new());
        for node in self.egraph[id].iter() {
            // Compute e-classes reachable from this node
            let reach_node = self.run_from_node(node);
            // Add them to the reachable set of the current e-class
            self.reachable
                .get_mut(&id)
                .unwrap()
                .extend(reach_node.clone());
            // Also add them to the co-occurrence set of the current e-class
            self.co_occurs.get_mut(&id).unwrap().extend(reach_node);
        }
        // Add the current e-class to its own reachable set only afterwards,
        // so that it doesn't end up in its own co-occurrence set by default.
        self.reachable.get_mut(&id).unwrap().insert(id);
    }

    /// Return all e-classes reachable from a given e-node
    /// (and fill in their rechable and co-occurrence sets)
    fn run_from_node(&mut self, node: &AstNode<Op>) -> HashSet<Id> {
        self.process_children(node.children())
    }

    /// Process a list of e-classes that can all co-occur with each other
    fn process_children(&mut self, children: &[Id]) -> HashSet<Id> {
        // Set of e-classes reachable from any of the children
        let mut reach = HashSet::new();
        for id in children {
            self.run_from_class(*id);
            // Mark as co-occurring any pair of e-classes
            // where one is reachable from the current child,
            // and the other one is reachable from one of the previous children (and is therefore in `reach`).
            // This makes our `co_occurs` representation asymmetric,
            // but that's okay since we have a custom getter.
            for r in self.reachable.get(id).unwrap().clone() {
                self.co_occurs.get_mut(&r).unwrap().extend(reach.iter());
            }
            // Finally add the descendants of the current child to `reach`
            reach.extend(self.reachable.get(id).unwrap().iter());
        }
        reach
    }
}
