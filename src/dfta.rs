use std::{
    collections::{BTreeSet, HashMap},
    hash::Hash,
    iter::FromIterator,
};

use egg::{Analysis, EGraph, Id};

use crate::{antiunifiable::Antiunifiable, ast_node::AstNode};

// TODO: would encoding this directly as an egraph be more efficient?
/// Conceptually, a DFTA is a list (or rather, a map) of transition rules from
/// an operand and its argument states to an output state. In practice, we
/// store this DFTA in "reverse", as a mapping from the output state to
/// all of the operand/argument pairs which map to it.
///
/// Note that while the transitions and states reference language nodes,
/// these language nodes and the Ids they reference may or may not be
/// in the actual target egraph; indeed, the Ids they reference might be
/// a part of an `IdInterner`, which generates new Ids to stand in for
/// pairs of Ids.
///
/// Additionally, every state contains an implicit transition from "Phi",
/// which denotes that a function argument could be introduced to reach
/// this state. If the state is a stand-in for two different states, Phi
/// should be replaced with values from the first state for the first expr
/// and values from the second state for the right expr.
#[derive(Debug)]
pub(crate) struct Dfta<K, S> {
    by_kind: HashMap<K, BTreeSet<(Vec<S>, S)>>,
    by_output: HashMap<S, BTreeSet<(K, Vec<S>)>>,
}

impl<K, S> Dfta<K, S> {
    /// Create an empty DFTA.
    #[must_use]
    pub(crate) fn new() -> Self {
        Self {
            by_kind: HashMap::new(),
            by_output: HashMap::new(),
        }
    }
}

impl<K, S> Dfta<K, S>
where
    K: Ord + Hash + Clone,
    S: Ord + Hash + Clone,
{
    /// Adds a new transition to the DFTA.
    pub(crate) fn add_rule<I>(&mut self, kind: K, inputs: I, output: S)
    where
        I: IntoIterator<Item = S>,
    {
        let inputs = Vec::from_iter(inputs);
        self.by_kind
            .entry(kind.clone())
            .or_default()
            .insert((inputs.clone(), output.clone()));
        self.by_output
            .entry(output)
            .or_default()
            .insert((kind, inputs));
    }

    /// Returns an iterator over the states in the DFTA.
    pub(crate) fn output_states(&self) -> impl Iterator<Item = &S> {
        self.by_output.keys()
    }

    /// Get all the transitions which have this state as an output.
    #[must_use]
    pub(crate) fn get_by_output(&self, output: &S) -> Option<&BTreeSet<(K, Vec<S>)>> {
        self.by_output.get(output)
    }

    /// Intersect the DFTA with itself to produce a new DFTA over pairs of
    /// states.
    #[must_use]
    pub(crate) fn self_intersection(&self) -> Dfta<K, (S, S)> {
        let mut new_dfta = Dfta::new();
        for (kind, rules) in &self.by_kind {
            for rule1 in rules {
                for (inputs2, output2) in rules.range(rule1..) {
                    let (inputs1, output1) = rule1;
                    let new_inputs = inputs1.iter().cloned().zip(inputs2.iter().cloned());
                    let new_output = (output1.clone(), output2.clone());
                    new_dfta.add_rule(kind.clone(), new_inputs, new_output);
                }
            }
        }
        new_dfta
    }
}

impl<K, S> Default for Dfta<K, S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Op, A> From<&EGraph<AstNode<Op>, A>> for Dfta<Op, Id>
where
    Op: Antiunifiable,
    A: Analysis<AstNode<Op>>,
{
    fn from(egraph: &EGraph<AstNode<Op>, A>) -> Self {
        let mut dfta = Dfta::new();
        for eclass in egraph.classes() {
            for enode in eclass.iter() {
                dfta.add_rule(
                    enode.operation().clone(),
                    enode.iter().map(|id| egraph.find(*id)),
                    egraph.find(eclass.id),
                );
            }
        }
        dfta
    }
}
