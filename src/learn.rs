//! The primary interface for library learning through antiunification.
use crate::{
    antiunifiable::Antiunifiable, antiunification::Antiunification, ast_node::AstNode, dfta::Dfta,
    fresh,
};
use egg::{Analysis, EGraph, Id, Pattern, Rewrite};
use itertools::Itertools;
use std::{collections::HashMap, fmt::Debug};

/// States in the anti-unified [`Dfta`].
type State = (Id, Id);

/// A `LearnedLibrary<Op>` is a collection of functions learned from an
/// [`EGraph<AstNode<Op>, _>`] by antiunifying pairs of enodes to find their
/// common structure.
///
/// You can create a `LearnedLibrary` using [`LearnedLibrary::from(&some_egraph)`].
#[derive(Debug, Clone)]
pub struct LearnedLibrary<Op> {
    /// A map from DFTA states (i.e. pairs of enodes) to their
    /// [`Antiunification`]s.
    antiunifications_by_state: HashMap<State, Vec<Antiunification<Op>>>,
}

impl<Op> Default for LearnedLibrary<Op> {
    /// Create an empty learned library.
    fn default() -> Self {
        Self {
            antiunifications_by_state: HashMap::new(),
        }
    }
}

impl<'a, Op, A> From<&'a EGraph<AstNode<Op>, A>> for LearnedLibrary<Op>
where
    Op: Antiunifiable,
    A: Analysis<AstNode<Op>>,
{
    /// Construct a `LearnedLibrary` from an [`EGraph`] by antiunifying pairs of
    /// enodes to find their common structure.
    fn from(egraph: &'a EGraph<AstNode<Op>, A>) -> Self {
        let mut learned_lib = Self::default();
        let dfta = Dfta::from(egraph).self_intersection();
        for state in dfta.output_states() {
            learned_lib.enumerate(&dfta, *state);
        }
        learned_lib
    }
}

impl<Op: Antiunifiable> LearnedLibrary<Op> {
    /// Returns an iterator over rewrite rules that replace expressions with
    /// equivalent calls to a learned library function.
    #[must_use]
    pub fn rewrites<A: Analysis<AstNode<Op>>>(
        &self,
    ) -> impl Iterator<Item = Rewrite<AstNode<Op>, A>> + '_ {
        self.antiunifications_by_state
            .iter()
            .flat_map(|(state, antiunifications)| {
                antiunifications
                    .iter()
                    .filter(|antiunification| !antiunification.is_trivial())
                    .cloned()
                    .enumerate()
                    .map(move |(i, antiunification)| {
                        let searcher: Pattern<AstNode<Op>> = antiunification.clone().into();
                        let applier: Pattern<AstNode<Op>> =
                            antiunification.into_library_fun(fresh::gen("f")).into();
                        let name = format!("anti-unify {:?} {}", state, i);
                        Rewrite::new(name, searcher, applier).unwrap_or_else(|_| unreachable!())
                    })
            })
    }

    /// Compute the antiunifications of `state` in the DFTA `dfta` and update
    /// `self.antiunifications_by_state` accordingly.
    fn enumerate(&mut self, dfta: &Dfta<Op, State>, state: State) {
        // If we've already computed the antiunifications of this state, we
        // don't need to do anything.
        if self.antiunifications_by_state.contains_key(&state) {
            return;
        }

        // We're going to recursively compute the antiunifications of the inputs
        // of all of the rules leading to this state. Before we do, we need to
        // mark this state as in progress so that a loop in the rules doesn't
        // cause infinite recursion.
        //
        // By initially setting the antiunifications of this state to empty, we
        // exclude any antiunifications that would come from looping sequences
        // of rules.
        self.antiunifications_by_state.insert(state, Vec::new());

        let mut antiunifications = Vec::new();

        if let Some(rules) = dfta.get_by_output(&state) {
            for (operation, inputs) in rules {
                if inputs.is_empty() {
                    antiunifications.push(Antiunification::leaf(operation.clone()));
                } else {
                    for input in inputs {
                        self.enumerate(dfta, *input);
                    }

                    // For a rule `op(s1, ..., sn) -> state`, we add an
                    // antiunification of the form `(op a1 ... an)` for every
                    // combination `a1, ..., an` of antiunifications of the
                    // input states `s1, ..., sn`, i.e., for every `(a1, ..., an)`
                    // in the cartesian product
                    // `antiunifications_by_state[s1] × ... × antiunifications_by_state[sn]`
                    antiunifications.extend(
                        inputs
                            .iter()
                            .map(|input| self.antiunifications_by_state[input].iter())
                            .multi_cartesian_product()
                            .map(|inputs| {
                                Antiunification::expr(
                                    operation.clone(),
                                    inputs.into_iter().cloned(),
                                )
                            }),
                    );
                }
            }
        } else {
            // This state isn't the output of any rules, so we treat it as a
            // metavariable. We don't do this for states that are rule outputs
            // to ensure we're getting only the "least general generalization",
            // rather than all generalizations.
            antiunifications.push(Antiunification::metavar(state));
        }

        self.antiunifications_by_state
            .insert(state, antiunifications);
    }
}
