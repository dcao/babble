//! Anti-unification of e-nodes within an e-graph.

use crate::{
    antiunifiable::Antiunifiable, antiunification::Antiunification, ast_node::AstNode, dfta::Dfta,
    fresh,
};
use egg::{Analysis, EGraph, Id, Language, Pattern, Rewrite};
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, fmt::Debug};
// How does this play into synthesis?
// see https://web.eecs.umich.edu/~xwangsd/pubs/oopsla17.pdf, sec 5.1 for inspo
// Synthesizing a program for a given input-output pair comes down
// to starting with an egraph containing only an output, applying all the rewrites
// until saturation, then finding an expression which a) is written in terms of the
// input, and b) is the smallest/otherwise most optimal program possible.
// To synthesize a program for multiple input-output pairs, we just have to run
// the egraph stuff for each input/output, then get the intersection between all
// egraphs.
// Thus, the interplay between synthesis and library learning comes down to how
// the library learning process interacts w the rewrites used during synthesis:
// 1. When we define functions, this should turn into a rewrite rule.
//    e.g. and True True = True, and _ _ = False
//    becomes rewrite rules
//    and True True => True, and x y => False, True => and True True, False => and x y
//    (we need both for equality - bidirectional!)
// 2. When we learn library functions, we have to add new rewrite rules from the library
//    learned functions to their equivalent expressions:
//    e.g. lets say we learned xor:
//    (or (and ?a (not ?b)) (and (not ?a) ?b)) => (app (fn "xor") ?a ?b)
//    (app (fn "xor") ?a ?b) => (or (and ?a (not ?b)) (and (not ?a) ?b))

/// States in the anti-unified [`Dfta`].
type State = (Id, Id);

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
                    enode.children().iter().map(|id| egraph.find(*id)),
                    egraph.find(eclass.id),
                );
            }
        }
        dfta
    }
}

/// An `AntiUnifier` stores the state of an anti-unification invocation.
/// This struct should take in a single [`EGraph`] with every program (both
/// library functions and synthesis solutions) compiled together.
/// After running anti-unification, we have a new [`EGraph`] which contains
/// all possible libraries we could learn from the
#[derive(Debug)]
pub struct Antiunifier<Op: Antiunifiable> {
    /// A [`Dfta`] representing the [`EGraph`] being anti-unified.
    dfta: Dfta<Op, State>,

    // A map from [`State`]s to their [`Antiunifications`]. Calling
    // [`anti_unify`] computes the anti-unifications for every state, and in the
    // process memoizes them in this map. This uses a [`RefCell`] to simplify
    // modifying it within recursive calls.
    antiunifications_by_state: RefCell<HashMap<State, Vec<Antiunification<Op>>>>,
}

impl<Op: Antiunifiable> Antiunifier<Op> {
    /// Initialize an [`Antiunifier`] from an [`EGraph`]. This builds a [`Dfta`]
    /// representing the given `egraph`.
    pub fn new<A: Analysis<AstNode<Op>>>(egraph: &EGraph<AstNode<Op>, A>) -> Self {
        Self {
            dfta: Dfta::from(egraph).self_intersection(),
            antiunifications_by_state: RefCell::new(HashMap::new()),
        }
    }

    /// Perform anti-unification and return the nontrivial anti-unifications as
    /// [`Rewrite`] rules.
    pub fn anti_unify<A: Analysis<AstNode<Op>>>(&mut self) -> Vec<Rewrite<AstNode<Op>, A>> {
        // TODO: parallelize this?
        // We then enumerate our transitions as well, additionally converting these
        // anti-unifications into rewrites which we will apply to the egraph.
        let mut rewrites: Vec<Rewrite<AstNode<Op>, A>> = Vec::new();
        for state in self.dfta.output_states() {
            self.enumerate(*state);

            for anti_unification in self
                .antiunifications_by_state
                .borrow()
                .get(state)
                .unwrap_or_else(|| unreachable!())
            {
                if !anti_unification.is_trivial() {
                    let searcher: Pattern<AstNode<Op>> = anti_unification.clone().into();
                    let applier: Pattern<AstNode<Op>> = anti_unification
                        .clone()
                        .into_library_fun(fresh::gen("f"))
                        .into();
                    let name = format!("anti-unify {:?}", state);
                    let rewrite =
                        Rewrite::new(name, searcher, applier).unwrap_or_else(|_| unreachable!());
                    rewrites.push(rewrite);
                }
            }
        }
        rewrites
    }

    /// Enumerate the [`Antiunification`]s of a particular [`State`], and update
    /// the `antiunifications_by_state` map accordingly. This modifies `self`,
    /// but uses interior mutability rather than taking a mutable reference
    /// because it simplifies recursive calls.
    fn enumerate(&self, state: State) {
        // If we've already computed the antiunifications of this state, we
        // don't need to do anything.
        if !self.antiunifications_by_state.borrow().contains_key(&state) {
            // We're going to recurse on the arguments of rules leading to this
            // state. We need to mark this state as "computed" so that a loop in
            // the rules doesn't cause infinite recursion. By initially setting
            // the antiunifications of this state to empty, we essentially
            // discard any antiunifications that would come from looping
            // sequences of rules.
            self.antiunifications_by_state
                .borrow_mut()
                .insert(state, Vec::new());

            let mut antiunifications = Vec::new();

            // We first check whether this state is ever an output state.
            if let Some(rules) = self.dfta.get_by_output(&state) {
                for (kind, inputs) in rules {
                    if inputs.is_empty() {
                        antiunifications.push(Antiunification::leaf(kind.clone()));
                    } else {
                        for input in inputs {
                            self.enumerate(*input);
                        }

                        let antiunifications_by_state = self.antiunifications_by_state.borrow();

                        for mut input_antiunifications in inputs
                            .iter()
                            .map(|input| antiunifications_by_state.get(input).unwrap().iter())
                            .multi_cartesian_product()
                            .map(Vec::into_iter)
                        {
                            // Since `input_antiunifications` is the same length
                            // as `inputs`, and we already checked that `inputs`
                            // is nonempty, this unwrap is safe.
                            let mut antiunification =
                                input_antiunifications.next().unwrap().clone();
                            let mut input_indices = vec![antiunification.index()];

                            for input_antiunification in input_antiunifications {
                                let input_index =
                                    antiunification.append(input_antiunification.clone());
                                input_indices.push(input_index);
                            }

                            antiunification
                                .push_ast_node(AstNode::from_parts(kind.clone(), input_indices));
                            antiunifications.push(antiunification);
                        }
                    }
                }
            } else {
                // This state represents a metavariable in the anti-unification.
                antiunifications.push(Antiunification::metavar(state));
            }

            // Finally, memoize the result.
            self.antiunifications_by_state
                .borrow_mut()
                .insert(state, antiunifications);
        }
    }
}

/// Anti-unifies within a given [`EGraph`], returning a [`Vec`] of [`Rewrite`]
/// rules as output.
///
/// # Note
///
/// These rewrites introduce lambdas that they can apply to. That is, a
/// rewrite rule like
/// ```text
/// (+ 1 ?X) -> (lib f0 (lambda (+ 1 $0)) (apply f0 ?X))
/// ```
/// introduces a new expression to the e-graph, but that expression contains as a
/// subexpression `(+ 1 $0)`, which matches the left-hand side of the rewrite.
/// That is, if we have the expression `(+ 1 2)`, applying this rule once will
/// transform it into `(lib f0 (lambda (+ 1 $0)) (apply f0 2))`, and applying it
/// again will transform it into
/// ```text
/// (lib f0 (lambda
///          (lib f0 (lambda (+ 1 $0))
///           (app f0 $0)))
///  (app f0 2))
/// ```
/// This isn't ideal, but e-graphs should help us here and represent all of these
/// new expressions compactly. Nevertheless, it's probably a good idea to only
/// run these rewrites once if possible.
pub fn anti_unify<Op, A>(egraph: &EGraph<AstNode<Op>, A>) -> Vec<Rewrite<AstNode<Op>, A>>
where
    Op: Antiunifiable,
    A: Analysis<AstNode<Op>>,
{
    Antiunifier::new(egraph).anti_unify()
}
