//! The primary interface for library learning through antiunification.
use crate::{
    ast_node::{AstNode, PartialExpr},
    dfta::Dfta,
    fresh,
    teachable::Teachable,
};
use egg::{Analysis, EGraph, Id, Pattern, Rewrite, Symbol, Var};
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

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
    antiunifications_by_state: HashMap<State, Vec<PartialExpr<Op, State>>>,
    antiunifications: HashSet<PartialExpr<Op, Var>>,
}

impl<Op> Default for LearnedLibrary<Op> {
    /// Create an empty learned library.
    fn default() -> Self {
        Self {
            antiunifications_by_state: HashMap::new(),
            antiunifications: HashSet::new(),
        }
    }
}

impl<'a, Op, A> From<&'a EGraph<AstNode<Op>, A>> for LearnedLibrary<Op>
where
    Op: Teachable,
    A: Analysis<AstNode<Op>>,
{
    /// Construct a `LearnedLibrary` from an [`EGraph`] by antiunifying pairs of
    /// enodes to find their common structure.
    fn from(egraph: &'a EGraph<AstNode<Op>, A>) -> Self {
        let mut learned_lib = Self::default();
        let dfta = Dfta::from(egraph).cross_over();
        for state in dfta.output_states() {
            learned_lib.enumerate(&dfta, state);
        }
        learned_lib
    }
}

impl<Op: Teachable> LearnedLibrary<Op> {
    /// Returns an iterator over rewrite rules that replace expressions with
    /// equivalent calls to a learned library function.
    pub fn rewrites<A: Analysis<AstNode<Op>>>(
        &self,
    ) -> impl Iterator<Item = Rewrite<AstNode<Op>, A>> + '_
    where
        Op: Display,
    {
        self.antiunifications
            .iter()
            .enumerate()
            .map(|(i, antiunification)| {
                let searcher: Pattern<_> = antiunification.clone().into();
                let applier: Pattern<_> = library_fun(antiunification.clone(), fresh::gen("f")).into();
                let name = format!("anti-unify {}", i);
                eprintln!("{}: {} -> {}", name, searcher, applier);
                Rewrite::new(name, searcher, applier).unwrap()
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

        if let Some(rules) = dfta.get_by_output(state) {
            for (operation, inputs) in rules {
                if inputs.is_empty() {
                    antiunifications.push(PartialExpr::leaf(operation.clone()));
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
                                PartialExpr::node(operation.clone(), inputs.into_iter().cloned())
                            }),
                    );
                }
            }

            self.antiunifications.extend(
                antiunifications
                    .iter()
                    .filter(|au| !au.is_expr())
                    .cloned()
                    .map(replace_states),
            );
        } else {
            // This state isn't the output of any rules, so we treat it as a
            // metavariable. We don't do this for states that are rule outputs
            // to ensure we're getting only the "least general generalization",
            // rather than all generalizations.
            antiunifications.push(PartialExpr::Hole(state));
        }

        self.antiunifications_by_state
            .insert(state, antiunifications);
    }
}

#[must_use]
fn replace_states<Op>(partial_expr: PartialExpr<Op, State>) -> PartialExpr<Op, Var> {
    let mut states = Vec::new();
    let state_to_var = |tag| {
        let index = states
            .iter()
            .position(|&state| state == tag)
            .unwrap_or_else(|| {
                states.push(tag);
                states.len() - 1
            });
        let var = format!("?x{}", index).parse().unwrap();
        PartialExpr::Hole(var)
    };
    partial_expr.fill(state_to_var)
}

/// Create a new anti-unification from this one by introducing a named
/// library function and applying it to each of the metavariables.
///
/// This transforms an anti-unification like `(+ ?X ?Y)` into the
/// anti-unification
/// ```text
/// (lib f0 (lambda (lambda (+ $0 $1)))
///   (apply (apply f0 ?Y) ?X))
/// ```
#[must_use]
fn library_fun<Op: Teachable>(
    partial_expr: PartialExpr<Op, Var>,
    name: Symbol,
) -> PartialExpr<Op, Var> {
    let mut vars = Vec::new();
    // We start by replacing every metavariable in this antiunification with
    // a de Bruijn-indexed variable whose index is the position of the
    // metavariable in `metavars`.
    let mut fun = partial_expr.fill(|var| {
        let index = vars.iter().position(|&v| v == var).unwrap_or_else(|| {
            vars.push(var);
            vars.len() - 1
        });
        Op::var(index).into()
    });

    // Now add a lambda for each variable.
    for _ in 0..vars.len() {
        fun = PartialExpr::Node(Op::lambda(fun));
    }

    let ident: PartialExpr<Op, Var> = PartialExpr::Node(Op::ident(name));

    // We apply that named library function to each of the metavariables, in
    // reverse order of their de Bruijn indices.
    let mut body = ident.clone();
    for i in 1..=vars.len() {
        let var = vars[vars.len() - i];
        body = PartialExpr::Node(Op::apply(body, PartialExpr::Hole(var)));
    }

    PartialExpr::Node(Op::lib(ident, fun, body))
}
