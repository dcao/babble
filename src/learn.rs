//! The primary interface for library learning through antiunification.
use crate::{
    ast_node::{Arity, AstNode, PartialExpr},
    dfta::Dfta,
    fresh,
    teachable::Teachable,
};
use egg::{Analysis, EGraph, Id, Pattern, Rewrite, Symbol, Var};
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
};

/// A `LearnedLibrary<Op>` is a collection of functions learned from an
/// [`EGraph<AstNode<Op>, _>`] by antiunifying pairs of enodes to find their
/// common structure.
///
/// You can create a `LearnedLibrary` using [`LearnedLibrary::from(&your_egraph)`].
#[derive(Debug, Clone)]
pub struct LearnedLibrary<Op, T> {
    /// A map from DFTA states (i.e. pairs of enodes) to their antiunifications.
    aus_by_state: HashMap<T, HashSet<PartialExpr<Op, T>>>,
    /// A set of all the nontrivial antiunifications discovered.
    nontrivial_aus: HashSet<PartialExpr<Op, Var>>,
}

impl<Op, T> Default for LearnedLibrary<Op, T> {
    /// Create an empty learned library.
    fn default() -> Self {
        Self {
            aus_by_state: HashMap::new(),
            nontrivial_aus: HashSet::new(),
        }
    }
}

impl<'a, Op, A> From<&'a EGraph<AstNode<Op>, A>> for LearnedLibrary<Op, (Id, Id)>
where
    Op: Teachable,
    A: Analysis<AstNode<Op>>,
{
    /// Constructs a [`LearnedLibrary`] from an [`EGraph`] by antiunifying pairs of
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

impl<Op, T> From<Dfta<Op, T>> for LearnedLibrary<Op, T>
where
    Op: Teachable,
    T: Ord + Hash + Clone,
{
    /// Constructs a [`LearnedLibrary`] from an [`EGraph`] by antiunifying pairs of
    /// enodes to find their common structure.
    fn from(dfta: Dfta<Op, T>) -> Self {
        let mut learned_lib = Self::default();
        for state in dfta.output_states() {
            learned_lib.enumerate(&dfta, state);
        }
        learned_lib
    }
}

impl<Op: Teachable, T> LearnedLibrary<Op, T> {
    /// Returns an iterator over rewrite rules that replace expressions with
    /// equivalent calls to a learned library function.
    ///
    /// For example, the expression
    ///
    /// ```text
    /// (* (+ 1 2) 5)
    /// ```
    ///
    /// might be rewritten to
    ///
    /// ```text
    /// (lib f (lambda (* (+ 1 $0) 5))
    ///  (apply f 2))
    /// ```
    ///
    /// by a rewrite rule
    ///
    /// ```text
    /// (* (+ 1 ?x) 5) => (lib f (lambda (* (+ 1 $0) 5)) (apply f ?x))
    /// ```
    pub fn rewrites<A: Analysis<AstNode<Op>>>(
        &self,
    ) -> impl Iterator<Item = Rewrite<AstNode<Op>, A>> + '_
    where
        Op: Display,
    {
        self.nontrivial_aus.iter().enumerate().map(|(i, au)| {
            let searcher: Pattern<_> = au.clone().into();
            let applier: Pattern<_> = reify(au.clone(), fresh::gen("f")).into();
            let name = format!("anti-unify {}", i);
            eprintln!("{}: {} => {}", name, searcher, applier);

            // Both patterns contain the same variables, so this can never fail.
            Rewrite::new(name, searcher, applier).unwrap_or_else(|_| unreachable!())
        })
    }
}

impl<Op, T> LearnedLibrary<Op, T>
where
    Op: Eq + Hash + Clone + Arity,
    T: Eq + Hash + Clone,
{
    /// Computes the antiunifications of `state` in the DFTA `dfta`.
    fn enumerate(&mut self, dfta: &Dfta<Op, T>, state: &T) {
        if self.aus_by_state.contains_key(state) {
            // We've already enumerated this state, so there's nothing to do.
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
        self.aus_by_state.insert(state.clone(), HashSet::new());
        let mut aus: HashSet<PartialExpr<Op, T>> = HashSet::new();

        if let Some(rules) = dfta.get_by_output(state) {
            for (operation, inputs) in rules {
                if inputs.is_empty() {
                    aus.insert(AstNode::leaf(operation.clone()).into());
                } else {
                    // Recursively enumerate the inputs to this rule.
                    for input in inputs {
                        self.enumerate(dfta, input);
                    }

                    // For a rule `op(s1, ..., sn) -> state`, we add an
                    // antiunification of the form `(op a1 ... an)` for every
                    // combination `a1, ..., an` of antiunifications of the
                    // input states `s1, ..., sn`, i.e., for every `(a1, ..., an)`
                    // in the cartesian product
                    // `antiunifications_by_state[s1] × ... × antiunifications_by_state[sn]`
                    let new_aus = inputs
                        .iter()
                        .map(|input| self.aus_by_state[input].iter().cloned())
                        .multi_cartesian_product()
                        .map(|inputs| AstNode::new(operation.clone(), inputs).into());

                    aus.extend(new_aus);
                }
            }

            // We filter out the anti-unifications which are just concrete
            // expressions with no variables, and then convert the contained
            // states to pattern variables. The conversion takes
            // alpha-equivalent anti-unifications to the same value, effectively
            // discarding redundant anti-unifications.
            let nontrivial_aus = aus
                .iter()
                .filter(|au| au.has_holes())
                .cloned()
                .map(normalize);

            self.nontrivial_aus.extend(nontrivial_aus);
        } else {
            // This state isn't the output of any rules, so we treat it as a
            // metavariable. We don't do this for states that are rule outputs
            // to ensure we're getting only the "least general generalization",
            // rather than all generalizations.
            aus.insert(PartialExpr::Hole(state.clone()));
        }

        *self.aus_by_state.get_mut(state).unwrap() = aus;
    }
}

/// Replaces the metavariables in an anti-unification with pattern variables.
/// Normalizing alpha-equivalent anti-unifications produces identical
/// anti-unifications.
#[must_use]
fn normalize<Op, T: Eq>(au: PartialExpr<Op, T>) -> PartialExpr<Op, Var> {
    let mut metavars = Vec::new();
    let to_var = |metavar| {
        let index = metavars
            .iter()
            .position(|other| other == &metavar)
            .unwrap_or_else(|| {
                metavars.push(metavar);
                metavars.len() - 1
            });

        let var = format!("?x{}", index)
            .parse()
            .unwrap_or_else(|_| unreachable!());
        PartialExpr::Hole(var)
    };
    au.fill(to_var)
}

/// Converts an anti-unification into a partial expression which defines a new
/// named function and applies it to the metavariables in the anti-unification.
/// The new function reifies the anti-unification, replacing metavariables by
/// lambda arguments.
///
/// For example, the anti-unification
///
/// ```text
/// (* ?x (+ ?y 1))
/// ```
///
/// would be converted to the partial expression
///
/// ```text
/// (lib foo (lambda (lambda (* $0 (+ $1 1))))
///  (apply (apply foo ?y) ?x))
/// ```
///
/// assuming `name` is "foo".
#[must_use]
fn reify<Op: Teachable, T: Eq>(au: PartialExpr<Op, T>, name: Symbol) -> PartialExpr<Op, T> {
    let mut metavars = Vec::new();

    // Replace every metavariable in this antiunification with a de
    // Bruijn-indexed variable.
    // Metavariables might be located inside lambdas. To deal with this,
    // the de Brujin index that we return is equal to the index of the
    // metavar, added to however many lambdas wrap the metavar at that
    // point.
    let mut fun = au.fill_with_binders(|metavar, binders| {
        let index = metavars
            .iter()
            .position(|other| other == &metavar)
            .unwrap_or_else(|| {
                metavars.push(metavar);
                metavars.len() - 1
            })
            + binders;
        PartialExpr::Hole(index)
    });

    let offset = metavars.len();

    fun = fun.map_leaves_with_binders(|op, binders| match op.var_index() {
        Some(index) if index > binders => Op::var(index + offset).into(),
        _ => AstNode::leaf(op).into(),
    });

    let mut fun = fun.fill(|index| Op::var(index).into());

    // Wrap that in a lambda-abstraction, one for each variable we introduced.
    for _ in 0..metavars.len() {
        fun = Op::lambda(fun).into();
    }

    // Now apply the new function to the metavariables in reverse order so they
    // match the correct de Bruijn indexed variable.
    let mut body = PartialExpr::Node(Op::ident(name));
    while let Some(metavar) = metavars.pop() {
        body = Op::apply(body, PartialExpr::Hole(metavar)).into();
    }

    let ident = PartialExpr::Node(Op::ident(name));
    Op::lib(ident, fun, body).into()
}
