//! The primary interface for library learning through antiunification.
//!
//! The antiunification algorithm is as follows: For each pair of eclasses (a, b),
//! we store a set of partial expressions AU(a, b). Partial expressions are
//! expressions where some of the sub-expressions may be replaced by another
//! data type. For example, a pattern is (equivalent to) a partial expression
//! over variables. In this algorithm, we use partial expressions over pairs of
//! enodes.
//!
//! To compute the set AU(a, b): For each pair of enodes with matching operators
//! and arities op(x1, ..., xn) \in a and op(y1, ..., yn) \in b, we recursively
//! compute AU(x1, y1), ..., AU(xn, yn). Then, for every n-tuple of partial
//! expressions (z1, ..., zn) with z1 \in AU(x1, y1), ..., zn \in AU(xn, yn), we
//! add the partial expression op(z1, ..., zn) to the set AU(a, b).
//! If the set AU(a, b) is empty, we add to it the partial expression (a, b).
use crate::{
    ast_node::{Arity, AstNode, PartialExpr},
    dfta::Dfta,
    teachable::{BindingExpr, Teachable},
};
use egg::{Analysis, EGraph, Id, Language, Pattern, Rewrite, Var};
use itertools::Itertools;
use log::debug;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
    num::ParseIntError,
    str::FromStr,
};
use thiserror::Error;

/// A library function's name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LibId(pub usize);

impl Display for LibId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "l{}", self.0)
    }
}

/// An error when parsing a LibId.
#[derive(Clone, Debug, Error)]
pub enum ParseLibIdError {
    /// The string did not start with "$"
    #[error("expected de Bruijn index to start with 'l")]
    NoLeadingL,
    /// The index is not a valid unsigned integer
    #[error(transparent)]
    InvalidIndex(ParseIntError),
}

impl FromStr for LibId {
    type Err = ParseLibIdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(n) = s.strip_prefix('l') {
            let n = n.parse().map_err(ParseLibIdError::InvalidIndex)?;
            Ok(LibId(n))
        } else {
            Err(ParseLibIdError::NoLeadingL)
        }
    }
}

/// A `LearnedLibrary<Op>` is a collection of functions learned from an
/// [`EGraph<AstNode<Op>, _>`] by antiunifying pairs of enodes to find their
/// common structure.
///
/// You can create a `LearnedLibrary` using [`LearnedLibrary::from(&your_egraph)`].
#[derive(Debug, Clone)]
pub struct LearnedLibrary<Op, T> {
    /// A map from DFTA states (i.e. pairs of enodes) to their antiunifications.
    aus_by_state: BTreeMap<T, BTreeSet<PartialExpr<Op, T>>>,
    /// A set of all the nontrivial antiunifications discovered.
    nontrivial_aus: BTreeSet<PartialExpr<Op, Var>>,
    /// Whether to also learn "library functions" which take no arguments.
    learn_constants: bool,
}

impl<'a, Op> LearnedLibrary<Op, (Id, Id)>
where
    Op: Arity + Clone + Debug + Ord,
    AstNode<Op>: Language,
{
    /// Constructs a [`LearnedLibrary`] from an [`EGraph`] by antiunifying pairs of
    /// enodes to find their common structure.
    pub fn new<A: Analysis<AstNode<Op>>>(
        egraph: &'a EGraph<AstNode<Op>, A>,
        learn_constants: bool,
    ) -> Self {
        let mut learned_lib = Self {
            aus_by_state: BTreeMap::new(),
            nontrivial_aus: BTreeSet::new(),
            learn_constants,
        };
        let dfta = Dfta::from(egraph).cross_over();
        for state in dfta.output_states() {
            learned_lib.enumerate(&dfta, state);
        }
        learned_lib
    }
}

impl<Op, T> LearnedLibrary<Op, T>
where
    Op: Arity + Clone + Display + Ord + Send + Sync + Teachable + 'static,
    AstNode<Op>: Language,
{
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
    ) -> impl Iterator<Item = Rewrite<AstNode<Op>, A>> + '_ {
        self.nontrivial_aus.iter().enumerate().map(|(i, au)| {
            let searcher: Pattern<_> = au.clone().into();
            let applier: Pattern<_> = reify(LibId(i), au.clone()).into();
            let name = format!("anti-unify {}", i);
            debug!("Found rewrite \"{}\":\n{} => {}", name, searcher, applier);

            // Both patterns contain the same variables, so this can never fail.
            Rewrite::new(name, searcher, applier).unwrap_or_else(|_| unreachable!())
        })
    }

    pub fn libs(&self) -> impl Iterator<Item = Pattern<AstNode<Op>>> + '_ {
        self.nontrivial_aus.iter().enumerate().map(|(i, au)| {
            let applier: Pattern<_> = reify(LibId(i), au.clone()).into();
            applier
        })
    }
}

impl<Op, T> LearnedLibrary<Op, T>
where
    Op: Arity + Clone + Debug + Ord,
    T: Clone + Ord,
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
        self.aus_by_state.insert(state.clone(), BTreeSet::new());
        let mut aus: BTreeSet<PartialExpr<Op, T>> = BTreeSet::new();

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
        }

        if aus.is_empty() {
            aus.insert(PartialExpr::Hole(state.clone()));
        } else {
            // We filter out the anti-unifications which are just concrete
            // expressions with no variables, and then convert the contained
            // states to pattern variables. The conversion takes
            // alpha-equivalent anti-unifications to the same value, effectively
            // discarding redundant anti-unifications.

            let learn_constants = self.learn_constants;

            let nontrivial_aus = aus
                .iter()
                .filter(|au| learn_constants || au.has_holes())
                .cloned()
                .map(normalize)
                .filter_map(|(au, num_vars)| {
                    // Here we filter out rewrites that don't actually simplify
                    // anything. We say that an AU rewrite simplifies an
                    // expression if it replaces that expression with a function
                    // call that is strictly smaller than the original
                    // expression.
                    //
                    // The size of a function call `f e_1 ... e_n` is size(e1) +
                    // ... + size(e_n) + n + 1, as there are n applications and
                    // the function's identifier `f`.
                    //
                    // The size of an expression e containing n subexpressions
                    // e_1, ..., e_n is k_1 * size(e_1) + ... + k_n * size(e_n)
                    // + size(e[x_1/e_1, ..., x_n/e_n]) - (k_1 + ... + k_n),
                    // where k_i is the number of times e_i appears in e and
                    // x_1, ..., x_n are variables.
                    //
                    // Because size(e_i) can be arbitrarily large, if any
                    // variable k_i is greater than 1, the difference in size
                    // between the function call and the original expression can
                    // also be arbitrarily large. Otherwise, if k_1 = ... = k_n
                    // = 1, the rewrite can simplify an expression if and only
                    // if size(e[x_1/e_1, ..., x_n/e_n]) > 2n + 1. This
                    // corresponds to an anti-unification containing at least n
                    // + 1 nodes.
                    if num_vars < au.num_holes() || au.num_nodes() > num_vars + 1 {
                        Some(au)
                    } else {
                        None
                    }
                });

            self.nontrivial_aus.extend(nontrivial_aus);
        }

        *self.aus_by_state.get_mut(state).unwrap() = aus;
    }
}

/// Replaces the metavariables in an anti-unification with pattern variables.
/// Normalizing alpha-equivalent anti-unifications produces identical
/// anti-unifications. Returns a pair of the anti-unification and the number of
/// unique variables it contains.
#[must_use]
fn normalize<Op, T: Eq>(au: PartialExpr<Op, T>) -> (PartialExpr<Op, Var>, usize) {
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
    let normalized = au.fill(to_var);
    (normalized, metavars.len())
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
/// (lib (lambda (lambda (* $0 (+ $1 1))))
///  (apply (apply $0 ?y) ?x))
/// ```
///
/// assuming `name` is "foo".
#[must_use]
fn reify<Op, T>(ix: LibId, au: PartialExpr<Op, T>) -> PartialExpr<Op, T>
where
    Op: Arity + Teachable,
    T: Eq,
{
    let mut metavars = Vec::new();

    // Replace every metavariable in this antiunification with a de
    // Bruijn-indexed variable.
    // Metavariables might be located inside lambdas. To deal with this,
    // the de Brujin index that we return is equal to the index of the
    // metavar, added to however many lambdas wrap the metavar at that
    // point.
    let mut fun = au.fill_with_binders(|metavar, num_binders| {
        let index = metavars
            .iter()
            .position(|other: &(T, usize)| other.0 == metavar)
            .unwrap_or_else(|| {
                metavars.push((metavar, num_binders));
                metavars.len() - 1
            });
        let index = index + num_binders;

        let mut res = PartialExpr::Hole(index);

        for i in (0..num_binders).rev() {
            res = Op::apply(res, Op::var(i).into()).into();
        }

        res
    });

    // foo (\. \. $0 $2 ?hole) => foo (\. \. $0 $2 ?$2)
    //                                          ^ binders = 2

    // All the function variables
    let offset = metavars.len();

    let mut max_locals = 0;

    fun = fun.map_leaves_with_binders(|node, binders| match node.as_binding_expr() {
        Some(BindingExpr::Var(index)) if index.0 >= binders => {
            max_locals = std::cmp::max(max_locals, index.0 - binders + 1);
            Op::var(index.0 + offset).into()
        }
        _ => node.into(),
    });

    // foo (\. \. $0 $2 ?$2) => foo (\. \. $0 $3 ?$2)

    let mut fun = fun.fill(|index| Op::var(index).into());

    // Wrap that in a lambda-abstraction, one for each variable we introduced.
    for _ in 0..(metavars.len() + max_locals) {
        fun = Op::lambda(fun).into();
    }

    // Now apply the new function to the metavariables in reverse order so they
    // match the correct de Bruijn indexed variable.
    let mut body = Op::lib_var(ix).into();
    while let Some((metavar, binders)) = metavars.pop() {
        let mut fn_arg = PartialExpr::Hole(metavar);
        for _i in 0..binders {
            fn_arg = Op::lambda(fn_arg).into();
        }
        body = Op::apply(body, fn_arg).into();
    }

    for index in 0..max_locals {
        body = Op::apply(body, Op::var(index).into()).into();
    }

    PartialExpr::Node(BindingExpr::Lib(ix, fun, body).into())
}
