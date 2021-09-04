//! Anti-unification of e-nodes within an e-graph.

use crate::{
    dfta::Dfta,
    expr::{Arity, Expr},
    fresh,
};
use egg::{Analysis, EGraph, ENodeOrVar, Id, Language, Pattern, RecExpr, Rewrite, Symbol, Var};
use std::{cell::RefCell, collections::HashMap, fmt::Debug, hash::Hash};
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

type State = (Id, Id);

impl<K, A> From<&EGraph<Expr<K>, A>> for Dfta<K, Id>
where
    K: Antiunifiable,
    A: Analysis<Expr<K>>,
{
    fn from(egraph: &EGraph<Expr<K>, A>) -> Self {
        let mut dfta = Dfta::new();
        for eclass in egraph.classes() {
            for enode in eclass.iter() {
                dfta.add_rule(
                    enode.kind().clone(),
                    enode.children().iter().map(|id| egraph.find(*id)),
                    egraph.find(eclass.id),
                );
            }
        }
        dfta
    }
}

/// An `AntiUnifTgt` is an extension of a Language which has constructs to
/// introduce lambdas, etc.
pub trait Antiunifiable: Debug + Clone + Ord + Hash + Arity + Send + Sync + 'static {
    /// Return a language node representing a lambda abstraction over some
    /// body.
    fn lambda() -> Self;

    /// Return a language node representing an application of a function
    /// to an argument.
    fn apply() -> Self;

    /// Return a node representing a de Brujin index for a lambda.
    fn arg(index: usize) -> Self;

    /// Return a node representing a reference to a named fn.
    fn ident(name: Symbol) -> Self;

    /// Return a node representing a new learned library fn.
    fn lib() -> Self;
}

// TODO: this seems inefficient
// some way of making our own custom Searcher?
/// An `AntiUnification` is an enumerated anti-unification, generated
/// by enumerating programs of a given Dfta state. It consists of two
/// parts: a generated pattern ast with metavariables where the phi
/// transitions are and a memoized map of the arguments of the output lambda.
/// We don't record the output lambda since we can just generate it from the
/// pattern ast (replace metavars with lambda args in the right places according
/// to the arg map), and since it makes extension complicated.
#[derive(Debug, Clone)]
pub struct Antiunification<K> {
    // We use a Vec here, but this is equivalent to a RecExpr.
    /// The search pattern of this anti-unification
    pub pattern: Vec<ENodeOrVar<Expr<K>>>,

    /// A sorted list of phi Ids, which will eventually become arguments to
    /// lambdas. At the end, we generate de Brujin indices by getting the
    /// index of each Id in this list.
    pub args: Vec<Var>,

    phi: bool,
}

impl<K> Default for Antiunification<K> {
    fn default() -> Self {
        Self {
            pattern: Vec::new(),
            args: Vec::new(),
            phi: false,
        }
    }
}

impl<K: Antiunifiable> Antiunification<K> {
    /// Creates a new empty anti-unification
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a phi anti-unification
    #[must_use]
    pub fn phi(state: State) -> Self {
        let var = format!("?x{}_{}", state.0, state.1)
            .parse()
            .unwrap_or_else(|_| unreachable!());
        Self {
            pattern: vec![ENodeOrVar::Var(var)],
            args: vec![var],
            phi: true,
        }
    }

    /// Checks if this is a phi anti-unif.
    #[must_use]
    pub fn is_invalid(&self) -> bool {
        self.phi || self.args.is_empty()
    }

    /// Extends this anti-unification with another anti-unification; i.e. adds
    /// another anti-unified program at the end of the current pattern.
    /// The length of the pattern ast - 1 will be the index of the program
    /// that was added.
    pub fn extend(&mut self, other: &Self) {
        // First, extend the pattern AST.
        // The delta we need to add to the items in the other
        // pattern AST is equal to the current length of the pattern.
        let delta = self.pattern.len();

        for mut enode_or_var in other.pattern.iter().cloned() {
            for child in enode_or_var.children_mut() {
                *child = (usize::from(*child) + delta).into();
            }
            self.pattern.push(enode_or_var);
        }

        // Then, extend the args list by doing binary insertion of each arg
        // in the other into our vec.
        for arg in &other.args {
            if let Err(i) = self.args.binary_search(arg) {
                self.args.insert(i, *arg);
            }
        }
    }

    /// Turns this anti-unification into a lambda, applied to each of the args.
    #[must_use]
    pub fn lambdify(&self) -> Vec<ENodeOrVar<Expr<K>>> {
        // We first create a map from the stringified Id to its de Brujin index.
        let mut lambda = vec![];
        let arg_map: HashMap<Var, _> = self
            .args
            .iter()
            .enumerate()
            .map(|(i, x)| (*x, i))
            .collect::<HashMap<_, _>>();

        // We first add all the nodes from our pattern ast to our result
        // expression. If it's an ENode, we insert as-is. If it's a var, we
        // turn it into a lambda argument.
        for node in &self.pattern {
            lambda.push(match node {
                ENodeOrVar::ENode(n) => ENodeOrVar::ENode(n.clone()),
                ENodeOrVar::Var(var) => {
                    ENodeOrVar::ENode(Expr::from_parts(K::arg(arg_map[var]), []))
                }
            });
        }

        // We then introduce as many lambdas as is needed to cover our
        // args.
        for _ in 0..self.args.len() {
            lambda.push(ENodeOrVar::ENode(Expr::from_parts(
                K::lambda(),
                [(lambda.len() - 1).into()],
            )));
        }
        let fn_id = lambda.len() - 1;

        // Push a symbol representing our hashed function to the lambda.
        let sym_id = lambda.len();
        lambda.push(ENodeOrVar::ENode(Expr::from_parts(
            K::ident(fresh::gen("f")),
            [],
        )));

        // Then we introduce applications
        // Make sure we iterate in the right order for our de brujin indices :))
        for arg in self.args.iter().copied().rev() {
            lambda.push(ENodeOrVar::Var(arg));
            lambda.push(ENodeOrVar::ENode(Expr::from_parts(
                K::apply(),
                [(lambda.len() - 2).into(), (lambda.len() - 1).into()],
            )));
        }

        // Finally, introduce a let.
        lambda.push(ENodeOrVar::ENode(Expr::from_parts(
            K::lib(),
            [sym_id.into(), fn_id.into(), (lambda.len() - 1).into()],
        )));

        lambda
    }
}

/// An `AntiUnifier` stores the state of an anti-unification invocation.
/// This struct should take in a single egraph with every program (both
/// library functions and synthesis solutions) compiled together.
/// After running anti-unification, we have a new egraph which contains
/// all possible libraries we could learn from the
#[derive(Debug)]
pub struct Antiunifier<K: Antiunifiable> {
    dfta: Dfta<K, State>,

    // Memoization for enumeration of anti-unified programs
    memo: RefCell<HashMap<State, Vec<Antiunification<K>>>>,
}

impl<K: Antiunifiable> Antiunifier<K> {
    /// Initialize an `AntiUnifier` from an `EGraph`.
    /// We first rebuild this egraph to make sure all its invariants hold.
    /// We then create a DFTA from it which we will use for anti-unification
    /// work.
    pub fn new<A: Analysis<Expr<K>>>(egraph: &EGraph<Expr<K>, A>) -> Self {
        Self {
            dfta: Dfta::from(egraph).self_intersection(),
            memo: RefCell::new(HashMap::new()),
        }
    }

    /// Perform anti-unification.
    pub fn anti_unify<A: Analysis<Expr<K>>>(&mut self) -> Vec<Rewrite<Expr<K>, A>> {
        // TODO: parallelize this?
        // We then enumerate our transitions as well, additionally converting these
        // anti-unifications into rewrites which we will apply to the egraph.
        let mut rewrites: Vec<Rewrite<Expr<K>, A>> = Vec::new();
        for state in self.dfta.output_states() {
            self.enumerate(*state);

            for anti_unification in self
                .memo
                .borrow()
                .get(state)
                .unwrap_or_else(|| unreachable!())
            {
                if anti_unification.is_invalid() {
                    continue;
                }
                let searcher: Pattern<Expr<K>> =
                    RecExpr::from(anti_unification.pattern.clone()).into();
                let applier: Pattern<Expr<K>> = RecExpr::from(anti_unification.lambdify()).into();
                let name = format!("anti-unify {:?}", state);
                let rewrite =
                    Rewrite::new(name, searcher, applier).unwrap_or_else(|_| unreachable!());
                rewrites.push(rewrite);
            }
        }
        rewrites
    }

    fn enumerate(&self, state: State) {
        if !self.memo.borrow().contains_key(&state) {
            // We mark this state as in progress so that we don't accidentally
            // loop forever.
            self.memo.borrow_mut().insert(state, Vec::new());

            let mut anti_unifications = Vec::new();

            // For each node in the eclass, we can create an anti-unification.
            if let Some(rules) = self.dfta.get_by_output(&state) {
                for (kind, inputs) in rules {
                    // Pair of argument positions, anti-unif program
                    let mut prev: Vec<(Vec<Id>, Antiunification<K>)> =
                        vec![(Vec::new(), Antiunification::new())];

                    // TODO: pruning optimization

                    // For each of the children, we need to enumerate each of them. We can
                    // then include their anti-unifications in our anti-unification.
                    for input in inputs {
                        self.enumerate(*input);

                        let mut cur = Vec::new();
                        for child_anti_unifications in self.memo.borrow().get(input).unwrap() {
                            for (mut arg_positions, mut anti_unification) in prev.clone() {
                                anti_unification.extend(child_anti_unifications);
                                arg_positions.push(Id::from(anti_unification.pattern.len() - 1));
                                cur.push((arg_positions, anti_unification));
                            }
                        }
                        prev = cur;
                    }

                    anti_unifications.extend(prev.into_iter().map(
                        |(arg_positions, mut anti_unification)| {
                            let enode = Expr::from_parts(kind.clone(), arg_positions);
                            anti_unification.pattern.push(ENodeOrVar::ENode(enode));
                            anti_unification
                        },
                    ));
                }
            } else {
                // This is a phi node, just introduce an anti-unification
                // with a phi node.
                anti_unifications.push(Antiunification::phi(state));
            };

            // Finally, memoize our result
            self.memo.borrow_mut().insert(state, anti_unifications);
        }
    }
}

// FIXME: These rewrites introduce lambdas that they can apply to.
/// Anti-unifies within a given `EGraph`, returning a vec of rewrite rules as output.
pub fn anti_unify<K, A>(egraph: &EGraph<Expr<K>, A>) -> Vec<Rewrite<Expr<K>, A>>
where
    K: Antiunifiable,
    A: Analysis<Expr<K>>,
{
    Antiunifier::new(egraph).anti_unify()
}
