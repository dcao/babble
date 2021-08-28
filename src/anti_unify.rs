//! Anti-unification of e-nodes within an e-graph.

use crate::dfta::Dfta;
use egg::{Analysis, EGraph, ENodeOrVar, Id, Language, Pattern, RecExpr, Rewrite, Var};
use std::{cell::RefCell, collections::HashMap, fmt::Display, hash::Hash};
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

impl<L, A> From<&EGraph<L, A>> for Dfta<L::Kind, Id>
where
    L: Antiunifiable,
    A: Analysis<L>,
{
    fn from(egraph: &EGraph<L, A>) -> Self {
        let mut dfta = Dfta::new();
        for eclass in egraph.classes() {
            for enode in eclass.iter() {
                dfta.add_rule(
                    enode.kind(),
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
pub trait Antiunifiable: Language + Sync + Send + Display + 'static {
    /// A type representing what kind of expression a language node is.
    type Kind: Ord + Hash + Clone;

    /// The kind of this language node. In particular, for any two language
    /// nodes `a: Self` and `b: Self`, if `a.kind() == b.kind()`, then
    /// `a.matches(&b)` must be true.
    fn kind(&self) -> Self::Kind;

    /// Create a language node with kind `kind` and children `children`.
    fn from_parts<I>(kind: Self::Kind, children: I) -> Self
    where
        I: IntoIterator<Item = Id>;

    /// Return a language node representing a lambda abstraction over some
    /// body.
    fn lambda(body: Id) -> Self;

    /// Return a language node representing an application of a function
    /// to an argument.
    fn app(lambda: Id, arg: Id) -> Self;

    /// Return a node representing a de Brujin index for a lambda.
    fn lambda_arg(ix: usize) -> Self;

    /// Return a node representing a reference to a named fn.
    fn fn_sym() -> Self;

    /// Return a node representing a new learned library fn.
    fn lib(name: Id, lam: Id, body: Id) -> Self;
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
pub struct Antiunification<L> {
    // We use a Vec here, but this is equivalent to a RecExpr.
    /// The search pattern of this anti-unification
    pub pattern: Vec<ENodeOrVar<L>>,

    /// A sorted list of phi Ids, which will eventually become arguments to
    /// lambdas. At the end, we generate de Brujin indices by getting the
    /// index of each Id in this list.
    pub args: Vec<Var>,

    phi: bool,
}

impl<L> Default for Antiunification<L> {
    fn default() -> Self {
        Self {
            pattern: Vec::new(),
            args: Vec::new(),
            phi: false,
        }
    }
}

impl<L: Antiunifiable> Antiunification<L> {
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
    pub fn lambdify(&self) -> Vec<ENodeOrVar<L>> {
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
                ENodeOrVar::Var(var) => ENodeOrVar::ENode(L::lambda_arg(arg_map[var])),
            });
        }

        // We then introduce as many lambdas as is needed to cover our
        // args.
        for _ in 0..self.args.len() {
            lambda.push(ENodeOrVar::ENode(L::lambda((lambda.len() - 1).into())));
        }
        let fn_id = lambda.len() - 1;

        // Push a symbol representing our hashed function to the lambda.
        let sym_id = lambda.len();
        lambda.push(ENodeOrVar::ENode(L::fn_sym()));

        // Then we introduce applications
        // Make sure we iterate in the right order for our de brujin indices :))
        for arg in self.args.iter().copied().rev() {
            lambda.push(ENodeOrVar::Var(arg));
            lambda.push(ENodeOrVar::ENode(L::app(
                (lambda.len() - 2).into(),
                (lambda.len() - 1).into(),
            )));
        }

        // Finally, introduce a let.
        lambda.push(ENodeOrVar::ENode(L::lib(
            sym_id.into(),
            fn_id.into(),
            (lambda.len() - 1).into(),
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
pub struct Antiunifier<L: Antiunifiable> {
    dfta: Dfta<L::Kind, State>,

    // Memoization for enumeration of anti-unified programs
    memo: RefCell<HashMap<State, Vec<Antiunification<L>>>>,
}

impl<L: Antiunifiable> Antiunifier<L> {
    /// Initialize an `AntiUnifier` from an `EGraph`.
    /// We first rebuild this egraph to make sure all its invariants hold.
    /// We then create a DFTA from it which we will use for anti-unification
    /// work.
    pub fn new<A: Analysis<L>>(egraph: &EGraph<L, A>) -> Self {
        Self {
            dfta: Dfta::from(egraph).self_intersection(),
            memo: RefCell::new(HashMap::new()),
        }
    }

    /// Perform anti-unification.
    pub fn anti_unify<A: Analysis<L>>(&mut self) -> Vec<Rewrite<L, A>> {
        // TODO: parallelize this?
        // We then enumerate our transitions as well, additionally converting these
        // anti-unifications into rewrites which we will apply to the egraph.
        let mut rewrites: Vec<Rewrite<L, A>> = Vec::new();
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
                let searcher: Pattern<L> = RecExpr::from(anti_unification.pattern.clone()).into();
                let applier: Pattern<L> = RecExpr::from(anti_unification.lambdify()).into();
                let name = format!("anti-unify {:?}", state);
                let rewrite =
                    Rewrite::new(name, searcher, applier).unwrap_or_else(|_| unreachable!());
                rewrites.push(rewrite);
            }
        }
        rewrites
    }

    fn enumerate(&self, state: State) {
        // TODO: this could infinite loop
        if !self.memo.borrow().contains_key(&state) {
            let mut anti_unifications = Vec::new();

            // For each node in the eclass, we can create an anti-unification.
            if let Some(rules) = self.dfta.get_by_output(&state) {
                for (kind, inputs) in rules {
                    // Pair of argument positions, anti-unif program
                    let mut prev: Vec<(Vec<usize>, Antiunification<L>)> =
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

                                arg_positions.push(anti_unification.pattern.len() - 1);

                                cur.push((arg_positions, anti_unification));
                            }
                        }
                        prev = cur;
                    }

                    anti_unifications.extend(prev.into_iter().map(
                        |(arg_positions, mut anti_unification)| {
                            let children_map: HashMap<State, Id> = inputs
                                .iter()
                                .copied()
                                .zip(arg_positions.into_iter().map(Id::from))
                                .collect();
                            let children = inputs.iter().map(|state| children_map[state]);
                            let enode = L::from_parts(kind.clone(), children);
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

/// Anti-unifies within a given `EGraph`, returning a vec of rewrite rules as output.
pub fn anti_unify<L, A>(egraph: &EGraph<L, A>) -> Vec<Rewrite<L, A>>
where
    L: Antiunifiable,
    A: Analysis<L>,
{
    Antiunifier::new(egraph).anti_unify()
}
