//! Implements anti-unification between two egraphs.

use dashmap::DashMap;
use egg::{Analysis, EGraph, ENodeOrVar, Id, Language, Pattern, RecExpr, Rewrite, Var};
use std::{
    collections::HashMap,
    mem::{self, Discriminant},
};

// Central idea of anti-unification technique:
// 1. Compile all programs into one central egraph
// 2. Turn egraph into DFTA, rebuilding it first to get our egraph invariants back
// 3. Select some sets of eclasses to try to anti-unify
//    Do we only select pairs from different exprs? Or can we have pairs within the same expr?
//    (i.e. where one eclass is the parent of another)
//    This informs what pairs will initially populate the queue
// 4. TODO: After this first round of anti-unification, we start to anti-unify the
//    anti-unifications, thus producing anti-unifications between 3 exprs.
//    We try anti-unifying these new anti-unified pairs (somehow we have to
//    look for pairs we can anti-unify, but only out of the newly inserted
//    ones?) Is this something we even want to do?

// Idea: turn anti-unified DFTAs back into egraph using rewrite rules!
// 1. Compile all programs into one central egraph
// 2. Pick anti-unification targets and anti-unify everything
// 3. e.g.
//    output DFTA: (+ q1 q2) -> q3
//                 1 -> q1
//                 phi((+ 1 2), 2) -> q3
// 4. Turn this into a rewrite rule to rewrite (+ 1 2) to (app f 2)?
//    or specifically apply this to the eclasses in question?

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

/// An `IdInterner` stores a two-way mapping between pairs of states and fresh
/// states used in their place. In order to avoid DFTAs having state arguments
/// and outputs of variable length, we instead generate fresh states using
/// the `IdInterner` from the two states that are being combined.
///
/// Note that these Ids have no relation to any egraph whatsoever!
#[derive(Debug)]
pub struct IdInterner {
    states: HashMap<Id, Vec<Id>>,
    sets: HashMap<Vec<Id>, Id>,
    /// The start Id
    pub start: Id,
    counter: Id,
}

impl IdInterner {
    /// Creates a new `IdInterner`.
    #[must_use]
    pub fn init(start: Id) -> Self {
        let mut states = HashMap::new();
        let mut sets = HashMap::new();
        for i in 0..start.into() {
            states.insert(i.into(), vec![i.into()]);
            sets.insert(vec![i.into()], i.into());
        }

        Self {
            states,
            sets,
            start,
            counter: start,
        }
    }

    /// Gets the set corresponding to two ids.
    ///
    /// # Panics
    /// Panics if a and b aren't in Ids.
    #[must_use]
    pub fn lookup(&self, a: Id, b: Id) -> Vec<Id> {
        let owned_states_b: Vec<Id>;
        let states_b = if b < self.start {
            owned_states_b = vec![b];
            &owned_states_b
        } else {
            self.states.get(&b).unwrap()
        };
        let mut res = if a < self.start {
            vec![a]
        } else {
            self.states.get(&a).unwrap().clone()
        };
        for bid in states_b {
            match res.binary_search(bid) {
                Ok(_) => {}
                Err(ix) => {
                    res.insert(ix, *bid);
                }
            }
        }

        res
    }

    /// Gets the Id corresponding to the pair of two Ids given, if it exists.
    #[must_use]
    pub fn get_id(&self, a: Id, b: Id) -> Option<Id> {
        self.sets.get(&self.lookup(a, b)).copied()
    }

    /// Generates the Id corresponding to the pair of two Ids given.
    pub fn gen_id(&mut self, a: Id, b: Id) -> Id {
        let res = self.counter;
        let new_id = self.lookup(a, b);
        self.states.insert(self.counter, new_id.clone());
        self.sets.insert(new_id, self.counter);
        self.counter = (usize::from(self.counter) + 1).into();
        res
    }

    /// Gets the Id corresponding to the pair of two Ids given if it exists;
    /// otherwise, generates this Id.
    pub fn get_or_gen(&mut self, a: Id, b: Id) -> Id {
        self.get_id(a, b)
            .map_or_else(|| self.gen_id(a, b), |res| res)
    }

    /// Gets all eclasses stored in this `IdInterner`.
    pub fn eclasses(&self) -> impl Iterator<Item = Id> + '_ {
        self.states
            .keys()
            .filter(move |x| *x < &self.start)
            .copied()
    }

    /// Gets all states (not in the egraph) stored in this `IdInterner`.
    pub fn states(&self) -> impl Iterator<Item = Id> + '_ {
        self.states
            .keys()
            .filter(move |x| *x >= &self.start)
            .copied()
    }
}

/// A transition rule in a DFTA.
#[derive(Debug, Clone)]
pub struct Rule<L> {
    input: L,
    output: Id,
}

impl<L> Rule<L> {
    /// Create a new transition from `input` to `output`.
    pub fn new(input: L, output: Id) -> Self {
        Rule { input, output }
    }
}

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
pub struct Dfta<L> {
    outputs_to_inputs: HashMap<Id, Vec<L>>,
}

impl<L, A> From<&EGraph<L, A>> for Dfta<L>
where
    L: Language,
    A: Analysis<L>,
{
    fn from(egraph: &EGraph<L, A>) -> Self {
        let mut dfta = Self::new();
        for eclass in egraph.classes() {
            for enode in eclass.iter() {
                dfta.push(Rule::new(enode.clone(), eclass.id));
            }
        }
        dfta
    }
}

impl<L> Dfta<L> {
    /// Create an empty DFTA.
    #[must_use]
    pub fn new() -> Self {
        Self {
            outputs_to_inputs: HashMap::new(),
        }
    }
    /// Adds a new transition to the DFTA.
    pub fn push(&mut self, rule: Rule<L>) {
        self.outputs_to_inputs
            .entry(rule.output)
            .or_default()
            .push(rule.input);
    }

    /// Marks the given state as visited in the DFTA.
    pub fn push_empty(&mut self, s: Id) {
        self.outputs_to_inputs.entry(s).or_default();
    }

    /// Checks if a state has been visited.
    #[must_use]
    pub fn has_visited(&self, s: Id) -> bool {
        self.outputs_to_inputs.contains_key(&s)
    }

    /// Get all the transitions which have this state as an output.
    #[must_use]
    pub fn get_by_output(&self, output: Id) -> Option<&[L]> {
        self.outputs_to_inputs.get(&output).map(Vec::as_slice)
    }
}

impl<L> Default for Dfta<L> {
    fn default() -> Self {
        Self::new()
    }
}

/// An `AntiUnifTgt` is an extension of a Language which has constructs to
/// introduce lambdas, etc.
pub trait AntiUnifTgt: Language + Sync + Send + std::fmt::Display + 'static
{
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

/// Converts an Id to a pattern variable.
fn id_to_pvar(c: Id) -> Var {
    format!("?{}", c).parse().unwrap()
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
pub struct AntiUnification<L> {
    // We use a Vec here, but this is equivalent to a RecExpr.
    /// The search pattern of this anti-unification
    pub pattern: Vec<ENodeOrVar<L>>,

    /// A sorted list of phi Ids, which will eventually become arguments to
    /// lambdas. At the end, we generate de Brujin indices by getting the
    /// index of each Id in this list.
    pub args: Vec<Id>,

    phi: bool,
}

impl<L> Default for AntiUnification<L> {
    fn default() -> Self {
        Self {
            pattern: Vec::new(),
            args: Vec::new(),
            phi: false,
        }
    }
}

impl<L: AntiUnifTgt> AntiUnification<L> {
    /// Creates a new empty anti-unification
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a phi anti-unification
    #[must_use]
    pub fn phi(c: Id) -> Self {
        Self {
            pattern: vec![ENodeOrVar::Var(id_to_pvar(c))],
            args: vec![c],
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
    pub fn lambdify(&self) -> Vec<ENodeOrVar<L>>
    {
        // We first create a map from the stringified Id to its de Brujin index.
        let mut lambda = vec![];
        let arg_map: HashMap<Var, _> = self
            .args
            .iter()
            .enumerate()
            .map(|(i, x)| (id_to_pvar(*x), i))
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
        for arg_id in self.args.iter().rev() {
            lambda.push(ENodeOrVar::Var(id_to_pvar(*arg_id)));
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
pub struct AntiUnifier<'a, L, A>
where
    L: AntiUnifTgt,
    A: Analysis<L>,
{

    egraph: &'a EGraph<L, A>,

    dfta: Dfta<L>,
    interner: IdInterner,

    // Memoization for enumeration of anti-unified programs
    memo: DashMap<Id, Vec<AntiUnification<L>>>,
}

impl<'a, L, A> AntiUnifier<'a, L, A>
where
    L: AntiUnifTgt,
    A: Analysis<L>,
{
    /// Initialize an `AntiUnifier` from an `EGraph`.
    /// We first rebuild this egraph to make sure all its invariants hold.
    /// We then create a DFTA from it which we will use for anti-unification
    /// work.
    pub fn new(egraph: &'a EGraph<L, A>) -> Self {
        let dfta = Dfta::from(egraph);
        let max_id = egraph
            .classes()
            .map(|x| usize::from(x.id))
            .max()
            .map_or(0, |x| x + 1)
            .into();

        Self {
            egraph,
            dfta,
            interner: IdInterner::init(max_id),
            memo: DashMap::new(),
        }
    }

    // TODO: init_worklist at depths larger than 2
    fn init_worklist(&self) -> Vec<(Rule<L>, Rule<L>)> {
        type Variant<L> = (Discriminant<L>, usize);

        let mut rules_by_variant: HashMap<Variant<L>, Vec<Rule<L>>> = HashMap::new();
        for eclass in self.egraph.classes() {
            for enode in eclass.iter() {
                // TODO: Replace this with something less hacky
                let variant = (mem::discriminant(enode), enode.len());
                rules_by_variant
                    .entry(variant)
                    .or_default()
                    .push(Rule::new(enode.clone(), eclass.id));
            }
        }

        let mut worklist = Vec::new();
        for rules in rules_by_variant.values() {
            for rule1 in rules {
                for rule2 in rules {
                    if rule1.output < rule2.output {
                        worklist.push((rule1.clone(), rule2.clone()));
                    }
                }
            }
        }
        worklist
    }

    /// Perform anti-unification.
    ///
    /// # Panics
    /// Technically can panic, but shouldn't given fn invariants.
    pub fn anti_unify(&mut self) -> Vec<Rewrite<L, A>> {
        // We first build our worklist from the graph.
        let worklist = self.init_worklist();

        // We then build our transitions from this worklist
        for (rule1, rule2) in worklist {
            let new_rule = self.zip_rules(rule1, &rule2);
            self.dfta.push(new_rule);
        }

        // We start by populating our memoization table by enumerating
        // the eclasses existing in the egraph. We do this separately partly
        // since the mechanics for populating from the egraph are different,
        // and partly since if we want to parallelize transition enumeration
        // later, it'll be difficult to do so if we have to access the egraph
        // across multiple threads. These memoized "anti-unifications" will
        // have no metavariables in the pattern, and no arguments in the
        // arg map.
        for eclass in self.interner.eclasses() {
            self.enumerate(eclass);
        }

        // TODO: parallelize this?
        // We then enumerate our transitions as well, additionally converting these
        // anti-unifications into rewrites which we will apply to the egraph.
        let mut rewrites: Vec<Rewrite<L, A>> = Vec::new();
        for new_eclass in self.interner.states() {
            self.enumerate(new_eclass);

            for anti_unification in self.memo.get(&new_eclass).unwrap().value() {
                if anti_unification.is_invalid() {
                    continue;
                }
                let searcher: Pattern<L> = RecExpr::from(anti_unification.pattern.clone()).into();
                let applier: Pattern<L> = RecExpr::from(anti_unification.lambdify()).into();
                let name = new_eclass.to_string();
                rewrites.push(Rewrite::new(name, searcher, applier).unwrap());
            }
        }

        rewrites
    }

    // fn add(&mut self, eclass: NewOrExisting) -> IdOrVar {
    //     let eclass = match eclass {
    //         New(id) => id,
    //         Existing(id) => return id,
    //     };
    //     let enodes = self.dfta.get_by_output(eclass);
    //     for enode in enodes {
    //         let children = enode.children().map(|child| self.add(child))
    //     }
    // }

    fn enumerate(&self, eclass: Id) {
        if !self.memo.contains_key(&eclass) {
            let mut anti_unifications = Vec::new();

            // For each node in the eclass, we can create an anti-unification.
            if let Some(enodes) = self.dfta.get_by_output(eclass) {
                for enode in enodes {
                    // Pair of argument positions, anti-unif program
                    let mut prev: Vec<(Vec<usize>, AntiUnification<L>)> =
                        vec![(Vec::new(), AntiUnification::new())];

                    // TODO: pruning optimization

                    // For each of the children, we need to enumerate each of them. We can
                    // then include their anti-unifications in our anti-unification.
                    for child in enode.children() {
                        self.enumerate(*child);

                        let mut cur = Vec::new();
                        for child_anti_unifications in self.memo.get(child).unwrap().value() {
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
                            let children_map: HashMap<Id, Id> = enode
                                .children()
                                .iter()
                                .copied()
                                .zip(arg_positions.into_iter().map(Id::from))
                                .collect();
                            let mut new_l = enode.clone();
                            for child in new_l.children_mut() {
                                *child = children_map[&*child];
                            }
                            anti_unification.pattern.push(ENodeOrVar::ENode(new_l));
                            anti_unification
                        },
                    ));
                }
            } else {
                // This is a phi node, just introduce an anti-unification
                // with a phi node.
                anti_unifications.push(AntiUnification::phi(eclass));
            };

            // Finally, memoize our result
            self.memo.insert(eclass, anti_unifications);
        }
    }

    /// Anti-unifies two transition rules, potentially producing a new rule.
    /// If the operations in the two cases differ, we cannot anti-unify further,
    /// and will return None. Otherwise, if the operations are the same, we produce
    /// a new rule that anti-unifies within the arguments.
    ///
    /// This function assumes the two transitions match.
    fn zip_rules(&mut self, rule1: Rule<L>, rule2: &Rule<L>) -> Rule<L> {
        let output = self.interner.get_or_gen(rule1.output, rule2.output);
        let children: HashMap<Id, (Id, Id)> = rule1
            .input
            .children()
            .iter()
            .copied()
            .zip(rule2.input.children().iter().copied())
            .map(|(child1, child2)| (child1, (child1, child2)))
            .collect();

        let mut input = rule1.input;
        for child in input.children_mut() {
            let (child1, child2) = children[&*child];
            *child = self.interner.get_or_gen(child1, child2);
        }

        Rule::new(input, output)
    }
}

/// Anti-unifies within a given `EGraph`, returning a vec of rewrite rules as output.
pub fn anti_unify<L, A>(egraph: &EGraph<L, A>) -> Vec<Rewrite<L, A>>
where
    L: AntiUnifTgt,
    A: Analysis<L>,
{
    AntiUnifier::new(egraph).anti_unify()
}
