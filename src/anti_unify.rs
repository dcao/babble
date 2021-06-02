//! Implements anti-unification between two egraphs.

use ahash::AHasher;
use dashmap::DashMap;
use egg::{
    Analysis, EGraph, ENodeOrVar, Id, Language, Pattern, RecExpr, Rewrite, Runner, Symbol, Var,
};
use hashbrown::{HashMap, HashSet};
use smallvec::{smallvec, SmallVec};
use std::hash::{Hash, Hasher};

use super::extract::Extractor;

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

// TODO: Because we don't have .children()
fn enode_children<L: Language>(enode: &L) -> Vec<Id> {
    let mut children = Vec::with_capacity(enode.len());
    enode.for_each(|child| {
        children.push(child);
    });
    children
}

/// An `IdInterner` stores a two-way mapping between pairs of states and fresh
/// states used in their place. In order to avoid DFTAs having state arguments
/// and outputs of variable length, we instead generate fresh states using
/// the `IdInterner` from the two states that are being combined.
///
/// Note that these Ids have no relation to any egraph whatsoever!
#[derive(Debug)]
pub struct IdInterner {
    states: HashMap<Id, SmallVec<[Id; 4]>>,
    sets: HashMap<SmallVec<[Id; 4]>, Id>,
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
            states.insert(i.into(), smallvec![i.into()]);
            sets.insert(smallvec![i.into()], i.into());
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
    pub fn lookup(&self, a: Id, b: Id) -> SmallVec<[Id; 4]> {
        let owned_states_b: SmallVec<[Id; 4]>;
        let states_b = if b < self.start {
            owned_states_b = smallvec![b];
            &owned_states_b
        } else {
            self.states.get(&b).unwrap()
        };
        let mut res = if a < self.start {
            smallvec![a]
        } else {
            self.states.get(&a).unwrap().clone()
        };
        for bid in states_b {
            match res.binary_search(&bid) {
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
        self.counter = (Into::<usize>::into(self.counter) + 1).into();
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
    states: HashMap<Id, SmallVec<[L; 8]>>,
}

impl<L: Language> Dfta<L> {
    /// Builds a new `Dfta` from an `EGraph`.
    pub fn build<N: Analysis<L>>(g: &EGraph<L, N>) -> Dfta<L> {
        let mut states = HashMap::new();
        for class in g.classes() {
            for node in class.iter() {
                states
                    .entry(class.id)
                    .or_insert_with(SmallVec::new)
                    // We first insert our lambda arguments into the RecExpr,
                    .push(node.clone());
            }
        }
        Self { states }
    }

    /// Adds a new transition to the DFTA.
    pub fn push(&mut self, (l, s): (L, Id)) {
        self.states.entry(s).or_insert_with(SmallVec::new).push(l);
    }

    /// Marks the given state as visited in the DFTA.
    pub fn push_empty(&mut self, s: Id) {
        self.states.entry(s).or_insert_with(SmallVec::new);
    }

    /// Checks if a state has been visited.
    #[must_use]
    pub fn has_visited(&self, s: Id) -> bool {
        self.states.contains_key(&s)
    }

    /// Get all the transitions which have this state as an output.
    #[must_use]
    pub fn get_by_state(&self, s: Id) -> Option<&SmallVec<[L; 8]>> {
        self.states.get(&s)
    }
}

pub trait AUAnalysis<L: Language>: Analysis<L, Data = HashSet<Symbol>> + Clone + Default {}

/// An `AntiUnifTgt` is an extension of a Language which has constructs to
/// introduce lambdas, etc.
pub trait AntiUnifTgt: Language + Sync + Send + std::fmt::Display + 'static {
    type Analysis: AUAnalysis<Self>;

    /// Return a language node representing a lambda abstraction over some
    /// body.
    fn lambda(body: Id) -> Self;

    /// Returns if a language node is a lambda or not.
    fn is_lambda(node: &Self) -> bool;

    /// Return a language node representing an application of a function
    /// to an argument.
    fn app(lambda: Id, arg: Id) -> Self;

    /// Return a node representing a de Brujin index for a lambda.
    fn lambda_arg(ix: usize) -> Self;

    /// Return a node representing a reference to a named fn.
    fn fn_sym(hash: u64) -> Self;

    /// Return a node representing a new learned library fn.
    fn lib(name: Id, lam: Id, body: Id) -> Self;

    /// Returns a list of rewrites which lifts lets to the top level
    /// of the expression in some order.
    fn lift_lets() -> Vec<Rewrite<Self, Self::Analysis>>;
}

/// Converts an Id to a pattern variable.
fn id_to_pvar(c: Id) -> Var {
    format!("?{}", c.to_string()).parse().unwrap()
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
    pub args: SmallVec<[Id; 32]>,

    phi: bool,
}

impl<L> Default for AntiUnification<L> {
    fn default() -> Self {
        Self {
            pattern: Vec::default(),
            args: SmallVec::new(),
            phi: false,
        }
    }
}

impl<L: AntiUnifTgt> AntiUnification<L> {
    /// Creates a new empty anti-unification
    #[must_use]
    pub fn new() -> AntiUnification<L> {
        Self::default()
    }

    /// Creates a phi anti-unification
    #[must_use]
    pub fn phi(c: Id) -> AntiUnification<L> {
        Self {
            pattern: vec![ENodeOrVar::Var(id_to_pvar(c))],
            args: smallvec![c],
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
    pub fn extend(&mut self, other: &AntiUnification<L>) {
        // First, extend the pattern AST.
        // The delta we need to add to the items in the other
        // pattern AST is equal to the current length of the pattern.
        let delta = self.pattern.len();
        let new_other = other
            .pattern
            .clone()
            .into_iter()
            .map(|x| x.map_children(|c| (Into::<usize>::into(c) + delta).into()));
        self.pattern.extend(new_other);

        // Then, extend the args list.
        for arg in &other.args {
            if !self.args.contains(arg) {
                self.args.push(*arg);
            }
        }
    }

    /// Turns this anti-unification into a lambda, applied to each of the args.
    /// Also returns a hash of the lambda itself, so we don't insert duplicate lambdas.
    #[must_use]
    pub fn lambdify(&self) -> (u64, Vec<ENodeOrVar<L>>) {
        // We first create a map from the stringified Id to its de Brujin index.
        let mut res = vec![];
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
            res.push(match node {
                ENodeOrVar::ENode(n) => ENodeOrVar::ENode(n.clone()),
                ENodeOrVar::Var(var) => ENodeOrVar::ENode(L::lambda_arg(arg_map[var])),
            });
        }

        // We then introduce as many lambdas as is needed to cover our
        // args.
        for _ in 0..self.args.len() {
            res.push(ENodeOrVar::ENode(L::lambda((res.len() - 1).into())));
        }
        let fn_id = res.len() - 1;

        // Hash our function!
        let mut h = AHasher::default();
        res.hash(&mut h);
        let hash = h.finish();

        // Push a symbol representing our hashed function to the lambda.
        let sym_id = res.len();
        res.push(ENodeOrVar::ENode(L::fn_sym(hash)));

        // Then we introduce applications
        // Make sure we iterate in the right order for our de brujin indices :))
        for arg_id in self.args.iter().rev() {
            res.push(ENodeOrVar::Var(id_to_pvar(*arg_id)));
            res.push(ENodeOrVar::ENode(L::app(
                (res.len() - 2).into(),
                (res.len() - 1).into(),
            )));
        }

        // Finally, introduce a let.
        res.push(ENodeOrVar::ENode(L::lib(
            sym_id.into(),
            fn_id.into(),
            (res.len() - 1).into(),
        )));

        (hash, res)
    }
}

/// An `AntiUnifier` stores the state of an anti-unification invocation.
/// This struct should take in a single egraph with every program (both
/// library functions and synthesis solutions) compiled together.
/// After running anti-unification, we have a new egraph which contains
/// all possible libraries we could learn from the
#[derive(Debug)]
pub struct AntiUnifier<L: AntiUnifTgt> {
    graph: EGraph<L, L::Analysis>,

    dfta: Dfta<L>,
    interner: IdInterner,

    // Memoization for enumeration of anti-unified programs
    memo: DashMap<Id, Vec<AntiUnification<L>>>,
}

impl<L: AntiUnifTgt> AntiUnifier<L> {
    /// Initialize an `AntiUnifier` from an `EGraph`.
    /// We first rebuild this egraph to make sure all its invariants hold.
    /// We then create a DFTA from it which we will use for anti-unification
    /// work.
    pub fn new(mut g: EGraph<L, L::Analysis>) -> Self {
        g.rebuild();
        let dfta = Dfta::build(&g);

        let max_id = g
            .classes()
            .map(|x| x.id)
            .max()
            .map_or_else(|| 0.into(), |x| (Into::<usize>::into(x) + 1).into());

        Self {
            graph: g,
            dfta,
            interner: IdInterner::init(max_id),
            memo: DashMap::new(),
        }
    }

    // TODO: init_worklist at depths larger than 2
    fn init_worklist(g: &EGraph<L, L::Analysis>) -> Vec<((L, Id), (L, Id))> {
        fn enode_hash<L: Language + std::fmt::Display>(enode: &L) -> String {
            format!("{}_{}", enode, enode.len())
        }

        let mut map: HashMap<_, Vec<(L, Id)>> = HashMap::new();
        for class in g.classes() {
            for node in class.iter() {
                let hash = enode_hash(node);
                let vals = map.entry(hash).or_insert_with(Vec::new);
                vals.push((node.clone(), class.id));
            }
        }

        let mut res = vec![];
        for (_d, ids) in map {
            for a in &ids {
                for b in &ids {
                    if a.1 < b.1 {
                        res.push((a.clone(), b.clone()));
                    }
                }
            }
        }
        res
    }

    /// Perform anti-unification.
    ///
    /// # Panics
    /// Technically can panic, but shouldn't given fn invariants.
    pub fn anti_unify(&mut self, root: Id) {
        // We first build our worklist from the graph.
        let worklist = Self::init_worklist(&self.graph);

        // We then build our transitions from this worklist
        for (a, b) in worklist {
            let t = self.anti_unif_transitions(a, b);
            self.dfta.push(t);
        }

        // We start by populating our memoization table by enumerating
        // the eclasses existing in the egraph. We do this separately partly
        // since the mechanics for populating from the egraph are different,
        // and partly since if we want to parallelize transition enumeration
        // later, it'll be difficult to do so if we have to access the egraph
        // across multiple threads. These memoized "anti-unifications" will
        // have no metavariables in the pattern, and no arguments in the
        // arg map.
        for c in self.interner.eclasses().collect::<Vec<_>>() {
            self.enumerate(c, |c| Some(&self.graph[c].nodes));
        }

        // TODO: parallelize this?
        // We then enumerate our transitions as well, additionally converting these
        // anti-unifications into rewrites which we will apply to the egraph.
        let mut rewrites: HashMap<u64, Rewrite<L, L::Analysis>> = HashMap::new();
        for c in self.interner.states().collect::<Vec<_>>() {
            self.enumerate(c, |c| {
                self.dfta.get_by_state(c).map(std::convert::AsRef::as_ref)
            });
            for prog in self.memo.get(&c).unwrap().value() {
                if prog.is_invalid() {
                    continue;
                }
                let searcher_rec: RecExpr<ENodeOrVar<L>> = prog.pattern.clone().into();
                let (hash, applier_rec): (u64, RecExpr<ENodeOrVar<L>>) = {
                    let l = prog.lambdify();
                    (l.0, l.1.into())
                };

                println!(
                    "rewrite:\n{}\n=>\n{}\n",
                    searcher_rec.pretty(80),
                    applier_rec.pretty(80)
                );

                let name = applier_rec.to_string();
                let searcher: Pattern<L> = searcher_rec.into();
                let applier: Pattern<L> = applier_rec.into();

                let _res =
                    rewrites.try_insert(hash, Rewrite::new(name, searcher, applier).unwrap());
            }
        }

        println!("final: {:#?}", rewrites);

        // Finally, run the rewrites!
        let runner = Runner::default()
            .with_scheduler(egg::SimpleScheduler)
            // .with_iter_limit(1_000)
            .with_node_limit(1_000_000)
            // .with_time_limit(core::time::Duration::from_secs(40))
            .with_egraph(self.graph.clone())
            // .run(rewrites.values().chain(L::lift_lets().iter()));
            .run(L::lift_lets().iter());
        // .run(rewrites.values());

        // println!("{:?}", runner.stop_reason);

        // TODO: find the root properly lol
        let egraph = runner.egraph;

        egraph.dot().to_svg("target/smh2.svg").unwrap();

        // Then, extract the best program from the egraph, starting at
        // the root
        let mut extractor = Extractor::new(&egraph, egg::AstSize, egraph.find(root));
        extractor.find_best(egraph.find(root));
        println!(
            "{}, {}",
            extractor.find_best(root).0,
            extractor.find_best(root).1.pretty(100)
        );
    }

    fn enumerate<'a, F>(&'a self, c: Id, get_nodes: F)
    where
        F: Fn(Id) -> Option<&'a [L]> + Copy,
    {
        if !self.memo.contains_key(&c) {
            // For each node in the eclass, we can create an anti-unification.
            let res = if let Some(nodes) = get_nodes(c) {
                let mut res = Vec::new();
                for l in nodes {
                    // Pair of argument positions, anti-unif program
                    let mut prev: Vec<(SmallVec<[usize; 16]>, AntiUnification<L>)> =
                        vec![(smallvec![], AntiUnification::new())];
                    let mut cur = Vec::new();
                    let children = enode_children(l);

                    // TODO: pruning optimization

                    // For each of the children, we need to enumerate each of them. We can
                    // then include their anti-unifications in our anti-unification.
                    for c in children.clone() {
                        self.enumerate(c, get_nodes);
                        for child_au in self.memo.get(&c).unwrap().value() {
                            for (mut pre_children, pre) in prev.clone() {
                                let mut pre: AntiUnification<L> = pre;
                                pre.extend(child_au);
                                pre_children.push(pre.pattern.len() - 1);
                                cur.push((pre_children, pre));
                            }
                        }
                        prev = cur;
                        cur = Vec::new();
                    }

                    res.extend(prev.into_iter().map(|(au_children, mut anti_unif)| {
                        let children_map = children
                            .iter()
                            .zip(au_children.iter())
                            .collect::<HashMap<_, _>>();
                        let new_l = l.clone().map_children(|c| (*children_map[&c]).into());
                        anti_unif.pattern.push(ENodeOrVar::ENode(new_l));
                        anti_unif
                    }));
                }
                res
            } else {
                // This is a phi node, just introduce an anti-unification
                // with a phi node.
                vec![AntiUnification::phi(c)]
            };

            // Finally, memoize our result
            self.memo.insert(c, res);
        }
    }

    /// Anti-unifies two transition rules, potentially producing a new rule.
    /// If the operations in the two cases differ, we cannot anti-unify further,
    /// and will return None. Otherwise, if the operations are the same, we produce
    /// a new rule that anti-unifies within the arguments.
    ///
    /// This function assumes the two transitions match.
    fn anti_unif_transitions(&mut self, (la, sa): (L, Id), (lb, sb): (L, Id)) -> (L, Id) {
        let out_state = self.interner.get_or_gen(sa, sb);
        // TODO: this hacky business is b/c we don't have .children(_mut) :/
        let children: HashMap<Id, (Id, Id)> = enode_children(&la)
            .into_iter()
            .zip(enode_children(&lb))
            .map(|(a, b)| (a, (a, b)))
            .collect();
        let mut lres = la;
        lres.for_each_mut(|t| {
            let (a, b) = children[t];
            *t = self.interner.get_or_gen(a, b);
        });
        (lres, out_state)
    }
}

/// Anti-unifies within a given `EGraph`, returning an `AntiUnifier` as output.
pub fn anti_unify<L: AntiUnifTgt>(g: EGraph<L, L::Analysis>, root: Id) -> AntiUnifier<L> {
    let mut a = AntiUnifier::new(g);
    a.anti_unify(root);
    a
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::smiley_lang::Rewrite;
    use egg::Runner;

    #[test]
    fn test_anti_unif_1() {
        // Our list of rewrite rules is here
        let rules: &[Rewrite] = &[];

        // First, parse the expression and build an egraph from it
        //        let expr = r"
        //(let f (fn (+ (move 4 4 (scale 2 arg_0)) (+ (move 3 2 arg_0) (+ (move 4 3 (scale 9 circle)) (move 5 2 arg_0)))))
        //(let s1 (app f line)
        //  (let s2 (app f circle)
        //    (+ s1 s2))))".parse().unwrap();
        let expr = r"
(let s1 (+ (move 4 4 (scale 2 line)) (+ (move 3 2 line) (+ (move 4 3 (scale 9 circle)) (move 5 2 line))))
  (let s2 (+ (move 4 4 (scale 2 circle)) (+ (move 3 2 circle) (+ (move 4 3 (scale 9 circle)) (move 5 2 circle))))
    (+ s1 s2)))".parse().unwrap();
        //        let expr = r"
        //(let s1 (app (fn (+ (move 4 4 (scale 2 arg_0)) (+ (move 3 2 arg_0) (+ (move 4 3 (scale 9 circle)) (move 5 2 arg_0))))) line)
        //  (let s2 (app (fn (+ (move 4 4 (scale 2 arg_0)) (+ (move 3 2 arg_0) (+ (move 4 3 (scale 9 circle)) (move 5 2 arg_0))))) circle)
        //    (+ s1 s2)))".parse().unwrap();
        let runner = Runner::default().with_expr(&expr).run(rules);
        let (egraph, root) = (runner.egraph, runner.roots[0]);

        let _res = anti_unify(egraph, root);

        // println!("{:#?}", res.dfta);
    }
}
