//! Implements anti-unification between two egraphs.

use bimap::BiHashMap;
use egg::{Analysis, EGraph, Id, Language};
use std::collections::HashMap;

// Central idea of anti-unification technique:
// 1. Compile all programs into one central egraph
// 2. Turn egraph into DFTA, rebuilding it first to get our egraph invariants back
// 3. Select some pairs of eclasses to try to anti-unify
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

/// An order-independent pair of Ids.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdPair(Id, Id);

impl IdPair {
    /// Creates a new `IdPair`.
    #[must_use]
    pub fn new(a: Id, b: Id) -> Self {
        if a < b {
            Self(a, b)
        } else {
            Self(b, a)
        }
    }
}

/// An `IdInterner` stores a two-way mapping between pairs of states and fresh
/// states used in their place. In order to avoid DFTAs having state arguments
/// and outputs of variable length, we instead generate fresh states using
/// the `IdInterner` from the two states that are being combined.
///
/// Note that these Ids have no relation to any egraph whatsoever!
#[derive(Debug)]
pub struct IdInterner {
    states: BiHashMap<Id, IdPair>,
    // TODO:
    // switch to:
    // state_sets: BiHashMap<Id, HashSet<Id>>,
    // the main problem is depending on how we encode phi args in the Dfta,
    // we may or may not need to record the order that we anti-unify in
    // (i.e. whether we use the right or left arg). If we just use
    // plain rewrite rules, then we don't need to worry about order at
    // all and can just use HashSets. Otherwise, we have to somehow encode
    // both order and depth ((1, 4) anti-unify (2, 3)) versus (((1, 4), 2) anti-unify 3)
    //
    // If we're just coming up with rewrite rules, there has to be a simpler
    // way to encode this in the egraph... HM.
    // e.g. if we come up with an anti-unification (+ x 2), with the pure egraph
    // approach, we would:
    // 1. add the corresponding expression (+ x 2) into the egraph: e = g.add_expr((+ x 2))
    // 2. add a lambda binding for it: e = g.add_expr(let fname e)
    // 3. add a new rewrite into this library fn: (+ ?a 2) -> (app fname ?a)
    //
    // problem: at some point, we would have to turn into transitions rules
    // to be able to do the intersection... or would we?
    // 1. generate initial eclass target pairs in worklist through same method as before - finding
    // matching ops.
    // 2. for each pair in worklist:
    // 2a. loop through nodes. for each pair of nodes that matches:
    // 2a1. zip together children nodes
    // 2a2. recursively anti-unify children here - we need to get back the list of ids from anti-unification!
    // 2a3. add a new node: op(zipped children)...
    // 2b. union together all added nodes for this pair. if no nodes were added, add in a phi value and return the id of that?
    // this process is the same as what we'd have to do with explicitly constructing the Dfta transitions
    // and then getting the egraph from it, but we never explicitly hold the list of transitions within the Dfta
    // or a list of Id mappings - the former is unnecessary, the latter is handled by the egraph.
    // additionally, you might think that adding the phi value over and over might be wasteful or whatever, but
    // we'll only ever add an actual phi value once - we'll just repeatedly use the id of that over and over again,
    // since nodes are shared in the egraph
    //
    // also, we shouldn't bother trying to anti-unify all pairs of nodes, because we know if there's no shared
    // structure at the top level, the cost of doing this would be strictly worse than not doing anything at all.
    // i.e.
    // anti-unify x, (+ 1 y) yields (\a. a) x, (\a. a) (+ 1 y), which yields no gains whatsoever
    //
    // we also need a way to add rewrite rules, either after the fact or during the process. this applies
    // for either the dfta method or the egraph method
    counter: Id,
}

impl IdInterner {
    /// Creates a new `IdInterner`.
    #[must_use] pub fn init(start: Id) -> Self {
        Self {
            states: BiHashMap::new(),
            counter: start,
        }
    }

    /// Gets the Id corresponding to the pair of two Ids given, if it exists.
    #[must_use] pub fn get_id(&self, a: Id, b: Id) -> Option<Id> {
        self.states.get_by_right(&IdPair::new(a, b)).copied()
    }

    /// Generates the Id corresponding to the pair of two Ids given.
    pub fn gen_id(&mut self, a: Id, b: Id) -> Id {
        let res = self.counter;
        self.states.insert(self.counter, IdPair::new(a, b));
        self.counter = (Into::<usize>::into(self.counter) + 1).into();
        res
    }

    /// Gets the Id corresponding to the pair of two Ids given if it exists;
    /// otherwise, generates this Id.
    pub fn get_or_gen(&mut self, a: Id, b: Id) -> Id {
        self.get_id(a, b).map_or_else(|| self.gen_id(a, b), |res| res)
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
    states: HashMap<Id, Vec<L>>,
}

impl<L: Language> Dfta<L> {
    /// Builds a new `Dfta` from an `EGraph`.
    pub fn build<N: Analysis<L>>(g: &EGraph<L, N>) -> Dfta<L> {
        let mut states = HashMap::new();
        for class in g.classes() {
            for node in class.iter() {
                states.entry(class.id).or_insert_with(Vec::new).push(node.clone());
            }
        }
        Self { states }
    }

    /// Adds a new transition to the DFTA.
    pub fn push(&mut self, (l, s): (L, Id)) {
        self.states.entry(s).or_insert_with(Vec::new).push(l);
    }

    /// Marks the given state as visited in the DFTA.
    pub fn push_empty(&mut self, s: Id) {
        self.states.entry(s).or_insert_with(Vec::new);
    }

    /// Checks if a state has been visited.
    #[must_use] pub fn has_visited(&self, s: Id) -> bool {
        self.states.contains_key(&s)
    }

    /// Get all the transitions which have this state as an output.
    #[must_use] pub fn get_by_state(&self, s: Id) -> Option<&Vec<L>> {
        self.states.get(&s)
    }
}

/// An `AntiUnifier` stores the state of an anti-unification invocation.
/// This struct should take in a single egraph with every program (both
/// library functions and synthesis solutions) compiled together.
/// After running anti-unification, we have a new egraph which contains
/// all possible libraries we could learn from the
#[derive(Debug)]
pub struct AntiUnifier<L: Language, N: Analysis<L>> {
    graph: EGraph<L, N>,

    dfta: Dfta<L>,
    interner: IdInterner,
    // A map of all newly-added nodes that we want to try to anti-unify
    // on the next iteration.
    // To avoid exponential blow-up, we purposely limit ourselves to
    // only anti-unifying newly added nodes together with each other,
    // rather than allowing ourselves to anti-unify new nodes with
    // old nodes.
    // TODO: is this a good optimization
    // TODO:
    // (1, 3) -> a1
    // (2, 4) -> a2
    // (1, 2) -> a3
    // (3, 4) -> a4
    // (a1, a2), (a3, a4) are equivalent, but we can't tell that...
    // TODO: For now, we only anti unify between pairs. Is this what we want?
    // newly_added: HashMap<::std::mem::Discriminant<L>, HashSet<Id>>,
    // we won't do multiple iterations - we just generate every possibility
    // from the worklist
    // init_worklist(egraph, size)
}

// TODO: go back from DFTA to EGraph with lambdas etc introduced
impl<L: Language, N: Analysis<L>> AntiUnifier<L, N> {
    /// Initialize an `AntiUnifier` from an `EGraph`.
    /// We first rebuild this egraph to make sure all its invariants hold.
    /// We then create a DFTA from it which we will use for anti-unification
    /// work.
    pub fn new(mut g: EGraph<L, N>) -> Self {
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
            // newly_added: HashMap::new(),
        }
    }

    fn init_worklist(g: &EGraph<L, N>) -> Vec<((L, Id), (L, Id))> {
        // TODO: Normally we'd use ::std::mem::Discriminant as an
        // enode hash, but clippy rejects it cause technically L isn't
        // always guaranteed to be an enum so...
        fn enode_hash<L: Language>(enode: &L) -> String {
            format!("{}_{}", enode.display_op(), enode.len())
        }

        let mut map: HashMap<String, Vec<(L, Id)>> = HashMap::new();
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

    /// Perform one iteration of anti-unification.
    pub fn anti_unify(&mut self) {
        let mut worklist = Self::init_worklist(&self.graph);

        while let Some((a, b)) = worklist.pop() {
            self.anti_unif_transitions(a, b);
        }
    }

    /// Anti-unifies two transition rules, potentially producing a new rule.
    /// If the operations in the two cases differ, we cannot anti-unify further,
    /// and will return None. Otherwise, if the operations are the same, we produce
    /// a new rule that anti-unifies within the arguments.
    ///
    /// This function assumes the two transitions match.
    fn anti_unif_transitions(&mut self, (la, sa): (L, Id), (lb, sb): (L, Id)) -> (L, Id) {
        assert!(la.matches(&lb));
        
        let out_state = self.interner.get_or_gen(sa, sb);
        // TODO: this hacky business is b/c we don't have .children(_mut) :/
        let children: HashMap<Id, (Id, Id)> = enode_children(&la)
            .into_iter()
            .zip(enode_children(&lb))
            .map(|(a, b)| (a, (a, b)))
            .collect();
        let mut lres = la.clone();
        lres.for_each_mut(|t| {
            let (a, b) = children[t];
            *t = self.interner.get_or_gen(a, b);
        });
        (lres, out_state)
    }
}

/// Anti-unifies within a given `EGraph`, returning an `AntiUnifier` as output.
pub fn anti_unify<L: Language, N: Analysis<L>>(g: EGraph<L, N>) -> AntiUnifier<L, N> {
    let mut a = AntiUnifier::new(g);
    a.anti_unify();
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
        let expr = r"
(let s1 (+ (move 4 4 (scale 2 line)) (+ (move 3 2 line) (+ (move 4 3 (scale 9 circle)) (move 5 2 line))))
  (let s2 (+ (move 4 4 (scale 2 circle)) (+ (move 3 2 circle) (+ (move 4 3 (scale 9 circle)) (move 5 2 circle))))
    (+ s1 s2)))".parse().unwrap();
        let runner = Runner::default()
            .with_expr(&expr)
            .run(rules);
        let (egraph, _root) = (runner.egraph, runner.roots[0]);

        let res = anti_unify(egraph);

        println!(
            "{:#?}",
            res.dfta
        );
    }
}
