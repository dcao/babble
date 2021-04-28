//! Implements anti-unification between two egraphs.

use egg::{Analysis, EGraph, Id, Language};
use hashbrown::HashMap;
use smallvec::{SmallVec, smallvec};

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
    counter: Id,
}

impl IdInterner {
    /// Creates a new `IdInterner`.
    #[must_use] pub fn init(start: Id) -> Self {
        let mut states = HashMap::new();
        let mut sets = HashMap::new();
        for i in 0..start.into() {
            states.insert(i.into(), smallvec![i.into()]);
            sets.insert(smallvec![i.into()], i.into());
        }

        Self { states, sets, counter: start }
    }

    /// Gets the set corresponding to two ids.
    ///
    /// # Panics
    /// Panics if a and b aren't in Ids.
    #[must_use] pub fn lookup(&self, a: Id, b: Id) -> SmallVec<[Id; 4]> {
        let states_a = self.states.get(&a).unwrap();
        let states_b = self.states.get(&b).unwrap();
        let mut res = states_a.clone();
        for bid in &*states_b {
            match res.binary_search(&bid) {
                Ok(_) => {},
                Err(ix) => { res.insert(ix, *bid); },
            }
        }

        res
    }

    /// Gets the Id corresponding to the pair of two Ids given, if it exists.
    #[must_use] pub fn get_id(&self, a: Id, b: Id) -> Option<Id> {
        self.sets.get(&self.lookup(a, b)).copied()
    }

    /// Generates the Id corresponding to the pair of two Ids given.
    pub fn gen_id(&mut self, a: Id, b: Id) -> Id {
        let res = self.counter;
        self.states.insert(self.counter, self.lookup(a, b));
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
    states: HashMap<Id, SmallVec<[L; 8]>>,
}

impl<L: Language> Dfta<L> {
    /// Builds a new `Dfta` from an `EGraph`.
    pub fn build<N: Analysis<L>>(g: &EGraph<L, N>) -> Dfta<L> {
        let mut states = HashMap::new();
        for class in g.classes() {
            for node in class.iter() {
                states.entry(class.id).or_insert_with(SmallVec::new).push(node.clone());
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
    #[must_use] pub fn has_visited(&self, s: Id) -> bool {
        self.states.contains_key(&s)
    }

    /// Get all the transitions which have this state as an output.
    #[must_use] pub fn get_by_state(&self, s: Id) -> Option<&SmallVec<[L; 8]>> {
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
        fn enode_hash<L: Language>(enode: &L) -> String {
            format!("{}_{}", enode.display_op(), enode.len())
        }

        let mut map: HashMap<_, Vec<(L, Id)>> = HashMap::new();
        for class in g.classes() {
            for node in class.iter() {
                let hash = enode_hash(node); // enode_hash(node);
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
        let worklist = Self::init_worklist(&self.graph);

        for (a, b) in worklist {
            let t = self.anti_unif_transitions(a, b);
            self.dfta.push(t);
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
        let mut lres = la;
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
