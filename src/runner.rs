//! Convenience functions and methods for running library learning
//! experiments.

use std::{
    collections::HashMap,
    fmt::{self, Debug, Display, Formatter},
    fs,
    hash::Hash,
    io::Write,
    marker::PhantomData,
    time::{Duration, Instant},
};

use egg::{AstSize, CostFunction, EGraph, RecExpr, Rewrite, Runner};
use log::debug;

use crate::{
    ast_node::{self, Arity, AstNode, Expr, Pretty, Printable},
    extract::{
        beam::{LibExtractor, LibsPerSel, PartialLibCost},
        lift_libs,
    },
    learn::LearnedLibrary,
    teachable::Teachable,
};

#[cfg(feature = "grb")]
use crate::extract::ilp::*;

struct ExperimentTitle<
    'a,
    Op: Printable + Teachable + Hash + Clone + Debug + Arity + Ord,
    T: Experiment<Op> + ?Sized,
> {
    experiment: &'a T,
    phantom: PhantomData<Op>,
}
impl<'a, Op, T: Experiment<Op> + ?Sized> Display for ExperimentTitle<'a, Op, T>
where
    Op: Printable + Teachable + Hash + Clone + Debug + Arity + Ord,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.experiment.fmt_title(f)
    }
}

pub trait Experiment<Op>
where
    Op: Printable + Teachable + Hash + Clone + Debug + Arity + Ord,
{
    // Ideally exprs would have type `I: IntoIterator<Item = Expr<Op>>` but that's not object-safe.
    fn run(&self, exprs: Vec<Expr<Op>>) -> RecExpr<AstNode<Op>>;

    fn rounds(&self) -> usize;

    fn write_to_csv(
        &self,
        writer: &mut csv::Writer<fs::File>,
        initial_cost: usize,
        final_cost: usize,
        time_elapsed: Duration,
    );

    fn fmt_title(&self, f: &mut Formatter<'_>) -> fmt::Result;

    fn run_csv(&self, mut exprs: Vec<Expr<Op>>, writer: &mut csv::Writer<fs::File>)
    {
        println!(
            "{}",
            ExperimentTitle {
                experiment: self,
                phantom: PhantomData
            }
        );

        let start_time = Instant::now();

        let recexprs: Vec<RecExpr<AstNode<Op>>> =
            exprs.clone().into_iter().map(|x| x.into()).collect();

        // Add one to account for root node, not added yet
        let initial_cost = {
            let s: usize = recexprs.iter().map(|x| AstSize.cost_rec(x)).sum();
            s + 1
        };

        // Start running our rounds
        let rounds = self.rounds();

        let mut libs = HashMap::new();

        let mut rc: RecExpr<AstNode<Op>>;

        for r in 0..rounds {
            println!("round {}/{}", r + 1, rounds);

            rc = self.run(exprs);

            // If rc is empty, just return early.
            if rc.as_ref().is_empty() {
                println!("empty recexpr, returning early");
                return;
            }

            let ls = plumbing::libs(rc.as_ref(), libs.len());
            libs.extend(ls);
            exprs = plumbing::exprs(rc.as_ref());
        }

        // Combine back into one big recexpr at the end
        rc = plumbing::combine(libs, exprs);
        let final_cost = AstSize.cost_rec(&rc);
        let time_elapsed = start_time.elapsed();

        // Print our analysis on this
        if rounds > 1 {
            println!("Final beam results");
            println!("{}", Pretty(&Expr::from(rc)));
            println!(
                "cost diff: {} -> {} (compression ratio {})",
                initial_cost,
                final_cost,
                final_cost as f64 / initial_cost as f64
            );
            println!("total time: {}ms", time_elapsed.as_millis());
            println!();
        }

        self.write_to_csv(writer, initial_cost, final_cost, time_elapsed)
    }
}

// /// When running experiments in babble, there are two types of experiments
// /// we want to run: ILP-based experiments and beam-based experiments.
// /// All library learning experiments fall into these two categories
// #[derive(Debug)]
// pub enum Experiment<Op, Extra>
// where
//     Op: std::fmt::Display + std::hash::Hash + Clone + Ord + 'static,
// {
//     /// A beam experiment
//     Beam(BeamExperiment<Op, Extra>),
//     /// An ILP experiment
//     ILP(ILPExperiment<Op, Extra>),
// }

// impl<Op, Extra> Experiment<Op, Extra>
// where
//     Op: Teachable
//         + Printable
//         + Arity
//         + Clone
//         + Send
//         + Sync
//         + std::fmt::Debug
//         + std::fmt::Display
//         + std::hash::Hash
//         + Ord
//         + 'static,
//     Extra: serde::ser::Serialize + std::fmt::Debug + Clone,
// {
/// Runs the experiment
/// If the experiment requires multiple rounds, run multiple times.
// }

/// A set of Experiments is just a list of individual Experiment structs
pub struct Experiments<Op> {
    experiments: Vec<Box<dyn Experiment<Op>>>,
    exprs: Vec<Expr<Op>>,
}

impl<Op> Experiments<Op>
where
    Op: Teachable
        + Printable
        + Arity
        + Clone
        + Send
        + Sync
        + fmt::Debug
        + fmt::Display
        + std::hash::Hash
        + Ord
        + 'static,
{
    /// Creates a new empty set of experiments
    pub fn new() -> Self {
        Self {
            experiments: Vec::new(),
            exprs: Vec::new(),
        }
    }

    /// Adds all the experiments from another experiment set into this one
    pub fn add(&mut self, other: Self) {
        self.experiments.extend(other.experiments);
    }

    // TODO: How to specify DSRs
    /// Generates a set of experiments from a set of params
    pub fn gen<Extra>(
        exprs: Vec<Expr<Op>>,
        dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
        mut beams: Vec<usize>,
        mut lpss: Vec<LibsPerSel>,
        mut rounds: Vec<usize>,
        mut extra_pors: Vec<bool>,
        timeouts: Vec<u64>,
        extra: Extra,
        learn_constants: bool,
    ) -> Self
    where
        Extra: serde::ser::Serialize + Clone + fmt::Debug + Clone + 'static,
    {
        let mut res: Vec<Box<dyn Experiment<Op>>> = Vec::new();

        // Defaults for if we have empty values
        if beams.is_empty() {
            beams.push(25);
        }

        if rounds.is_empty() {
            rounds.push(1);
        }

        if lpss.is_empty() {
            lpss.push(LibsPerSel::Unlimited);
        }

        if extra_pors.is_empty() {
            extra_pors.push(false);
        }

        for beam in beams {
            for extra_por in &extra_pors {
                for lps in &lpss {
                    for rounds in &rounds {
                        res.push(Box::new(BeamExperiment {
                            exprs: exprs.clone(),
                            dsrs: dsrs.clone(),
                            final_beams: beam,
                            inter_beams: beam,
                            lps: *lps,
                            rounds: *rounds,
                            extra_por: *extra_por,
                            extra_data: extra.clone(),
                            learn_constants,
                        }));
                    }
                }
            }
        }

        for timeout in timeouts {
            for r in &rounds {
                res.push(Box::new(ILPExperiment {
                    exprs: exprs.clone(),
                    dsrs: dsrs.clone(),
                    timeout,
                    rounds: *r,
                    extra_data: extra.clone(),
                    learn_constants,
                }));
            }
        }

        Self {
            exprs,
            experiments: res,
        }
    }

    /// Runs all experiments in this set
    pub fn run(self, csv_path: &str) {
        let mut writer = csv::Writer::from_path(csv_path).unwrap();

        for experiment in self.experiments {
            experiment.run_csv(self.exprs.clone(), &mut writer);
        }
    }
}

/// A BeamExperiment contains all of the information needed to run a
/// library learning experiment with the beam extractor.
#[derive(Debug)]
pub struct BeamExperiment<Op, Extra>
where
    Op: fmt::Display + std::hash::Hash + Clone + Ord + 'static,
{
    /// The expressions to run the experiment over
    exprs: Vec<Expr<Op>>,
    /// The domain-specific rewrites to apply
    dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
    /// The final beam size to use
    final_beams: usize,
    /// The inter beam size to use
    inter_beams: usize,
    /// The number of libs to learn at a time
    lps: LibsPerSel,
    /// The number of rounds of library learning to do
    rounds: usize,
    /// Whether to use the extra partial order reduction or not
    extra_por: bool,
    /// Any extra data associated with this experiment
    extra_data: Extra,
    /// Whether to learn "library functions" with no arguments.
    learn_constants: bool,
}

impl<Op, Extra> Experiment<Op> for BeamExperiment<Op, Extra>
where
    Op: Teachable
        + Printable
        + Arity
        + Clone
        + Send
        + Sync
        + fmt::Debug
        + fmt::Display
        + std::hash::Hash
        + Ord
        + 'static,
    Extra: serde::ser::Serialize + fmt::Debug + Clone,
{
    // // TODO: Generalize this for ILP too
    // fn run_csv(&self, wtr: &mut csv::Writer<fs::File>) {
    //     if self.final_beams > self.inter_beams {
    //         return;
    //     }

    //     println!(
    //         "beam | final_beams: {}, inter_beams: {}, lps: {:?}, rounds: {}, extra_por: {}, extra_data: {:?}",
    //         self.final_beams, self.inter_beams, self.lps, self.rounds, self.extra_por, self.extra_data
    //     );

    //     let start_time = Instant::now();

    //     // First, let's turn our list of exprs into a list of recexprs
    //     let recexprs: Vec<RecExpr<AstNode<Op>>> =
    //         self.exprs.clone().into_iter().map(|x| x.into()).collect();

    //     // Add one to account for root node, not added yet
    //     let initial_cost = {
    //         let s: usize = recexprs.iter().map(|x| AstSize.cost_rec(x)).sum();
    //         s + 1
    //     };

    //     // Start running our rounds
    //     let mut libs = HashMap::new();
    //     let mut exprs = self.exprs.clone();

    //     let mut rc: RecExpr<AstNode<Op>>;

    //     for r in 0..self.rounds {
    //         println!("round {}/{}", r + 1, self.rounds);

    //         rc = self.run_one(exprs);

    //         let ls = plumbing::libs(rc.as_ref(), libs.len());
    //         libs.extend(ls);
    //         exprs = plumbing::exprs(rc.as_ref());
    //     }

    //     // Combine back into one big recexpr at the end
    //     rc = plumbing::combine(libs, exprs);
    //     let final_cost = AstSize.cost_rec(&rc);

    //     // Print our analysis on this
    //     if self.rounds > 1 {
    //         println!("Final beam results");
    //         println!("{}", Pretty(&Expr::from(rc)));
    //         println!(
    //             "cost diff: {} -> {} (compression ratio {})",
    //             initial_cost,
    //             final_cost,
    //             final_cost as f32 / initial_cost as f32
    //         );
    //         println!("round time: {}ms", start_time.elapsed().as_millis());
    //         println!();
    //     }

    //     wtr.serialize((
    //         "beam",
    //         0,
    //         self.final_beams,
    //         self.inter_beams,
    //         self.lps,
    //         self.rounds,
    //         self.extra_por,
    //         self.extra_data.clone(),
    //         initial_cost,
    //         final_cost,
    //         start_time.elapsed().as_secs_f64(),
    //     ))
    //     .unwrap();
    //     wtr.flush().unwrap();
    // }

    fn run(&self, exprs: Vec<Expr<Op>>) -> RecExpr<AstNode<Op>> {
        let start_time = Instant::now();
        let timeout = Duration::from_secs(60 * 100000);

        // First, let's turn our list of exprs into a list of recexprs
        let recexprs: Vec<RecExpr<AstNode<Op>>> =
            exprs.clone().into_iter().map(|x| x.into()).collect();

        // Add one to account for root node, not added yet
        let initial_cost = {
            let s: usize = recexprs.iter().map(|x| AstSize.cost_rec(x)).sum();
            s + 1
        };

        println!("Starting cost: {}", initial_cost);

        let mut aeg = EGraph::new(PartialLibCost::new(
            self.final_beams,
            self.inter_beams,
            self.lps,
            self.extra_por,
        ));
        let roots = recexprs.iter().map(|x| aeg.add_expr(x)).collect::<Vec<_>>();
        aeg.rebuild();

        print!("Running {} DSRs... ", self.dsrs.len());

        let runner = Runner::<_, _, ()>::new(PartialLibCost::empty())
            .with_egraph(aeg)
            .with_time_limit(timeout)
            .run(&self.dsrs);

        let aeg = runner.egraph;

        println!("Finished in {}ms", start_time.elapsed().as_millis());
        print!("Running anti-unification... ");

        let ll_time = Instant::now();

        let learned_lib = LearnedLibrary::new(&aeg, self.learn_constants);
        let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();

        println!(
            "Found {} antiunifications in {}ms",
            lib_rewrites.len(),
            ll_time.elapsed().as_millis()
        );

        let anti_time = Instant::now();
        print!("Rewriting egraph... ");

        let runner = Runner::<_, _, ()>::new(PartialLibCost::new(
            self.final_beams,
            self.inter_beams,
            self.lps,
            self.extra_por,
        ))
        .with_egraph(aeg.clone())
        .with_iter_limit(1)
        .with_time_limit(timeout)
        .with_node_limit(1_000_000)
        .run(lib_rewrites.iter());

        println!("Finished in {}ms", anti_time.elapsed().as_millis());
        println!("Stop reason: {:?}", runner.stop_reason.unwrap());

        let mut egraph = runner.egraph;
        println!("Number of nodes: {}", egraph.total_size());

        let root_time = Instant::now();
        print!("Adding root node... ");

        let root = egraph.add(AstNode::new(Op::list(), roots.iter().copied()));

        let mut cs = egraph[egraph.find(root)].data.clone();
        cs.set.sort_unstable_by_key(|elem| elem.full_cost);

        println!("Finished in {}ms", root_time.elapsed().as_millis());

        debug!("learned libs");
        let all_libs: Vec<_> = learned_lib.libs().collect();
        for lib in &cs.set[0].libs {
            debug!("{}: {}", lib.0, &all_libs[lib.0 .0]);
        }

        println!("upper bound ('full') cost: {}", cs.set[0].full_cost);

        let ex_time = Instant::now();
        print!("Extracting... ");
        let (lifted, final_cost) = cs
            .set
            // .par_iter()
            .iter()
            .take(1)
            .map(|ls| {
                // Add the root combine node again
                let mut fin = Runner::<_, _, ()>::new(PartialLibCost::empty())
                    .with_egraph(aeg.clone())
                    .with_iter_limit(1)
                    .run(
                        lib_rewrites
                            .iter()
                            .enumerate()
                            .filter(|(i, _)| ls.libs.iter().any(|x| *i == x.0 .0))
                            .map(|x| x.1),
                    )
                    .egraph;
                let root = fin.add(AstNode::new(Op::list(), roots.iter().copied()));

                let mut extractor = LibExtractor::new(&fin);
                let best = extractor.best(root);

                // println!("extracting (before lib lifting)");
                // println!("{}", best.pretty(100));
                // println!();

                let lifted = lift_libs(best);
                // let final_cost = true_cost(lifted.clone());
                let final_cost = AstSize.cost_rec(&lifted);

                (lifted, final_cost)
            })
            .min_by_key(|x| x.1)
            .unwrap();

        println!("Finished in {}ms", ex_time.elapsed().as_millis());
        println!("{}", Pretty(&Expr::from(lifted.clone())));
        println!(
            "cost diff: {} -> {} (compression ratio {})",
            initial_cost,
            final_cost,
            final_cost as f32 / initial_cost as f32
        );
        println!("round time: {}ms", start_time.elapsed().as_millis());
        println!();

        return lifted;
    }

    fn rounds(&self) -> usize {
        self.rounds
    }

    fn write_to_csv(
        &self,
        writer: &mut csv::Writer<fs::File>,
        initial_cost: usize,
        final_cost: usize,
        time_elapsed: Duration,
    ) {
        writer
            .serialize((
                "beam",
                0,
                self.final_beams,
                self.inter_beams,
                self.lps,
                self.rounds,
                self.extra_por,
                self.extra_data.clone(),
                initial_cost,
                final_cost,
                time_elapsed.as_secs_f64(),
            ))
            .unwrap();
        writer.flush().unwrap();
    }

    fn fmt_title(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "beam | final_beams: {}, inter_beams: {}, lps: {:?}, rounds: {}, extra_por: {}, extra_data: {:?}",
               self.final_beams, self.inter_beams, self.lps, self.rounds, self.extra_por, self.extra_data)
    }
}

/// An ILPExperiment contains all of the info needed to run a library
/// learning experiment with the ILP extractor.
#[derive(Debug)]
pub struct ILPExperiment<Op, Extra>
where
    Op: fmt::Display + std::hash::Hash + Clone + Ord + 'static,
{
    /// The expression to run the experiment over
    exprs: Vec<Expr<Op>>,
    /// The domain-specific rewrites to apply
    dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
    /// The timeout length to use
    timeout: u64,
    /// Number of rounds to do
    rounds: usize,
    /// Any extra data associated with this experiment
    extra_data: Extra,
    /// Whether to learn "library functions" without any arguments.
    learn_constants: bool,
}

//#[cfg(feature = "grb")]
impl<Op, Extra> Experiment<Op> for ILPExperiment<Op, Extra>
where
    Op: Teachable
        + Printable
        + Arity
        + Clone
        + Send
        + Sync
        + fmt::Debug
        + fmt::Display
        + std::hash::Hash
        + Ord
        + 'static,
    Extra: serde::ser::Serialize + fmt::Debug + Clone,
{
    #[cfg(not(feature = "grb"))]
    fn run(&self, exprs: Vec<Expr<Op>>) -> RecExpr<AstNode<Op>> {
        unimplemented!("feature `grb` not enabled");
    }

    #[cfg(feature = "grb")]
    fn run(&self, exprs: Vec<Expr<Op>>) -> RecExpr<AstNode<Op>> {
        let start_time = Instant::now();
        let timeout = Duration::from_secs(self.timeout);

        // First, let's turn our list of exprs into a list of recexprs
        let recexprs: Vec<RecExpr<AstNode<Op>>> =
            exprs.clone().into_iter().map(|x| x.into()).collect();

        // Add one to account for root node, not added yet
        let initial_cost = {
            let s: usize = recexprs.iter().map(|x| AstSize.cost_rec(x)).sum();
            s + 1
        };

        println!("Starting cost: {}", initial_cost);

        let mut aeg = EGraph::new(());
        let roots = recexprs.iter().map(|x| aeg.add_expr(x)).collect::<Vec<_>>();
        aeg.rebuild();

        // FIXME: Right now we're not running DSRs with the ILP experiment
        //        This is because of some type weirdness (Rewrites take in an analysis as a param)
        //        and ideally I'd like to have one list of rewrites, not two sets of rewrites for
        //        ILP and beam
        println!("FIXME: no dsrs are bein run rn!");
        print!("Running {} DSRs... ", self.dsrs.len());

        let runner = Runner::<_, _, ()>::new(())
            .with_egraph(aeg)
            .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
            .run(&[]);

        let aeg = runner.egraph;

        println!("Finished in {}ms", start_time.elapsed().as_millis());
        print!("Running anti-unification... ");

        let ll_time = Instant::now();

        let learned_lib = LearnedLibrary::new(&aeg, self.learn_constants);
        let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();

        println!(
            "Found {} antiunifications in {}ms",
            lib_rewrites.len(),
            ll_time.elapsed().as_millis()
        );

        let anti_time = Instant::now();
        print!("Rewriting egraph... ");

        let runner = Runner::<_, _, ()>::new(())
            .with_egraph(aeg.clone())
            .with_iter_limit(1)
            .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
            .with_node_limit(1_000_000)
            .run(lib_rewrites.iter());

        println!("Finished in {}ms", anti_time.elapsed().as_millis());
        println!("Stop reason: {:?}", runner.stop_reason.unwrap());

        let mut egraph = runner.egraph;
        println!("Number of nodes: {}", egraph.total_size());

        let root_time = Instant::now();
        print!("Adding root node... ");

        let root = egraph.add(AstNode::new(Op::list(), roots.iter().copied()));
        println!("Finished in {}ms", root_time.elapsed().as_millis());

        let ex_time = Instant::now();
        print!("Extracting... ");

        let best = LpExtractor::new(&egraph, egg::AstSize)
            .timeout(timeout.saturating_sub(start_time.elapsed()).as_secs_f64())
            .solve(root);

        let lifted = lift_libs(best);
        let final_cost = AstSize.cost_rec(&lifted) - 1;

        println!("Finished in {}ms", ex_time.elapsed().as_millis());
        println!("{}", Pretty(&Expr::from(lifted.clone())));
        println!(
            "cost diff: {} -> {} (compression ratio {})",
            initial_cost,
            final_cost,
            final_cost as f32 / initial_cost as f32
        );
        println!("round time: {}ms", start_time.elapsed().as_millis());
        println!();

        return lifted;
    }

    fn rounds(&self) -> usize {
        self.rounds
    }

    fn write_to_csv(
        &self,
        writer: &mut csv::Writer<fs::File>,
        initial_cost: usize,
        final_cost: usize,
        time_elapsed: Duration,
    ) {
        writer
            .serialize((
                "ilp",
                self.timeout,
                0,
                0,
                "Unlimited",
                self.rounds,
                false,
                self.extra_data.clone(),
                initial_cost,
                final_cost,
                time_elapsed.as_secs_f64(),
            ))
            .unwrap();
        writer.flush().unwrap();
    }

    fn fmt_title(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ilp | timeout: {}, extra_data: {:?}",
            self.timeout, self.extra_data
        )
    }
}

/// Defines some helper functions for finagling with the results of a library learning run.
/// These runs return a single RecExpr, but when running library learning multiple times in
/// a row, we need to get the defined libs and individual expressions out from this single RecExpr;
/// this is what the functions in this module are for.
mod plumbing {
    use std::collections::HashMap;

    use egg::{Id, Language, RecExpr};

    use crate::{
        ast_node::{Arity, AstNode, Expr},
        learn::LibId,
        teachable::Teachable,
    };

    /// The result of running library learning after one pass.
    type LLRes<'a, Op> = &'a [AstNode<Op>];

    /// At the end of all rounds, combine libs hashmap, and list of exprs back into one big recexpr
    pub(crate) fn combine<Op>(
        libs: HashMap<LibId, Vec<AstNode<Op>>>,
        exprs: Vec<Expr<Op>>,
    ) -> RecExpr<AstNode<Op>>
    where
        Op: Teachable + std::fmt::Debug + std::hash::Hash + Clone + Arity + Ord,
    {
        // First, build our root "combine" node
        let root_list = AstNode::new(Op::list(), std::iter::repeat(Id::from(0)).take(exprs.len()));
        let mut exprs_iter = exprs.into_iter().map(|x| RecExpr::from(x));
        let mut res = root_list.join_recexprs(|_id| exprs_iter.next().unwrap());

        // Then, add our libs back in
        for (libid, body) in libs {
            let root = Op::lib(libid, Id::from(0), Id::from(0));
            let mut children = vec![body.into(), res].into_iter();

            res = root.join_recexprs(|_id| children.next().unwrap());
        }

        // And we're done!
        res
    }

    /// Gets all of the libs and their defns out of the result of a lib learning pass.
    /// We take into account the current number of libs defined so that we don't overwrite existing
    /// libs from previous runs.
    pub(crate) fn libs<Op>(llr: LLRes<'_, Op>, cur_libs: usize) -> HashMap<LibId, Vec<AstNode<Op>>>
    where
        Op: Teachable + Clone + std::hash::Hash + Ord + std::fmt::Debug,
    {
        // For our strategy, we start at the root and walk downwards.
        // While we see libs, add its body to the hashmap and keep moving
        // If we don't see a lib, that means we've hit the non-libs section,
        // so we give up on the spot.
        let mut res = HashMap::new();

        fn walk<Op>(
            from: LLRes<'_, Op>,
            res: &mut HashMap<LibId, Vec<AstNode<Op>>>,
            cur_libs: usize,
            ix: Id,
        ) where
            Op: Teachable + Clone + std::hash::Hash + Ord + std::fmt::Debug,
        {
            // Check what kind of node we're at.
            match &from[usize::from(ix)].as_binding_expr() {
                Some(crate::teachable::BindingExpr::Lib(lid, defn, b)) => {
                    // Extract recursive expression
                    let rc = (&from[usize::from(**defn)])
                        .build_recexpr(|x| from[usize::from(x)].clone());

                    // Push to res
                    res.insert(LibId(lid.0 + cur_libs), rc.as_ref().to_vec());
                    // Recursively walk in body
                    walk(from, res, cur_libs, **b);
                }
                _ => {} // no-op
            }
        }

        // Walk starting from root
        walk(llr, &mut res, cur_libs, Id::from(llr.len() - 1));

        res
    }

    /// Returns a list of rewritten expressions from the result of a lib learning pass.
    pub(crate) fn exprs<Op>(llr: LLRes<'_, Op>) -> Vec<Expr<Op>>
    where
        Op: Teachable + Clone + std::hash::Hash + Ord + std::fmt::Debug,
    {
        // Start at the root and walk downwards.
        // We assume the first non-lib node we see is the root "list/combine" node.
        // For each of the children, extract expressions for those.
        let mut res = Vec::new();

        fn walk<Op>(from: LLRes<'_, Op>, res: &mut Vec<Expr<Op>>, ix: Id)
        where
            Op: Teachable + Clone + std::hash::Hash + Ord + std::fmt::Debug,
        {
            // Check what kind of node we're at.
            match &from[usize::from(ix)].as_binding_expr() {
                Some(crate::teachable::BindingExpr::Lib(_, _, b)) => {
                    // Recursively walk in body
                    walk(from, res, **b);
                }
                _ => {
                    // Get children of current node
                    from[usize::from(ix)].for_each(|c| {
                        // Extract recursive expression
                        let rc =
                            (&from[usize::from(c)]).build_recexpr(|x| from[usize::from(x)].clone());
                        // Convert into expr
                        let e = Expr::from(rc);
                        // Push to res
                        res.push(e);
                    });
                }
            }
        }

        // Walk starting from root
        walk(llr, &mut res, Id::from(llr.len() - 1));

        res
    }
}
