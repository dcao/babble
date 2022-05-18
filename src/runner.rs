//! Convenience functions and methods for running library learning
//! experiments.

use std::{
    fs,
    time::{Duration, Instant},
};

use egg::{AstSize, CostFunction, EGraph, RecExpr, Rewrite, Runner};
use log::debug;

use crate::{
    ast_node::{Arity, AstNode, Expr, Pretty, Printable},
    extract::{
        beam::{LibExtractor, PartialLibCost},
        lift_libs, true_cost,
    },
    learn::LearnedLibrary,
    teachable::Teachable,
};

#[cfg(feature = "grb")]
use crate::extract::ilp::*;

/// When running experiments in babble, there are two types of experiments
/// we want to run: ILP-based experiments and beam-based experiments.
/// All library learning experiments fall into these two categories
#[derive(Debug)]
pub enum Experiment<Op, Extra>
where
    Op: std::fmt::Display + std::hash::Hash + Clone + Ord + 'static,
{
    /// A beam experiment
    Beam(BeamExperiment<Op, Extra>),
    /// An ILP experiment
    ILP(ILPExperiment<Op, Extra>),
}

impl<Op, Extra> Experiment<Op, Extra>
where
    Op: Teachable
        + Printable
        + Arity
        + Clone
        + Send
        + Sync
        + std::fmt::Debug
        + std::fmt::Display
        + std::hash::Hash
        + Ord
        + 'static,
    Extra: serde::ser::Serialize + std::fmt::Debug,
{
    /// Runs the experiment
    pub fn run(self, wtr: &mut csv::Writer<fs::File>) {
        match self {
            Experiment::Beam(b) => b.run(wtr),
            Experiment::ILP(b) => b.run(wtr),
        }
    }
}

/// A set of Experiments is just a list of individual Experiment structs
#[derive(Debug)]
pub struct Experiments<Op, Extra>
where
    Op: std::fmt::Display + std::hash::Hash + Clone + Ord + 'static,
{
    exps: Vec<Experiment<Op, Extra>>,
}

impl<Op, Extra> Experiments<Op, Extra>
where
    Op: Teachable
        + Printable
        + Arity
        + Clone
        + Send
        + Sync
        + std::fmt::Debug
        + std::fmt::Display
        + std::hash::Hash
        + Ord
        + 'static,
    Extra: serde::ser::Serialize + Clone + std::fmt::Debug,
{
    /// Creates a new empty set of experiments
    pub fn new() -> Self {
        Self { exps: Vec::new() }
    }

    /// Adds all the experiments from another experiment set into this one
    pub fn add(&mut self, other: Self) {
        self.exps.extend(other.exps);
    }

    // TODO: How to specify DSRs
    /// Generates a set of experiments from a set of params
    pub fn gen(
        expr: RecExpr<AstNode<Op>>,
        dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
        beams: Vec<usize>,
        mut extra_pors: Vec<bool>,
        timeouts: Vec<u64>,
        extra: Extra,
        learn_constants: bool,
    ) -> Self {
        let mut res = Vec::new();

        // Defaults for if we have empty values
        if extra_pors.is_empty() {
            extra_pors.push(false);
        }

        for beam in beams {
            for extra_por in &extra_pors {
                res.push(Experiment::Beam(BeamExperiment {
                    expr: expr.clone(),
                    dsrs: dsrs.clone(),
                    final_beams: beam,
                    inter_beams: beam,
                    extra_por: *extra_por,
                    extra_data: extra.clone(),
                    learn_constants,
                }));
            }
        }

        for timeout in timeouts {
            res.push(Experiment::ILP(ILPExperiment {
                expr: expr.clone(),
                dsrs: dsrs.clone(),
                timeout,
                extra_data: extra.clone(),
                learn_constants,
            }));
        }

        Self { exps: res }
    }

    /// Runs all experiments in this set
    pub fn run(self, csv_path: &str) {
        let mut wtr = csv::Writer::from_path(csv_path).unwrap();

        for exp in self.exps {
            exp.run(&mut wtr);
        }
    }
}

/// A BeamExperiment contains all of the information needed to run a
/// library learning experiment with the beam extractor.
#[derive(Debug)]
pub struct BeamExperiment<Op, Extra>
where
    Op: std::fmt::Display + std::hash::Hash + Clone + Ord + 'static,
{
    /// The expression to run the experiment over
    expr: RecExpr<AstNode<Op>>,
    /// The domain-specific rewrites to apply
    dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
    /// The final beam size to use
    final_beams: usize,
    /// The inter beam size to use
    inter_beams: usize,
    /// Whether to use the extra partial order reduction or not
    extra_por: bool,
    /// Any extra data associated with this experiment
    extra_data: Extra,
    /// Whether to learn "library functions" with no arguments.
    learn_constants: bool,
}

impl<Op, Extra> BeamExperiment<Op, Extra>
where
    Op: Teachable
        + Printable
        + Arity
        + Clone
        + Send
        + Sync
        + std::fmt::Debug
        + std::fmt::Display
        + std::hash::Hash
        + Ord
        + 'static,
    Extra: serde::ser::Serialize + std::fmt::Debug,
{
    fn run(self, wtr: &mut csv::Writer<fs::File>) {
        if self.final_beams > self.inter_beams {
            return;
        }

        println!(
            "beam | final_beams: {}, inter_beams: {}, extra_por: {}, extra_data: {:?}",
            self.final_beams, self.inter_beams, self.extra_por, self.extra_data
        );

        let start_time = Instant::now();
        let timeout = Duration::from_secs(60 * 100000);

        let initial_cost = true_cost(self.expr.clone());

        println!("Starting cost: {}", initial_cost);

        let mut aeg = EGraph::new(PartialLibCost::new(
            self.final_beams,
            self.inter_beams,
            self.extra_por,
        ));
        let root = aeg.add_expr(&self.expr);
        aeg.rebuild();

        print!("Running {} DSRs... ", self.dsrs.len());

        let runner = Runner::<_, _, ()>::new(PartialLibCost::new(0, 0, false))
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
        print!("Anti-unifying... ");

        let runner = Runner::<_, _, ()>::new(PartialLibCost::new(
            self.final_beams,
            self.inter_beams,
            self.extra_por,
        ))
        .with_egraph(aeg.clone())
        .with_iter_limit(1)
        .with_time_limit(timeout)
        .with_node_limit(1_000_000)
        .run(lib_rewrites.iter());

        println!("Finished in {}ms", anti_time.elapsed().as_millis());
        println!("Stop reason: {:?}", runner.stop_reason.unwrap());

        let egraph = runner.egraph;
        println!("Number of nodes: {}", egraph.total_size());

        let mut cs = egraph[egraph.find(root)].data.clone();
        cs.set.sort_unstable_by_key(|elem| elem.full_cost);

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
                let fin = Runner::<_, _, ()>::new(PartialLibCost::new(0, 0, false))
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
        println!("{}", Pretty(&Expr::from(lifted)));
        println!("final cost: {}", final_cost);
        println!("final time: {}ms", start_time.elapsed().as_millis());
        println!();

        wtr.serialize((
            "beam",
            timeout.as_secs(),
            self.final_beams,
            self.inter_beams,
            self.extra_por,
            self.extra_data,
            initial_cost,
            final_cost,
            start_time.elapsed().as_secs_f64(),
        ))
        .unwrap();
        wtr.flush().unwrap();
    }
}

/// An ILPExperiment contains all of the info needed to run a library
/// learning experiment with the ILP extractor.
#[derive(Debug)]
pub struct ILPExperiment<Op, Extra>
where
    Op: std::fmt::Display + std::hash::Hash + Clone + Ord + 'static,
{
    /// The expression to run the experiment over
    expr: RecExpr<AstNode<Op>>,
    /// The domain-specific rewrites to apply
    dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
    /// The timeout length to use
    timeout: u64,
    /// Any extra data associated with this experiment
    extra_data: Extra,
    /// Whether to learn "library functions" without any arguments.
    learn_constants: bool,
}

#[cfg(feature = "grb")]
impl<Op, Extra> ILPExperiment<Op, Extra>
where
    Op: Teachable
        + Printable
        + Arity
        + Clone
        + Send
        + Sync
        + std::fmt::Debug
        + std::fmt::Display
        + std::hash::Hash
        + Ord
        + 'static,
    Extra: serde::ser::Serialize + std::fmt::Debug,
{
    fn run(self, wtr: &mut csv::Writer<fs::File>) {
        println!(
            "ilp | timeout: {}, extra_data: {:?}",
            self.timeout, self.extra_data
        );

        let start_time = Instant::now();
        let timeout = Duration::from_secs(self.timeout);

        let initial_cost = true_cost(self.expr.clone());

        println!("Starting cost: {}", initial_cost);

        let mut aeg = EGraph::new(());
        let root = aeg.add_expr(&self.expr);
        aeg.rebuild();

        print!("Running {} DSRs... ", self.dsrs.len());

        let runner = Runner::<_, _, ()>::new(())
            .with_egraph(aeg)
            .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
            .run(&[
                // egg::rewrite!("add comm"; "(@ (@ + ?x) ?y)" => "(@ (@ + ?y) ?x)"),
                // egg::rewrite!("len range"; "(@ length (@ range ?x))" => "?x"),
            ]);

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
        print!("Anti-unifying... ");

        let runner = Runner::<_, _, ()>::new(())
            .with_egraph(aeg.clone())
            .with_iter_limit(1)
            .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
            .with_node_limit(1_000_000)
            .run(lib_rewrites.iter());

        println!("Finished in {}ms", anti_time.elapsed().as_millis());
        println!("Stop reason: {:?}", runner.stop_reason.unwrap());

        let egraph = runner.egraph;
        println!("Number of nodes: {}", egraph.total_size());

        let best = LpExtractor::new(&egraph, egg::AstSize)
            .timeout(timeout.saturating_sub(start_time.elapsed()).as_secs_f64())
            .solve(root);

        let ex_time = Instant::now();
        print!("Extracting... ");
        let lifted = lift_libs(best);
        let final_cost = true_cost(lifted.clone()) - 1;

        println!("Finished in {}ms", ex_time.elapsed().as_millis());
        println!("{}", Pretty(&Expr::from(lifted)));
        println!("final cost: {}", final_cost);
        println!("final time: {}ms", start_time.elapsed().as_millis());
        println!();

        wtr.serialize((
            "ilp",
            timeout.as_secs(),
            0,
            0,
            self.extra_data,
            initial_cost,
            final_cost,
            start_time.elapsed().as_secs_f64(),
        ))
        .unwrap();
        wtr.flush().unwrap();
    }
}

#[cfg(not(feature = "grb"))]
impl<Op, Extra> ILPExperiment<Op, Extra>
where
    Op: std::fmt::Display + std::hash::Hash + Clone + Ord + 'static,
{
    fn run(self, _wtr: &mut csv::Writer<fs::File>) {}
}
