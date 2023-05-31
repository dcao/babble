use std::{
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    time::{Duration, Instant},
};

use egg::{AstSize, CostFunction, EGraph, Id, RecExpr, Rewrite, Runner};
use itertools::Itertools;
use log::{debug, info};
use serde::ser::Serialize;

use crate::{
    ast_node::{Arity, AstNode, Expr, Pretty, Printable},
    co_occurrence::COBuilder,
    extract::{apply_libs, beam::PartialLibCost},
    learn::LearnedLibrary,
    teachable::Teachable,
};

use super::{CsvWriter, Experiment, ExperimentResult};

/// A `BeamExperiment` contains all of the information needed to run a
/// library learning experiment with the beam extractor.
#[derive(Debug)]
pub struct BeamExperiment<Op, Extra>
where
    Op: Display + Hash + Clone + Ord + 'static,
{
    /// The domain-specific rewrites to apply
    dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
    /// The final beam size to use
    final_beams: usize,
    /// The inter beam size to use
    inter_beams: usize,
    /// The number of times to apply library rewrites
    lib_iter_limit: usize,
    /// The number of libs to learn at a time
    lps: usize,
    /// Any extra data associated with this experiment
    extra_data: Extra,
    /// Whether to learn "library functions" with no arguments.
    learn_constants: bool,
    /// Maximum arity of a library function.
    max_arity: Option<usize>,
}

impl<Op, Extra> BeamExperiment<Op, Extra>
where
    Op: Arity
        + Teachable
        + Printable
        + Debug
        + Display
        + Hash
        + Clone
        + Ord
        + Sync
        + Send
        + 'static,
{
    // TODO: Use a builder pattern.
    #[allow(clippy::too_many_arguments)]
    pub fn new<I>(
        dsrs: I,
        final_beams: usize,
        inter_beams: usize,
        lps: usize,
        extra_data: Extra,
        learn_constants: bool,
        max_arity: Option<usize>,
        lib_iter_limit: usize,
    ) -> Self
    where
        I: IntoIterator<Item = Rewrite<AstNode<Op>, PartialLibCost>>,
    {
        Self {
            dsrs: dsrs.into_iter().collect(),
            final_beams,
            inter_beams,
            lps,
            extra_data,
            learn_constants,
            max_arity,
            lib_iter_limit,
        }
    }

    fn run_egraph(
        &self,
        roots: &[Id],
        egraph: EGraph<AstNode<Op>, PartialLibCost>,
    ) -> ExperimentResult<Op> {
        let start_time = Instant::now();
        let timeout = Duration::from_secs(60 * 100_000);

        info!("Initial egraph size: {}", egraph.total_size());
        info!("Running {} DSRs... ", self.dsrs.len());

        let runner = Runner::<_, _, ()>::new(PartialLibCost::empty())
            .with_egraph(egraph)
            .with_time_limit(timeout)
            .with_iter_limit(3)
            .run(&self.dsrs);

        let aeg = runner.egraph;

        info!(
            "Finished in {}ms; final egraph size: {}",
            start_time.elapsed().as_millis(),
            aeg.total_size()
        );

        info!("Running co-occurrence analysis... ");
        let co_time = Instant::now();
        let co_ext = COBuilder::new(&aeg, roots);
        let co_occurs = co_ext.run();
        info!("Finished in {}ms", co_time.elapsed().as_millis());

        info!("Running anti-unification... ");
        let au_time = Instant::now();
        let mut learned_lib =
            LearnedLibrary::new(&aeg, self.learn_constants, self.max_arity, co_occurs);
        info!(
            "Found {} patterns in {}ms",
            learned_lib.size(),
            au_time.elapsed().as_millis()
        );

        info!("Deduplicating patterns... ");
        let dedup_time = Instant::now();
        learned_lib.deduplicate(&aeg);
        let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();
        info!(
            "Reduced to {} patterns in {}ms",
            learned_lib.size(),
            dedup_time.elapsed().as_millis()
        );

        info!("Adding libs and running beam search... ");
        let lib_rewrite_time = Instant::now();
        let runner = Runner::<_, _, ()>::new(PartialLibCost::new(
            self.final_beams,
            self.inter_beams,
            self.lps,
        ))
        .with_egraph(aeg.clone())
        .with_iter_limit(self.lib_iter_limit)
        .with_time_limit(timeout)
        .with_node_limit(1_000_000)
        .run(lib_rewrites.iter());

        let mut egraph = runner.egraph;
        let root = egraph.add(AstNode::new(Op::list(), roots.iter().copied()));
        let mut cs = egraph[egraph.find(root)].data.clone();
        cs.set.sort_unstable_by_key(|elem| elem.full_cost);

        info!("Finished in {}ms", lib_rewrite_time.elapsed().as_millis());
        info!("Stop reason: {:?}", runner.stop_reason.unwrap());
        info!("Number of nodes: {}", egraph.total_size());

        debug!("learned libs");
        let all_libs: Vec<_> = learned_lib.libs().collect();
        let mut chosen_rewrites = Vec::new();
        for lib in &cs.set[0].libs {
            debug!("{}: {}", lib.0, &all_libs[lib.0 .0]);
            chosen_rewrites.push(lib_rewrites[lib.0 .0].clone());
        }

        debug!("upper bound ('full') cost: {}", cs.set[0].full_cost);

        let ex_time = Instant::now();
        info!("Extracting... ");
        let lifted = apply_libs(aeg.clone(), roots, &chosen_rewrites);
        let final_cost = AstSize.cost_rec(&lifted);

        info!("Finished in {}ms", ex_time.elapsed().as_millis());
        info!("final cost: {}", final_cost);
        debug!("{}", Pretty(&Expr::from(lifted.clone())));
        info!("round time: {}ms", start_time.elapsed().as_millis());

        ExperimentResult {
            final_expr: lifted.into(),
            num_libs: chosen_rewrites.len(),
            rewrites: chosen_rewrites,
        }
    }
}

impl<Op, Extra> Experiment<Op> for BeamExperiment<Op, Extra>
where
    Op: Teachable
        + Printable
        + Arity
        + Clone
        + Send
        + Sync
        + Debug
        + Display
        + Hash
        + Ord
        + 'static,
    Extra: Serialize + Debug + Clone,
{
    /// The list of domain-specific rewrites used in this experiment.
    fn dsrs(&self) -> &[Rewrite<AstNode<Op>, PartialLibCost>] {
        &self.dsrs
    }

    fn run(&self, exprs: Vec<Expr<Op>>, _writer: &mut CsvWriter) -> ExperimentResult<Op> {
        // First, let's turn our list of exprs into a list of recexprs
        let recexprs: Vec<RecExpr<AstNode<Op>>> = exprs.into_iter().map_into().collect();

        // Add one to account for root node, not added yet
        let initial_cost = {
            let s: usize = recexprs.iter().map(|x| AstSize.cost_rec(x)).sum();
            s + 1
        };

        debug!("Starting cost: {}", initial_cost);

        let mut egraph = EGraph::new(PartialLibCost::new(
            self.final_beams,
            self.inter_beams,
            self.lps,
        ));
        let roots = recexprs
            .iter()
            .map(|x| egraph.add_expr(x))
            .collect::<Vec<_>>();
        egraph.rebuild();

        self.run_egraph(&roots, egraph)
    }

    fn total_rounds(&self) -> usize {
        1
    }

    fn run_multi(&self, expr_groups: Vec<Vec<Expr<Op>>>) -> ExperimentResult<Op> {
        // First, let's turn our list of exprs into a list of recexprs
        let recexpr_groups: Vec<Vec<_>> = expr_groups
            .into_iter()
            .map(|group| group.into_iter().map(RecExpr::from).collect())
            .collect();

        let mut egraph = EGraph::new(PartialLibCost::new(
            self.final_beams,
            self.inter_beams,
            self.lps,
        ));

        let roots: Vec<_> = recexpr_groups
            .into_iter()
            .map(|mut group| {
                let first_expr = group.pop().unwrap();
                let root = egraph.add_expr(&first_expr);
                for expr in group {
                    let class = egraph.add_expr(&expr);
                    egraph.union(root, class);
                }

                root
            })
            .collect();

        egraph.rebuild();

        self.run_egraph(&roots, egraph)
    }

    fn write_to_csv(
        &self,
        writer: &mut CsvWriter,
        round: usize,
        initial_cost: usize,
        final_cost: usize,
        compression: f64,
        num_libs: usize,
        time_elapsed: Duration,
    ) {
        writer
            .serialize((
                "beam",
                0,
                self.final_beams,
                self.inter_beams,
                self.lps,
                self.extra_data.clone(),
                round,
                initial_cost,
                final_cost,
                compression,
                num_libs,
                time_elapsed.as_secs_f64(),
            ))
            .unwrap();
        writer.flush().unwrap();
    }

    fn fmt_title(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self {
            final_beams,
            inter_beams,
            lps,
            extra_data,
            ..
        } = self;
        write!(
            f,
            "beam | final_beams: {final_beams}, inter_beams: {inter_beams}, lps: {lps:?}, extra_data: {extra_data:?}",
        )
    }
}
