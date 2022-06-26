use super::{Experiment, ExperimentResult};
use crate::{
    ast_node::{Arity, AstNode, Expr, Printable},
    extract::beam::PartialLibCost,
    teachable::Teachable,
};
use egg::{RecExpr, Rewrite};
use serde::ser::Serialize;
use std::{
    fmt::{self, Debug, Display, Formatter},
    fs::File,
    hash::Hash,
    time::Duration,
};

/// An ILPExperiment contains all of the info needed to run a library
/// learning experiment with the ILP extractor.
#[derive(Debug)]
pub struct IlpExperiment<Op, Extra>
where
    Op: Display + Hash + Clone + Ord + 'static,
{
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

impl<Op, Extra> IlpExperiment<Op, Extra>
where
    Op: Display + Hash + Clone + Ord + 'static,
{
    pub fn new<I>(
        dsrs: I,
        timeout: u64,
        rounds: usize,
        extra_data: Extra,
        learn_constants: bool,
    ) -> Self
    where
        I: IntoIterator<Item = Rewrite<AstNode<Op>, PartialLibCost>>,
    {
        Self {
            dsrs: dsrs.into_iter().collect(),
            timeout,
            rounds,
            extra_data,
            learn_constants,
        }
    }
}

impl<Op, Extra> Experiment<Op> for IlpExperiment<Op, Extra>
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

    #[cfg(not(feature = "grb"))]
    fn run(&self, exprs: Vec<Expr<Op>>) -> ExperimentResult<Op> {
        unimplemented!("feature `grb` not enabled");
    }

    #[cfg(feature = "grb")]
    fn run(&self, exprs: Vec<Expr<Op>>) -> ExperimentResult<Op> {
        use crate::{
            ast_node::Pretty,
            extract::{ilp::LpExtractor, lift_libs},
            learn::LearnedLibrary,
        };
        use egg::{AstSize, CostFunction, EGraph, Runner};
        use std::time::Instant;

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

        return ExperimentResult {
            final_expr: lifted.into(),
            rewrites: vec![],
        };
    }

    fn write_to_csv(
        &self,
        writer: &mut csv::Writer<File>,
        initial_cost: usize,
        final_cost: usize,
        compression: f64,
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
                compression,
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
