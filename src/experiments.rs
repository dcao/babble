pub use self::beam_experiment::BeamExperiment;
pub use self::ilp_experiment::IlpExperiment;

use crate::{
    ast_node::{Arity, AstNode, Expr, Pretty, Printable},
    extract::beam::{LibsPerSel, PartialLibCost},
    teachable::Teachable,
};
use egg::{RecExpr, Rewrite};
use std::{
    collections::HashMap,
    fmt::{self, Debug, Display, Formatter},
    fs,
    hash::Hash,
    marker::PhantomData,
    time::{Duration, Instant},
};

mod beam_experiment;
mod ilp_experiment;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Summary {
    pub num_exprs: usize,
    pub initial_cost: usize,
    pub final_cost: usize,
    pub duration: Duration,
}

impl Summary {
    pub fn compression_ratio(&self) -> f64 {
        (self.final_cost as f64) / (self.initial_cost as f64)
    }

    pub fn percent_reduction(&self) -> f64 {
        100.0 * (1.0 - self.compression_ratio())
    }
}

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
    fn run(&self, exprs: Vec<Expr<Op>>) -> Expr<Op>;

    fn write_to_csv(
        &self,
        writer: &mut csv::Writer<fs::File>,
        initial_cost: usize,
        final_cost: usize,
        time_elapsed: Duration,
    );

    fn fmt_title(&self, f: &mut Formatter<'_>) -> fmt::Result;

    fn run_summary(&self, exprs: Vec<Expr<Op>>) -> Summary {
        let num_exprs = exprs.len();
        let initial_cost = exprs.iter().map(|expr| expr.len()).sum::<usize>() + 1;
        let start_time = Instant::now();
        let final_expr = self.run(exprs);
        let duration = start_time.elapsed();
        let final_cost = final_expr.len();
        Summary {
            num_exprs,
            initial_cost,
            final_cost,
            duration,
        }
    }

    fn run_csv(&self, exprs: Vec<Expr<Op>>, writer: &mut csv::Writer<fs::File>) {
        println!(
            "{}",
            ExperimentTitle {
                experiment: self,
                phantom: PhantomData
            }
        );

        let start_time = Instant::now();

        // Add one to account for root node, not added yet
        let initial_cost = exprs.iter().map(|expr| expr.len()).sum::<usize>() + 1;
        let final_expr = self.run(exprs);

        let final_cost = final_expr.len();
        let time_elapsed = start_time.elapsed();

        // Print our analysis on this
        println!("Final beam results");
        println!("{}", Pretty(&Expr::from(final_expr)));
        println!(
            "cost diff: {} -> {} (compression ratio {})",
            initial_cost,
            final_cost,
            final_cost as f64 / initial_cost as f64
        );
        println!("total time: {}ms", time_elapsed.as_millis());
        println!();

        self.write_to_csv(writer, initial_cost, final_cost, time_elapsed)
    }
}

/// A set of Experiments is just a list of individual Experiment structs
pub struct Experiments<Op> {
    experiments: Vec<Box<dyn Experiment<Op>>>,
    exprs: Vec<Expr<Op>>,
}

impl<Op: Debug> Debug for Experiments<Op> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Experiments")
            .field(
                "experiments",
                &format_args!("<{} experiments>", self.experiments.len()),
            )
            .field("exprs", &self.exprs)
            .finish()
    }
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
        self.exprs.extend(other.exprs);
    }

    // TODO: How to specify DSRs
    /// Generates a set of experiments from a set of params
    pub fn gen<Extra>(
        exprs: Vec<Expr<Op>>,
        dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
        mut beams: Vec<usize>,
        mut lpss: Vec<LibsPerSel>,
        mut rounds_list: Vec<usize>,
        mut extra_pors: Vec<bool>,
        timeouts: Vec<u64>,
        extra: Extra,
        learn_constants: bool,
    ) -> Self
    where
        Extra: serde::ser::Serialize + Clone + Debug + Clone + 'static,
    {
        let mut res: Vec<Box<dyn Experiment<Op>>> = Vec::new();

        // Defaults for if we have empty values
        if beams.is_empty() {
            beams.push(25);
        }

        if rounds_list.is_empty() {
            rounds_list.push(1);
        }

        if lpss.is_empty() {
            lpss.push(LibsPerSel::Unlimited);
        }

        if extra_pors.is_empty() {
            extra_pors.push(false);
        }

        for beam in beams {
            for &extra_por in &extra_pors {
                for &lps in &lpss {
                    for &rounds in &rounds_list {
                        let beam_experiment = BeamExperiment::new(
                            dsrs.clone(),
                            beam,
                            beam,
                            lps,
                            rounds,
                            extra_por,
                            extra.clone(),
                            learn_constants,
                        );
                        if rounds > 1 {
                            res.push(Box::new(Rounds::new(rounds, beam_experiment)));
                        } else {
                            res.push(Box::new(beam_experiment));
                        }
                    }
                }
            }
        }

        for timeout in timeouts {
            for &rounds in &rounds_list {
                let ilp_experiment = IlpExperiment::new(
                    dsrs.clone(),
                    timeout,
                    rounds,
                    extra.clone(),
                    learn_constants,
                );
                if rounds > 1 {
                    res.push(Box::new(Rounds::new(rounds, ilp_experiment)));
                } else {
                    res.push(Box::new(ilp_experiment));
                }
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
    ) -> Expr<Op>
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
        res.into()
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

#[derive(Debug)]
pub struct Rounds<Op, T: Experiment<Op>>
where
    Op: Printable + Teachable + Hash + Clone + Debug + Arity + Ord,
{
    rounds: usize,
    experiment: T,
    phantom: PhantomData<Op>,
}

impl<Op, T: Experiment<Op>> Rounds<Op, T>
where
    Op: Printable + Teachable + Hash + Clone + Debug + Arity + Ord,
{
    pub fn new(rounds: usize, experiment: T) -> Self {
        Self {
            rounds,
            experiment,
            phantom: PhantomData,
        }
    }
}

impl<Op, T: Experiment<Op>> Experiment<Op> for Rounds<Op, T>
where
    Op: Printable + Teachable + Hash + Clone + Debug + Arity + Ord,
{
    fn run(&self, exprs: Vec<Expr<Op>>) -> Expr<Op> {
        let mut current_exprs = exprs;
        let mut rc: RecExpr<AstNode<Op>>;
        let mut libs = HashMap::new();

        for round in 0..self.rounds {
            println!("round {}/{}", round + 1, self.rounds);

            rc = self.experiment.run(current_exprs).into();

            let ls = plumbing::libs(rc.as_ref(), libs.len());
            libs.extend(ls);
            current_exprs = plumbing::exprs(rc.as_ref());
        }

        // Combine back into one big recexpr at the end
        plumbing::combine(libs, current_exprs)
    }

    fn fmt_title(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.experiment.fmt_title(f)
    }

    fn write_to_csv(
        &self,
        writer: &mut csv::Writer<fs::File>,
        initial_cost: usize,
        final_cost: usize,
        time_elapsed: Duration,
    ) {
        self.experiment
            .write_to_csv(writer, initial_cost, final_cost, time_elapsed)
    }
}
