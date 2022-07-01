pub use self::beam_experiment::BeamExperiment;
pub use self::ilp_experiment::IlpExperiment;
pub use self::eqsat_experiment::EqsatExperiment;

use crate::{
    ast_node::{Arity, AstNode, Expr, Pretty, Printable},
    extract::{apply_libs, beam::PartialLibCost},
    teachable::Teachable,
};
use egg::{EGraph, Id, RecExpr, Rewrite, Runner};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    io,
    marker::PhantomData,
    time::{Duration, Instant},
};

mod beam_experiment;
mod eqsat_experiment;
pub mod cache;
mod ilp_experiment;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Summary<Op> {
    pub initial_expr_groups: Vec<Vec<Expr<Op>>>,
    pub initial_cost: usize,
    pub final_expr: Expr<Op>,
    pub final_cost: usize,
    pub run_time: Duration,
}

impl<Op> Summary<Op> {
    /// The [compression ratio][wiki] achieved on this example.
    ///
    /// [wiki]: https://en.wikipedia.org/wiki/Data_compression_ratio
    pub fn compression_ratio(&self) -> f64 {
        (self.initial_cost as f64) / (self.final_cost as f64)
    }

    /// The [space saving][wiki] achieved on this example, reported as a number
    /// between 0 and 100.
    ///
    /// [wiki]: https://en.wikipedia.org/wiki/Data_compression_ratio
    pub fn space_saving_percentage(&self) -> f64 {
        let space_saving = 1.0 - ((self.final_cost as f64) / (self.initial_cost as f64));
        space_saving * 100.0
    }

    /// Returns the reduction in cost between `old` and `self` as a percentage
    /// of the initial cost.
    pub fn percent_improved(&self, old: &Self) -> f64 {
        assert_eq!(self.initial_cost, old.initial_cost);
        let improvement = (old.final_cost as f64) - (self.final_cost as f64);
        let relative_improvement = improvement / (self.initial_cost as f64);
        relative_improvement * 100.0
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

/// Output of library learning.
pub struct ExperimentResult<Op: Printable + Teachable + Hash + Clone + Debug + Arity + Ord> {
    pub final_expr: Expr<Op>,
    pub num_libs: usize,
    pub rewrites: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
}

pub type CsvWriter = csv::Writer<Box<dyn io::Write>>;

/// Library learning experiment.
pub trait Experiment<Op>
where
    Op: Printable + Teachable + Hash + Clone + Debug + Arity + Ord,
{
    /// The list of domain-specific rewrites used in this experiment.
    fn dsrs(&self) -> &[Rewrite<AstNode<Op>, PartialLibCost>];

    // Ideally exprs would have type `I: IntoIterator<Item = Expr<Op>>` but that's not object-safe.
    // This function also gets a writer method to write out intermediate results to the csv.
    fn run(&self, exprs: Vec<Expr<Op>>, writer: &mut CsvWriter) -> ExperimentResult<Op>;

    fn run_multi(&self, expr_groups: Vec<Vec<Expr<Op>>>) -> ExperimentResult<Op>;

    fn run_multi_summary(&self, expr_groups: Vec<Vec<Expr<Op>>>) -> Summary<Op> {
        let start_time = Instant::now();

        let initial_expr_groups = expr_groups.clone();
        let initial_cost: usize = initial_expr_groups
            .iter()
            .map(|group| group.iter().map(|expr| expr.len()).min().unwrap())
            .sum();
        let initial_cost = initial_cost + 1;

        let final_expr = self.run_multi(expr_groups).final_expr;
        let final_cost = final_expr.len();

        Summary {
            initial_expr_groups,
            initial_cost,
            final_expr,
            final_cost,
            run_time: start_time.elapsed(),
        }
    }

    /// Write experiments result to CSV.
    fn write_to_csv(
        &self,
        writer: &mut CsvWriter,
        round: usize,
        initial_cost: usize,
        final_cost: usize,
        compression: f64,
        num_libs: usize,
        time_elapsed: Duration,
    );

    fn fmt_title(&self, f: &mut Formatter<'_>) -> fmt::Result;

    fn total_rounds(&self) -> usize;

    /// Run experiment and write results to CSV.
    fn run_csv(&self, exprs: Vec<Expr<Op>>, writer: &mut CsvWriter) {
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
        let res = self.run(exprs, writer);

        let final_cost = res.final_expr.len();
        let compression = initial_cost as f64 / final_cost as f64;
        let time_elapsed = start_time.elapsed();

        // Print our analysis on this
        println!("Final beam results");
        println!("{}", Pretty(&res.final_expr));
        println!(
            "cost diff: {} -> {} (compression ratio {})",
            initial_cost, final_cost, compression
        );
        // println!("learned rewrites: {:?}", res.rewrites);
        println!("total time: {}ms", time_elapsed.as_millis());
        println!();

        self.write_to_csv(
            writer,
            self.total_rounds(),
            initial_cost,
            final_cost,
            compression,
            res.num_libs,
            time_elapsed,
        );
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
        + Debug
        + Display
        + Hash
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
        test_exprs: Vec<Expr<Op>>,
        dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
        mut beams: Vec<usize>,
        mut lpss: Vec<usize>,
        rounds: usize,
        mut extra_pors: Vec<bool>,
        timeouts: Vec<u64>,
        extra: Extra,
        learn_constants: bool,
        max_arity: Option<usize>,
    ) -> Self
    where
        Extra: serde::ser::Serialize + Clone + Debug + Clone + 'static,
    {
        let mut res: Vec<Box<dyn Experiment<Op>>> = Vec::new();

        // Defaults for if we have empty values
        if beams.is_empty() {
            // TODO: be more graceful about this
            panic!("error: beams not specified");
        }

        if lpss.is_empty() {
            // TODO: be more graceful about this
            panic!("error: lps not specified");
        }

        if extra_pors.is_empty() {
            extra_pors.push(false);
        }

        for beam in beams {
            for &extra_por in &extra_pors {
                for &lps in &lpss {
                    if lps > beam {
                        // TODO: be more graceful about this too
                        panic!("lps {} greater than beam {}", lps, beam);
                    }

                    let beam_experiment = BeamExperiment::new(
                        dsrs.clone(),
                        beam,
                        beam,
                        lps,
                        extra_por,
                        extra.clone(),
                        learn_constants,
                        max_arity,
                    );
                    let te = test_exprs.clone();
                    if !te.is_empty() {
                        res.push(Box::new(Generalization::new(beam_experiment, te, rounds)));
                    } else {
                        // We always use Rounds so that we unconditionally run our
                        // plumbing infra, in the case of e.g. nested libs
                        res.push(Box::new(Rounds::new(rounds, beam_experiment)));
                    }
                }
            }
        }

        for timeout in timeouts {
            let ilp_experiment =
                IlpExperiment::new(dsrs.clone(), timeout, extra.clone(), learn_constants);

            res.push(Box::new(Rounds::new(rounds, ilp_experiment)));
        }

        Self {
            exprs,
            experiments: res,
        }
    }

    /// Runs all experiments in this set
    pub fn run(self, csv_path: &str) {
        let file = std::fs::File::create(csv_path).unwrap();
        let mut writer: CsvWriter = csv::Writer::from_writer(Box::new(file));

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
    pub(crate) fn libs<Op>(llr: LLRes<'_, Op>) -> HashMap<LibId, Vec<AstNode<Op>>>
    where
        Op: Teachable + Clone + std::hash::Hash + Ord + std::fmt::Debug,
    {
        // For our strategy, we start at the root and walk downwards.
        // While we see libs, add its body to the hashmap and keep moving
        // If we don't see a lib, that means we've hit the non-libs section,
        // so we give up on the spot.
        let mut res = HashMap::new();

        fn walk<Op>(from: LLRes<'_, Op>, res: &mut HashMap<LibId, Vec<AstNode<Op>>>, ix: Id)
        where
            Op: Teachable + Clone + std::hash::Hash + Ord + std::fmt::Debug,
        {
            // Check what kind of node we're at.
            match &from[usize::from(ix)].as_binding_expr() {
                Some(crate::teachable::BindingExpr::Lib(lid, defn, b)) => {
                    // Extract recursive expression
                    let rc = build_recexpr(**defn, |x| {
                        // When building up the RecExpr for our lib defn, we need to do some
                        // special processing to make sure that we're accounting for the case
                        // where we have nested lib defns, e.g.
                        //
                        // ```
                        // lib l119 =
                        //   lib l48 =
                        //     λx5 x6 -> map (λx7 -> x5 x6 x7) x6
                        //   in
                        //     l48 (λx5 x6 -> l192 x6)
                        // in
                        //   (body)
                        // ```
                        match &from[usize::from(x)].as_binding_expr() {
                            Some(crate::teachable::BindingExpr::Lib(_n_lid, _n_defn, n_b)) => {
                                // We have a nested lib!
                                // Process the lib itself by walking thru this node
                                walk(from, res, x);

                                // Then return the node given by n_b, the body of the nested lib.
                                from[usize::from(**n_b)].clone()
                            }
                            _ => from[usize::from(x)].clone(),
                        }
                    });

                    // Push to res
                    res.insert(LibId(lid.0), rc.as_ref().to_vec());
                    // Recursively walk in body
                    walk(from, res, **b);
                }
                _ => {} // no-op
            }
        }

        // Walk starting from root
        walk(llr, &mut res, Id::from(llr.len() - 1));

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

    /// A riff off Language::build_recexpr, which also uses the get_node arg fn
    /// on the root index passed.
    fn build_recexpr<F, L>(root: Id, mut get_node: F) -> RecExpr<L>
    where
        L: Language,
        F: FnMut(Id) -> L,
    {
        let mut set = indexmap::IndexSet::<L>::default();
        let mut ids = HashMap::<Id, Id>::default();
        let mut todo = vec![root];

        while let Some(id) = todo.last().copied() {
            if ids.contains_key(&id) {
                todo.pop();
                continue;
            }

            let node = get_node(id);

            // check to see if we can do this node yet
            let mut ids_has_all_children = true;
            for child in node.children() {
                if !ids.contains_key(child) {
                    ids_has_all_children = false;
                    todo.push(*child)
                }
            }

            // all children are processed, so we can lookup this node safely
            if ids_has_all_children {
                let node = node.map_children(|id| ids[&id]);
                let new_id = set.insert_full(node).0;
                ids.insert(id, Id::from(new_id));
                todo.pop();
            }
        }

        // finally, add the root node and create the expression
        let mut nodes: Vec<L> = set.into_iter().collect();
        RecExpr::from(nodes)
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
    /// The list of domain-specific rewrites used in this experiment.
    fn dsrs(&self) -> &[Rewrite<AstNode<Op>, PartialLibCost>] {
        self.experiment.dsrs()
    }

    fn run(&self, exprs: Vec<Expr<Op>>, writer: &mut CsvWriter) -> ExperimentResult<Op> {
        let initial_cost = exprs.iter().map(|expr| expr.len()).sum::<usize>() + 1;
        let start = std::time::Instant::now();

        let mut current_exprs = exprs;
        let mut rc: RecExpr<AstNode<Op>>;
        let mut libs = HashMap::new();
        let mut current_rewrites = Vec::new();

        for round in 0..self.rounds {
            print!("round {}/{}", round + 1, self.rounds);
            let round_res = self.experiment.run(current_exprs, writer);

            rc = round_res.final_expr.into();

            let ls = plumbing::libs(rc.as_ref());
            libs.extend(ls);
            current_exprs = plumbing::exprs(rc.as_ref());
            current_rewrites.extend(round_res.rewrites);

            // We record intermediate results if we're not at the last round yet
            if round != self.rounds - 1 {
                let inter_expr = plumbing::combine(libs.clone(), current_exprs.clone());
                let inter_cost = inter_expr.len();
                let compression = initial_cost as f64 / inter_cost as f64;

                self.write_to_csv(
                    writer,
                    round,
                    initial_cost,
                    inter_cost,
                    compression,
                    libs.len(),
                    start.elapsed(),
                );

                println!(
                    " results: {}/{} (r {})",
                    inter_cost, initial_cost, compression
                );

                log::debug!("{}", Pretty(&inter_expr));
            } else {
                println!(" finished!");
            }
        }

        let ll = libs.len();

        // Combine back into one big recexpr at the end
        ExperimentResult {
            final_expr: plumbing::combine(libs, current_exprs),
            num_libs: ll,
            rewrites: current_rewrites,
        }
    }

    fn run_multi(&self, expr_groups: Vec<Vec<Expr<Op>>>) -> ExperimentResult<Op> {
        // Hack: just ignore any written info.
        let mut writer = CsvWriter::from_writer(Box::new(io::sink()));

        let initial_cost = expr_groups
            .iter()
            .map(|expr_group| expr_group.iter().map(|expr| expr.len()).min().unwrap())
            .sum::<usize>()
            + 1;
        let start = std::time::Instant::now();

        let first_res = self.experiment.run_multi(expr_groups);

        let mut rc: RecExpr<AstNode<Op>> = first_res.final_expr.into();

        let mut current_exprs = plumbing::exprs(rc.as_ref());
        let mut libs = plumbing::libs(rc.as_ref());
        let mut current_rewrites = first_res.rewrites;

        for round in 1..self.rounds {
            print!("round {}/{}", round + 1, self.rounds);
            let round_res = self.experiment.run(current_exprs, &mut writer);

            rc = round_res.final_expr.into();

            let ls = plumbing::libs(rc.as_ref());
            libs.extend(ls);
            current_exprs = plumbing::exprs(rc.as_ref());
            current_rewrites.extend(round_res.rewrites);

            // We record intermediate results if we're not at the last round yet
            if round != self.rounds - 1 {
                let inter_expr = plumbing::combine(libs.clone(), current_exprs.clone());
                let inter_cost = inter_expr.len();
                let compression = initial_cost as f64 / inter_cost as f64;

                self.write_to_csv(
                    &mut writer,
                    round,
                    initial_cost,
                    inter_cost,
                    compression,
                    libs.len(),
                    start.elapsed(),
                );

                println!(
                    " results: {}/{} (r {})",
                    inter_cost, initial_cost, compression
                );

                log::debug!("{}", Pretty(&inter_expr));
            } else {
                println!(" finished!");
            }
        }

        let ll = libs.len();

        // Combine back into one big recexpr at the end
        ExperimentResult {
            final_expr: plumbing::combine(libs, current_exprs),
            num_libs: ll,
            rewrites: current_rewrites,
        }
    }

    fn fmt_title(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.experiment.fmt_title(f)
    }

    fn total_rounds(&self) -> usize {
        self.rounds
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
        self.experiment.write_to_csv(
            writer,
            round,
            initial_cost,
            final_cost,
            compression,
            num_libs,
            time_elapsed,
        )
    }
}

/// Generalization experiment, which applies learned libraries on a test set.
#[derive(Debug)]
pub struct Generalization<Op, T: Experiment<Op>>
where
    Op: Printable + Teachable + Hash + Clone + Debug + Arity + Ord,
{
    experiment: T,
    test_set: Vec<Expr<Op>>,
    /// I tried to make this compose with Rounds, but it's very difficult
    /// because iteratively learned rewrites have to be applied also iteratively.
    /// So I had to mash the two together.
    rounds: usize,
    phantom: PhantomData<Op>,
}

impl<Op, T: Experiment<Op>> Generalization<Op, T>
where
    Op: Printable
        + Teachable
        + Hash
        + Clone
        + Debug
        + Arity
        + Ord
        + Display
        + Send
        + Sync
        + 'static,
{
    pub fn new(experiment: T, test_set: Vec<Expr<Op>>, rounds: usize) -> Self {
        Self {
            experiment,
            test_set,
            rounds,
            phantom: PhantomData,
        }
    }

    /// Create an egraph out of `exprs` rewritten with my DSRs.
    fn to_egraph(&self, exprs: Vec<Expr<Op>>) -> (EGraph<AstNode<Op>, PartialLibCost>, Vec<Id>) {
        let recexprs: Vec<RecExpr<AstNode<Op>>> =
            exprs.clone().into_iter().map(|x| x.into()).collect();
        let mut aeg = EGraph::new(PartialLibCost::empty());
        let roots = recexprs.iter().map(|x| aeg.add_expr(x)).collect::<Vec<_>>();
        aeg.rebuild();
        let runner = Runner::<_, _, ()>::new(PartialLibCost::empty())
            .with_egraph(aeg)
            .run(self.dsrs());
        (runner.egraph, roots)
    }
}

impl<Op, T: Experiment<Op>> Experiment<Op> for Generalization<Op, T>
where
    Op: Printable
        + Teachable
        + Hash
        + Clone
        + Debug
        + Arity
        + Ord
        + Display
        + Send
        + Sync
        + 'static,
{
    /// The list of domain-specific rewrites used in this experiment.
    fn dsrs(&self) -> &[Rewrite<AstNode<Op>, PartialLibCost>] {
        self.experiment.dsrs()
    }

    fn run(&self, exprs: Vec<Expr<Op>>, writer: &mut CsvWriter) -> ExperimentResult<Op> {
        let mut current_train_exprs = exprs;
        let mut current_test_exprs = self.test_set.clone();
        let mut rc: RecExpr<AstNode<Op>>;
        let mut libs = HashMap::new();
        let mut test_libs = HashMap::new(); // can be subset of the libs
        let mut current_rewrites = Vec::new();

        for round in 0..self.rounds {
            println!("round {}/{}", round + 1, self.rounds);

            let round_res = self.experiment.run(current_train_exprs, writer);

            rc = round_res.final_expr.into();
            libs.extend(plumbing::libs(rc.as_ref()));
            current_train_exprs = plumbing::exprs(rc.as_ref());

            let (aeg, roots) = self.to_egraph(current_test_exprs.clone());
            rc = apply_libs(aeg, &roots, &round_res.rewrites);
            test_libs.extend(plumbing::libs(rc.as_ref()));
            current_test_exprs = plumbing::exprs(rc.as_ref());

            current_rewrites.extend(round_res.rewrites);
        }

        let ll = test_libs.len();

        ExperimentResult {
            final_expr: plumbing::combine(test_libs, current_test_exprs),
            num_libs: ll,
            rewrites: current_rewrites,
        }
    }

    fn fmt_title(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.experiment.fmt_title(f)
    }

    fn total_rounds(&self) -> usize {
        1
    }

    fn write_to_csv(
        &self,
        writer: &mut CsvWriter,
        rounds: usize,
        initial_cost: usize,
        final_cost: usize,
        compression: f64,
        num_libs: usize,
        time_elapsed: Duration,
    ) {
        self.experiment.write_to_csv(
            writer,
            rounds,
            initial_cost,
            final_cost,
            compression,
            num_libs,
            time_elapsed,
        );
    }

    /// Run experiment and write results to CSV.
    fn run_csv(&self, exprs: Vec<Expr<Op>>, writer: &mut CsvWriter) {
        println!(
            "{}",
            ExperimentTitle {
                experiment: self,
                phantom: PhantomData
            }
        );

        let start_time = Instant::now();

        // Add one to account for root node, not added yet
        let initial_cost = self.test_set.iter().map(|expr| expr.len()).sum::<usize>() + 1;
        let res = self.run(exprs, writer);

        let final_cost = res.final_expr.len();
        let compression = initial_cost as f64 / final_cost as f64;
        let time_elapsed = start_time.elapsed();

        // Print our analysis on this
        println!("Final beam results");
        println!("{}", Pretty(&res.final_expr));
        println!(
            "cost diff: {} -> {} (compression ratio {})",
            initial_cost, final_cost, compression
        );
        // println!("learned rewrites: {:?}", res.rewrites);
        println!("total time: {}ms", time_elapsed.as_millis());
        println!();

        self.write_to_csv(
            writer,
            self.total_rounds(),
            initial_cost,
            final_cost,
            compression,
            res.num_libs,
            time_elapsed,
        );
    }

    fn run_multi(&self, expr_groups: Vec<Vec<Expr<Op>>>) -> ExperimentResult<Op> {
        unimplemented!()
    }
}
