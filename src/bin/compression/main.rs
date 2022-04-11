#![warn(
    clippy::all,
    clippy::pedantic,
    anonymous_parameters,
    elided_lifetimes_in_paths,
    missing_copy_implementations,
    missing_debug_implementations,
    single_use_lifetimes,
    trivial_casts,
    unreachable_pub,
    unused_lifetimes
)]
#![allow(clippy::non_ascii_literal)]

use babble::{
    ast_node::{AstNode, Expr, Pretty},
    extract::{
        beam::{LibExtractor, PartialLibCost},
        lift_libs, true_cost,
    },
    learn::LearnedLibrary,
};
use clap::Clap;
use dreamcoder::json::CompressionInput;
use egg::{AstSize, CostFunction, EGraph, RecExpr, Runner};
use log::debug;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
    time::{Duration, Instant},
};

#[cfg(feature = "grb")]
use babble::extract::ilp::*;

use crate::dreamcoder::expr::DreamCoderOp;

pub mod dreamcoder;

#[allow(clippy::struct_excessive_bools)]
#[derive(Clap)]
#[clap(version, author, about)]
struct Opts {
    /// The input file. If no file is specified, reads from stdin.
    #[clap(parse(from_os_str))]
    file: Option<PathBuf>,

    /// Enables pretty-printing of JSON output.
    #[clap(long)]
    pretty: bool,

    /// The maximum number of programs to anti-unify.
    #[clap(long)]
    limit: Option<usize>,
}

fn main() {
    env_logger::init();
    let opts: Opts = Opts::parse();

    let input = opts
        .file
        .map_or_else(
            || {
                let mut buf = String::new();
                io::stdin().read_to_string(&mut buf).map(|_| buf)
            },
            fs::read_to_string,
        )
        .expect("Error reading input");

    let input: CompressionInput = serde_json::from_str(&input).expect("Error parsing JSON input");
    // res: Vec<(limit, final beam size, inter beam size, smallest full cost, time)>
    let mut wtr = csv::Writer::from_path("target/res.csv").unwrap();

    let run_beam_exp = |limit, final_beams, inter_beams, wtr: &mut csv::Writer<fs::File>| {
        // let inter_beams = final_beams;
        // let final_beams = inter_beams;
        if final_beams > inter_beams {
            return;
        }

        println!(
            "limit: {}, final_beams: {}, inter_beams: {}",
            limit, final_beams, inter_beams
        );

        let start_time = Instant::now();
        let timeout = Duration::from_secs(60 * 100000);

        let mut aeg = EGraph::new(PartialLibCost::new(final_beams, inter_beams));
        let programs: Vec<Expr<DreamCoderOp>> = input
            .clone()
            .frontiers
            .into_iter()
            .flat_map(|frontier| frontier.programs)
            .map(|program| program.program.into())
            .take(limit)
            .collect();
        let mut roots = Vec::with_capacity(programs.len());
        // Add 1 to initial cost to account for the top-level `Compose` node.
        let initial_cost = programs.iter().map(Expr::len).sum::<usize>() + 1;
        for expr in programs.iter().cloned().map(RecExpr::from) {
            let root = aeg.add_expr(&expr);
            roots.push(root);
        }

        aeg.rebuild();

        let runner = Runner::<_, _, ()>::new(PartialLibCost::new(final_beams, inter_beams))
            .with_egraph(aeg)
            .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
            .run(&[
                egg::rewrite!("add comm"; "(@ (@ + ?x) ?y)" => "(@ (@ + ?y) ?x)"),
                // egg::rewrite!("len range"; "(@ length (@ range ?x))" => "?x"),
            ]);

        let aeg = runner.egraph;

        println!("Compressing {} programs", roots.len());
        println!("Starting cost: {}", initial_cost);

        let learned_lib = LearnedLibrary::from(&aeg);
        let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();

        println!("Found {} antiunifications", lib_rewrites.len());

        println!("Anti-unifying");
        let runner = Runner::<_, _, ()>::new(PartialLibCost::new(final_beams, inter_beams))
            .with_egraph(aeg.clone())
            .with_iter_limit(1)
            .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
            .with_node_limit(1_000_000)
            .run(lib_rewrites.iter());

        println!("Stop reason: {:?}", runner.stop_reason.unwrap());

        let mut egraph = runner.egraph;
        println!("Number of nodes: {}", egraph.total_size());

        // Add the root combine node.
        let root = egraph.add(AstNode::new(DreamCoderOp::Combine, roots.iter().copied()));

        let mut cs = egraph[egraph.find(root)].data.clone();
        cs.set.sort_unstable_by_key(|elem| elem.full_cost);

        debug!("learned libs");
        let all_libs: Vec<_> = learned_lib.libs().collect();
        for lib in &cs.set[0].libs {
            debug!("{}: {}", lib.0, &all_libs[lib.0 .0]);
        }

        println!("upper bound ('full') cost: {}", cs.set[0].full_cost);
        println!();

        println!("extracting (final, lifted libs)");
        let (lifted, final_cost) = cs
            .set
            // .par_iter()
            .iter()
            .take(1)
            .map(|ls| {
                // Add the root combine node again
                let mut fin = Runner::<_, _, ()>::new(PartialLibCost::new(0, 0))
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
                let root = fin.add(AstNode::new(DreamCoderOp::Combine, roots.iter().copied()));

                // print out the fin egraph:
                // for eclass in fin.classes() {
                //     debug!("{}:\n{:?}\n{:?}\n", eclass.id, eclass.nodes, eclass.data);
                // }

                // let best = less_dumb_extractor(&fin, root);
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

        // println!("{}", lifted.pretty(100));
        println!("{}", Pretty(&Expr::from(lifted)));
        println!("final cost: {}", final_cost);
        println!();

        wtr.serialize((
            limit,
            "beam",
            timeout.as_secs(),
            final_beams,
            inter_beams,
            initial_cost,
            final_cost,
            start_time.elapsed().as_secs_f64(),
        ))
        .unwrap();
        wtr.flush().unwrap();
    };

    #[cfg(feature = "grb")]
    let run_ilp_exp = |limit, timeout, wtr: &mut csv::Writer<fs::File>| {
        println!("limit: {} [ILP]", limit);

        let start_time = Instant::now();
        let timeout = Duration::from_secs(timeout);

        let mut aeg = EGraph::new(());
        let programs: Vec<Expr<DreamCoderOp>> = input
            .clone()
            .frontiers
            .into_iter()
            .flat_map(|frontier| frontier.programs)
            .map(|program| program.program.into())
            .take(limit)
            .collect();
        let mut roots = Vec::with_capacity(programs.len());
        let initial_cost: usize = programs.iter().map(Expr::len).sum();
        for expr in programs.iter().cloned().map(RecExpr::from) {
            let root = aeg.add_expr(&expr);
            roots.push(root);
        }

        aeg.rebuild();

        let runner = Runner::<_, _, ()>::new(())
            .with_egraph(aeg)
            .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
            .run(&[
                egg::rewrite!("add comm"; "(@ (@ + ?x) ?y)" => "(@ (@ + ?y) ?x)"),
                // egg::rewrite!("len range"; "(@ length (@ range ?x))" => "?x"),
            ]);

        let aeg = runner.egraph;

        println!("Compressing {} programs", roots.len());
        println!("Starting cost: {}", initial_cost);

        let learned_lib = LearnedLibrary::from(&aeg);
        let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();

        println!("Found {} antiunifications", lib_rewrites.len());

        println!("Anti-unifying");
        let runner = Runner::<_, _, ()>::new(())
            .with_egraph(aeg.clone())
            .with_iter_limit(1)
            .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
            .with_node_limit(1_000_000)
            .run(lib_rewrites.iter());

        println!("Stop reason: {:?}", runner.stop_reason.unwrap());

        let mut egraph = runner.egraph;
        println!("Number of nodes: {}", egraph.total_size());

        // Add the root combine node.
        let root = egraph.add(AstNode::new(DreamCoderOp::Combine, roots.iter().copied()));

        // println!("extracting (ILP, before lifting)");
        let best = LpExtractor::new(&egraph, egg::AstSize)
            .timeout(timeout.saturating_sub(start_time.elapsed()).as_secs_f64())
            .solve(root);
        // println!("{}", best.pretty(100));
        // println!();

        println!("extracting (final, lifted libs)");
        let lifted = lift_libs(best);
        let final_cost = true_cost(lifted.clone()) - 1;
        // println!("{}", lifted.pretty(100));
        println!("{}", Pretty(&Expr::from(lifted)));
        println!("final cost: {}", final_cost);
        println!();

        wtr.serialize((
            limit,
            "ilp",
            timeout.as_secs(),
            0,
            0,
            initial_cost,
            final_cost,
            start_time.elapsed().as_secs_f64(),
        ))
        .unwrap();
        wtr.flush().unwrap();
    };

    #[cfg(not(feature = "grb"))]
    let run_ilp_exp = |_limit: usize, _timeout: usize, _wtr: &mut csv::Writer<fs::File>| {
        // no-op
    };

    // For benching purposes: ignore the limit option and just rerun with multiple different possibilities
    for limit in [20] {
        // for final_beams in (10..=50).step_by(10) {
        //     for inter_beams in (100..=1000).step_by(100) {
        //         run_beam_exp(limit, final_beams, inter_beams, &mut wtr);
        //     }
        // }

        for beam_size in [25] {
            // [25, 50, 100, 200] {
            run_beam_exp(limit, beam_size, beam_size, &mut wtr);
        }

        for timeout in [10] {
            run_ilp_exp(limit, timeout, &mut wtr);
        }

        // run_beam_exp(limit, 30, 100, &mut wtr);
        // run_ilp_exp(limit, 10, &mut wtr);
    }

    // --- old code below

    // let limit = opts.limit.unwrap_or(usize::MAX);

    // let mut egraph = EGraph::new(PartialLibCost::new(20));
    // let programs: Vec<Expr<DreamCoderOp>> = input
    //     .frontiers
    //     .into_iter()
    //     .flat_map(|frontier| frontier.programs)
    //     .map(|program| program.program.into())
    //     .take(limit)
    //     .collect();
    // let mut roots = Vec::with_capacity(programs.len());
    // let initial_cost: usize = programs.iter().map(Expr::len).sum();
    // for expr in programs.iter().cloned().map(RecExpr::from) {
    //     let root = egraph.add_expr(&expr);
    //     roots.push(root);
    // }

    // egraph.rebuild();

    // println!("Compressing {} programs", roots.len());
    // println!("Starting cost: {}", initial_cost);

    // let learned_lib = LearnedLibrary::from(&egraph);
    // let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();

    // println!("Found {} antiunifications", lib_rewrites.len());

    // println!("Anti-unifying");
    // let runner = Runner::<_, _, ()>::new(PartialLibCost::new(20))
    //     .with_egraph(egraph)
    //     .with_iter_limit(1)
    //     .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
    //     .with_node_limit(100_000)
    //     .run(lib_rewrites.iter());

    // println!("Stop reason: {:?}", runner.stop_reason.unwrap());

    // let mut egraph = runner.egraph;
    // println!("Number of nodes: {}", egraph.total_size());

    // // Add the root combine node.
    // let root = egraph.add(AstNode::new(DreamCoderOp::Combine, roots.iter().copied()));

    // println!("root analysis data:");
    // let mut cs = egraph[egraph.find(root)].data.clone();
    // cs.set.sort_unstable_by_key(|elem| elem.full_cost);

    // println!("learned libs");
    // for lib in &cs.set[0].libs {
    //     println!("{}", DcExpr::from(Expr::from(egraph[egraph.find(lib.0)].nodes[0].build_recexpr(|id| egraph[egraph.find(id)].nodes[0].clone()))));
    // }

    // println!("cost {}", cs.set[0].full_cost);

    // // println!("Extracting");

    // // let (exprs, ids) = LpExtractor::new(&egraph, AstSize)
    // //     .timeout(timeout.saturating_sub(start_time.elapsed()).as_secs_f64())
    // //     .solve_multiple(&roots);
    // // let final_exprs: Vec<Expr<_>> = ids
    // //     .into_iter()
    // //     .map(|id| RecExpr::from(exprs.as_ref()[..=usize::from(id)].to_vec()).into())
    // //     .collect();
    // // let final_cost: usize = final_exprs.iter().map(Expr::len).sum();
    // // println!("Final cost: {}", final_cost);
    // // println!("Solutions:");
    // // for expr in final_exprs {
    // //     println!("{}", DcExpr::from(expr));
    // //     println!();
    // // }
}
