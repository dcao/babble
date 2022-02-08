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
    ast_node::{AstNode, Expr},
    extract::{beam::PartialLibCost, LpExtractor},
    learn::LearnedLibrary,
};
use clap::Clap;
use dreamcoder::{expr::DcExpr, json::CompressionInput};
use egg::{AstSize, EGraph, Language, RecExpr, Runner};
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
    time::{Duration, Instant},
};

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

    // For benching purposes: ignore the limit option and just rerun with multiple different possibilities
    for limit in [802] {
        for final_beams in (10..=50).step_by(10) {
            for inter_beams in (10..=50).step_by(10) {
                // let inter_beams = final_beams;
                // let final_beams = inter_beams;
                println!("limit: {}, final_beams: {}, inter_beams: {}", limit, final_beams, inter_beams);

                let start_time = Instant::now();
                let timeout = Duration::from_secs(60 * 100000);

                let mut egraph = EGraph::new(PartialLibCost::new(final_beams, inter_beams));
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
                    let root = egraph.add_expr(&expr);
                    roots.push(root);
                }

                egraph.rebuild();

                println!("Compressing {} programs", roots.len());
                println!("Starting cost: {}", initial_cost);

                let learned_lib = LearnedLibrary::from(&egraph);
                let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();

                println!("Found {} antiunifications", lib_rewrites.len());

                println!("Anti-unifying");
                let runner = Runner::<_, _, ()>::new(PartialLibCost::new(final_beams, inter_beams))
                    .with_egraph(egraph)
                    .with_iter_limit(1)
                    .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
                    .with_node_limit(100_000)
                    .run(lib_rewrites.iter());

                println!("Stop reason: {:?}", runner.stop_reason.unwrap());

                let mut egraph = runner.egraph;
                println!("Number of nodes: {}", egraph.total_size());

                // Add the root combine node.
                let root = egraph.add(AstNode::new(DreamCoderOp::Combine, roots.iter().copied()));

                let mut cs = egraph[egraph.find(root)].data.clone();
                cs.set.sort_unstable_by_key(|elem| elem.full_cost);

                println!("learned libs");
                for lib in &cs.set[0].libs {
                    println!("{}", egraph[egraph.find(lib.0)].nodes[0].build_recexpr(|id| egraph[egraph.find(id)].nodes[0].clone()).pretty(100));
                }

                println!("full cost: {}", cs.set.iter().min_by_key(|ls| ls.full_cost).unwrap().full_cost);
                println!();

                wtr.serialize((limit, final_beams, inter_beams, cs.set[0].full_cost, start_time.elapsed().as_secs_f64())).unwrap();
                wtr.flush().unwrap();
            }
        }
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
