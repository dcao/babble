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
    ast_node::Expr,
    extract::{beam::*, lift_libs},
    learn::LearnedLibrary,
    sexp::Sexp,
};
use clap::Clap;
use egg::{AstSize, CostFunction, EGraph, RecExpr, Runner};
use std::{
    convert::TryInto,
    fs,
    io::{self, Read},
    path::PathBuf,
};

use crate::pretty::Pretty;

pub mod lang;
pub mod pretty;

#[derive(Clap)]
#[clap(version, author, about)]
struct Opts {
    /// The input file. If no file is specified, reads from stdin.
    #[clap(parse(from_os_str))]
    file: Option<PathBuf>,
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

    let initial_expr: Expr<_> = Sexp::parse(&input)
        .expect("Failed to parse sexp")
        .try_into()
        .expect("Input is not a valid expression");
    let pretty_expr = Pretty(&initial_expr);
    let initial_expr: RecExpr<_> = initial_expr.clone().into();
    let initial_cost = AstSize.cost_rec(&initial_expr);

    println!("Initial expression (cost {}):", initial_cost);
    // println!("{}", initial_expr.pretty(100));
    println!("{}", pretty_expr);
    println!();

    println!("running...");

    println!("stage one");
    let mut aeg = EGraph::new(PartialLibCost::new(20, 100));
    let root = aeg.add_expr(&initial_expr);
    aeg.rebuild();

    println!("stage two");
    let learned_lib = LearnedLibrary::from(&aeg);
    let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();
    let egraph = Runner::<_, _, ()>::new(PartialLibCost::new(20, 100))
        .with_egraph(aeg.clone())
        .with_iter_limit(1)
        .run(lib_rewrites.iter())
        .egraph;
    println!();

    let mut cs = egraph[egraph.find(root)].data.clone();
    cs.set.sort_unstable_by_key(|elem| elem.full_cost);

    println!("learned libs");
    let all_libs: Vec<_> = learned_lib.libs().collect();
    for lib in &cs.set[0].libs {
        println!("{}: {}", lib.0, &all_libs[lib.0 .0]);
    }

    println!("upper bound ('full') cost: {}", cs.set[0].full_cost);
    println!();

    println!("extracting (with duplicate libs)");
    let fin = Runner::<_, _, ()>::new(PartialLibCost::new(20, 100))
        .with_egraph(aeg.clone())
        .with_iter_limit(1)
        .run(
            lib_rewrites
                .iter()
                .enumerate()
                .filter(|(i, _)| cs.set[0].libs.iter().any(|x| *i == x.0 .0))
                .map(|x| x.1),
        )
        .egraph;

    let best = less_dumb_extractor(&fin, root);
    println!("{}", best.pretty(100));
    println!();

    println!("extracting (final, lifted libs)");
    let lifted = lift_libs(best);
    println!("{}", lifted.pretty(100));
    println!("final cost: {}", lifted.as_ref().len());
    println!();

    // let runner = Runner::default()
    //     .with_egraph(egraph)
    //     .run(*lang::LIFT_LIB_REWRITES);
    // let stop_reason = runner.stop_reason.unwrap_or_else(|| unreachable!());
    // info!("Stop reason: {:?}", stop_reason);
    // info!("Number of iterations: {}", runner.iterations.len());

    // let egraph = runner.egraph;
    // info!("Number of nodes: {}", egraph.total_size());
    // let final_expr = LpExtractor::new(&egraph, AstSize).solve(root);
    // let final_cost = final_expr.as_ref().len();

    // println!("Final expression (cost {}):", final_cost);
    // // println!("{}", final_rexpr.pretty(100));
    // println!("{}", Pretty(&Expr::from(final_expr)));
    // println!();

    // #[allow(clippy::cast_precision_loss)]
    // let compression_ratio = (initial_cost as f64) / (final_cost as f64);
    // println!("Compression ratio: {:.2}", compression_ratio);
}
