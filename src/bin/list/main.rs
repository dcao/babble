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

use babble::{ast_node::Expr, learn::LearnedLibrary, sexp::Sexp};
use clap::Clap;
use egg::{AstSize, CostFunction, EGraph, Extractor, RecExpr, Runner};
use log::info;
use std::{
    convert::TryInto,
    fs,
    io::{self, Read},
    path::PathBuf,
};

pub mod lang;

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
    let initial_expr: RecExpr<_> = initial_expr.into();
    let initial_cost = AstSize.cost_rec(&initial_expr);

    println!("Initial expression (cost {}):", initial_cost);
    println!("{}", initial_expr.pretty(100));
    println!();

    let mut egraph = EGraph::default();
    let root = egraph.add_expr(&initial_expr);
    egraph.rebuild();

    let learned_lib = LearnedLibrary::from(&egraph);
    let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();
    let egraph = Runner::default()
        .with_egraph(egraph)
        .with_iter_limit(1)
        .run(lib_rewrites.iter())
        .egraph;
    let runner = Runner::default()
        .with_egraph(egraph)
        .run(*lang::LIFT_LIB_REWRITES);
    let stop_reason = runner.stop_reason.unwrap_or_else(|| unreachable!());
    info!("Stop reason: {:?}", stop_reason);
    info!("Number of iterations: {}", runner.iterations.len());

    let egraph = runner.egraph;
    info!("Number of nodes: {}", egraph.total_size());
    let (final_cost, final_expr) = Extractor::new(&egraph, AstSize).find_best(root);

    println!("Final expression (cost {}):", final_cost);
    println!("{}", final_expr.pretty(100));
    println!();

    #[allow(clippy::cast_precision_loss)]
    let compression_ratio = (initial_cost as f64) / (final_cost as f64);
    println!("Compression ratio: {:.2}", compression_ratio);
}
