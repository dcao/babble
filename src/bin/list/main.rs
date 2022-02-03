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
    extract::{partial::*, LpExtractor},
    learn::LearnedLibrary,
    sexp::Sexp,
};
use clap::Clap;
use egg::{AstSize, CostFunction, EGraph, Language, RecExpr, Runner};
use log::info;
use std::{
    convert::TryInto,
    fs,
    io::{self, Read},
    path::PathBuf,
};

use crate::pretty::Pretty;

pub mod lang;
pub mod pretty;

// TODO: make this more general for all langs
struct NoLibAstSize;

impl babble::extract::LpCostFunction<babble::ast_node::AstNode<lang::ListOp>, ()> for NoLibAstSize {
    fn node_cost(
        &mut self,
        _egraph: &EGraph<babble::ast_node::AstNode<lang::ListOp>, ()>,
        _eclass: egg::Id,
        enode: &babble::ast_node::AstNode<lang::ListOp>,
    ) -> f64 {
        match enode.operation() {
            lang::ListOp::Lib => 0.0,
            lang::ListOp::Lambda => 0.0,
            _ => 1.0,
        }
    }
}

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

    println!("stage one");
    let mut egraph = EGraph::new(PartialLibCost::new(20));
    let root = egraph.add_expr(&initial_expr);
    egraph.rebuild();

    println!("stage two");
    let learned_lib = LearnedLibrary::from(&egraph);
    let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();
    let egraph = Runner::<_, _, ()>::new(PartialLibCost::new(20))
        .with_egraph(egraph)
        .with_iter_limit(1)
        .run(lib_rewrites.iter())
        .egraph;

    // For debug purposes: print the analysis for the root node
    println!("root analysis data:");
    let cs = &egraph[egraph.find(root)].data;
    for (i, ls) in cs.set.iter().enumerate() {
        println!("lib selection {}", i);
        if i == 0 {
            println!("MOST OPTIMAL");
        }
        println!("libs:");
        for (l, _c) in &ls.libs {
            println!("new lib");
            for n in &egraph[*l].nodes {
                println!(
                    "{}",
                    n.build_recexpr(|id| egraph[id].nodes[0].clone())
                        .pretty(100)
                );
            }
        }
        println!("costs: {} {}", ls.expr_cost, ls.full_cost);
        println!()
    }

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
