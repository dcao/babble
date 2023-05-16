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
    ast_node::{combine_exprs, Expr, Pretty},
    dreamcoder::{expr::DreamCoderOp, json::CompressionInput},
    experiments::Experiments,
};
use clap::Parser;
use egg::{AstSize, CostFunction, RecExpr};
// use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
};

#[allow(clippy::struct_excessive_bools)]
#[derive(Parser)]
#[clap(version, author, about)]
struct Opts {
    /// The input file. If no file is specified, reads from stdin.
    #[clap(parse(from_os_str))]
    file: Option<PathBuf>,

    /// Enables pretty-printing of JSON output.
    #[clap(long)]
    pretty: bool,

    /// Whether to learn "library functions" with no arguments.
    #[clap(long)]
    learn_constants: bool,

    /// Maximum arity of functions to learn.
    #[clap(long)]
    max_arity: Option<usize>,

    /// Do not use domain-specific rewrites
    #[clap(long)]
    no_dsr: bool,

    /// The number of programs to anti-unify
    #[clap(long)]
    limit: Vec<usize>,

    /// The beam sizes to use for the beam extractor
    #[clap(long)]
    beams: Vec<usize>,

    /// The number of libs to learn at a time
    #[clap(long)]
    lps: Vec<usize>,

    /// The number of rounds of lib learning to run
    #[clap(long, default_value_t = 1)]
    rounds: usize,

    /// The timeouts to use for the ILP extractor
    #[clap(long)]
    timeout: Vec<u64>,

    /// Whether to use the additional partial order reduction step
    #[clap(long)]
    extra_por: Vec<bool>,
}

fn main() {
    env_logger::init();
    let opts: Opts = Opts::parse();

    let input = opts
        .file
        .as_ref()
        .map_or_else(
            || {
                let mut buf = String::new();
                io::stdin().read_to_string(&mut buf).map(|_| buf)
            },
            fs::read_to_string,
        )
        .expect("Error reading input");

    let input: CompressionInput = serde_json::from_str(&input).expect("Error parsing JSON input");

    let mut all = Experiments::new();

    for &limit in &opts.limit {
        let exprs: Vec<Expr<DreamCoderOp>> = input
            .clone()
            .frontiers
            .into_iter()
            .flat_map(|frontier| frontier.programs)
            .map(|program| program.program.into())
            .take(limit)
            .collect();

        // For the sake of pretty printing
        {
            let initial_expr: RecExpr<_> = combine_exprs(exprs.clone());
            let initial_cost = AstSize.cost_rec(&initial_expr);

            println!("Initial expression (cost {initial_cost}, limit {limit}):");
            println!("{}", Pretty(&Expr::from(initial_expr.clone())));
            println!();
        }

        let dsrs = if opts.no_dsr {
            vec![]
        } else {
            vec![
                egg::rewrite!("add commute"; "(@ (@ + ?x) ?y)" => "(@ (@ + ?y) ?x)"),
                egg::rewrite!("add assoc"; "(@ (@ + (@ (@ + ?x) ?y)) ?z)" => "(@ (@ + ?x) (@ (@ + ?y) ?z))"),
                egg::rewrite!("len range"; "(@ length (@ range ?x))" => "?x"),
            ]
        };

        let exps = Experiments::gen(
            exprs,
            vec![],
            dsrs,
            opts.beams.clone(),
            opts.lps.clone(),
            opts.rounds,
            opts.extra_por.clone(),
            opts.timeout.clone(),
            limit,
            opts.learn_constants,
            opts.max_arity,
        );

        all.add(exps);
    }

    println!("running...");
    all.run("harness/data_gen/res_compression.csv");
}
