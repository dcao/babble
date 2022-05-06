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
    dreamcoder::{expr::DreamCoderOp, json::CompressionInput},
    runner::Experiments,
};
use clap::Clap;
use egg::{AstSize, CostFunction, Language, RecExpr};
// use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
};

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

    /// The number of programs to anti-unify
    #[clap(long)]
    limit: Vec<usize>,

    /// The beam sizes to use for the beam extractor
    #[clap(long)]
    beams: Vec<usize>,

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

    for limit in &opts.limit {
        let exprs: Vec<Expr<DreamCoderOp>> = input
            .clone()
            .frontiers
            .into_iter()
            .flat_map(|frontier| frontier.programs)
            .map(|program| program.program.into())
            .take(*limit)
            .collect();

        let mut res = Vec::new();
        let mut roots: Vec<egg::Id> = Vec::new();

        for expr in exprs {
            // Turn the expr into a RecExpr
            let recx: RecExpr<_> = expr.into();

            // Then turn the RecExpr into a Vec
            let mut vecx: Vec<AstNode<DreamCoderOp>> = recx.as_ref().to_vec();

            // For each node, increment the children by the current size of the accum expr
            for node in vecx.iter_mut() {
                node.update_children(|x| (usize::from(x) + res.len()).into());
            }

            // Then push everything into the accum expr
            res.extend(vecx);
            roots.push((res.len() - 1).into());
        }

        // Add the root node
        res.push(AstNode::new(DreamCoderOp::Combine, roots));

        // Turn res back into a recexpr!
        let initial_expr: RecExpr<_> = res.into();

        println!(
            "Initial expression (limit {}, cost {}):",
            limit,
            AstSize.cost_rec(&initial_expr)
        );
        println!("{}", Pretty(&initial_expr.clone().into()));
        println!();

        let exps = Experiments::gen(
            initial_expr,
            vec![], // TODO
            opts.beams.clone(),
            opts.extra_por.clone(),
            opts.timeout.clone(),
            limit,
        );

        all.add(exps);
    }

    println!("running...");
    all.run("target/res_compression.csv");
}
