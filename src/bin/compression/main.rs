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

use babble::{extract::LpExtractor, learn::LearnedLibrary};
use clap::Clap;
use dreamcoder::{expr::Expr, json::CompressionInput};
use egg::{AstSize, EGraph, Runner};
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
    time::{Duration, Instant},
};

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
    let start_time = Instant::now();
    let timeout = Duration::from_secs(60 * 10);
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
    let limit = opts.limit.unwrap_or(usize::MAX);

    let mut egraph = EGraph::new(());
    let mut roots = vec![];

    for frontier in &input.frontiers {
        if roots.len() > limit {
            break;
        }

        for program in &frontier.programs {
            if roots.len() > limit {
                break;
            }

            let expr = &program.program;
            let root = egraph.add_expr(&expr.into());
            roots.push(root);
        }
    }

    egraph.rebuild();

    println!("Compressing {} programs", roots.len());

    let learned_lib = LearnedLibrary::from(&egraph);
    let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();

    println!("Found {} antiunifications", lib_rewrites.len());

    println!("Anti-unifying");
    let runner = Runner::default()
        .with_egraph(egraph)
        .with_iter_limit(1)
        .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
        .with_node_limit(100_000)
        .run(lib_rewrites.iter());

    println!("Stop reason: {:?}", runner.stop_reason.unwrap());

    let egraph = runner.egraph;
    println!("Number of nodes: {}", egraph.total_size());

    println!("Extracting");

    let (exprs, ids) = LpExtractor::new(&egraph, AstSize)
        .timeout(timeout.saturating_sub(start_time.elapsed()).as_secs_f64())
        .solve_multiple(&roots);
    println!("Solutions:");
    for id in ids {
        let expr = Expr {
            context: exprs.clone(),
            index: id,
        };
        println!("{}", expr);
        println!();
    }
}
