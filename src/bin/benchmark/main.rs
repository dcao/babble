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
    dreamcoder::{
        expr::{DreamCoderOp, DcExpr},
        json::{CompressionInput, Frontier},
    },
    runner::Experiments, extract::beam::PartialLibCost,
};
use clap::Clap;
use egg::{AstSize, CostFunction, Language, RecExpr, EGraph};
// use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    collections::HashSet,
    fs::{self, DirEntry},
    io::{self, Read},
    path::{Path, PathBuf},
};

#[allow(clippy::struct_excessive_bools)]
#[derive(Clap)]
#[clap(version, author, about)]
struct Opts {
    /// The input directory. If none is specified, defaults to `"data/dreamcoder-benchmarks/benches"`.
    #[clap(parse(from_os_str))]
    file: Option<PathBuf>,
}

const BENCHMARK_PATH: &str = "data/dreamcoder-benchmarks/benches";

fn main() -> io::Result<()> {
    env_logger::init();
    let opts: Opts = Opts::parse();

    let benchmark_path = opts.file.unwrap_or(PathBuf::from(BENCHMARK_PATH));

    let mut benchmark_dirs = Vec::new();
    for entry in fs::read_dir(&benchmark_path)? {
        let path = entry?.path();
        if fs::metadata(&path)?.is_dir() {
            benchmark_dirs.push(path);
        }
    }

    benchmark_dirs.sort_unstable();

    for benchmark_dir in &benchmark_dirs {
        println!(
            "benchmark: {}",
            benchmark_dir.file_name().unwrap().to_str().unwrap()
        );
        let mut inputs = Vec::new();

        for entry in fs::read_dir(benchmark_dir)? {
            let path = entry?.path();
            if fs::metadata(&path)?.is_file() {
                inputs.push(path);
            }
        }

        inputs.sort_unstable();

        for input in &inputs {
            let mut programs = Vec::new();

            println!("file: {}", input.file_name().unwrap().to_str().unwrap());

            let input = fs::read_to_string(input)?;
            let input: CompressionInput = serde_json::from_str(&input)?;

            for frontier in input.frontiers {
                for program in frontier.programs {
                    programs.push(program.program.into());
                }
            }
            println!("{} programs", programs.len());

            let experiments = Experiments::gen(programs, vec![], vec![], vec![], vec![], vec![], vec![], (), true);
            experiments.run("/dev/null");

        }
    }

    println!("Running benchmarks");

    // let mut benchmark_dirs = benchmark_dirs.co

    // for entry in benchmark_dirs {
    //     let entry = entry?;
    //     println!("{}", entry.file_name().to_string_lossy());

    //     let mut total_programs = 0;
    //     let mut programs = HashSet::new();
    //     if fs::metadata(entry.path())?.is_dir() {
    //         for entry in fs::read_dir(entry.path())? {
    //             let entry = entry?;

    //             if entry.file_name().to_str() != Some("out") {
    //                 let input = fs::read_to_string(entry.path())?;
    //                 // println!("{}", entry.file_name().to_string_lossy());

    //                 let input: CompressionInput = serde_json::from_str(&input)?;
    //                 // println!("{} frontiers", input.frontiers.len());

    //                 for frontier in input.frontiers {
    //                     total_programs += frontier.programs.len();
    //                     programs.extend(frontier.programs.into_iter().map(|p| p.program));
    //                     if let Some(task) = frontier.task {
    //                         println!("{}", task);
    //                     }
    //                 }

    //                 // println!("{} programs", programs.len());
    //             }
    //         }
    //     }

    //     println!("{} programs ({} unique)", total_programs, programs.len());
    // }

    Ok(())

    // let input = opts
    //     .file
    //     .as_ref()
    //     .map_or_else(
    //         || {
    //             let mut buf = String::new();
    //             io::stdin().read_to_string(&mut buf).map(|_| buf)
    //         },
    //         fs::read_to_string,
    //     )
    //     .expect("Error reading input");

    // let input: CompressionInput = serde_json::from_str(&input).expect("Error parsing JSON input");

    // let mut all = Experiments::new();

    // for limit in &opts.limit {
    //     let exprs: Vec<Expr<DreamCoderOp>> = input
    //         .clone()
    //         .frontiers
    //         .into_iter()
    //         .flat_map(|frontier| frontier.programs)
    //         .map(|program| program.program.into())
    //         .take(*limit)
    //         .collect();

    //     let mut res = Vec::new();
    //     let mut roots: Vec<egg::Id> = Vec::new();

    //     for expr in exprs {
    //         // Turn the expr into a RecExpr
    //         let recx: RecExpr<_> = expr.into();

    //         // Then turn the RecExpr into a Vec
    //         let mut vecx: Vec<AstNode<DreamCoderOp>> = recx.as_ref().to_vec();

    //         // For each node, increment the children by the current size of the accum expr
    //         for node in vecx.iter_mut() {
    //             node.update_children(|x| (usize::from(x) + res.len()).into());
    //         }

    //         // Then push everything into the accum expr
    //         res.extend(vecx);
    //         roots.push((res.len() - 1).into());
    //     }

    //     // Add the root node
    //     res.push(AstNode::new(DreamCoderOp::Combine, roots));

    //     // Turn res back into a recexpr!
    //     let initial_expr: RecExpr<_> = res.into();

    //     println!(
    //         "Initial expression (limit {}, cost {}):",
    //         limit,
    //         AstSize.cost_rec(&initial_expr)
    //     );
    //     println!("{}", Pretty(&initial_expr.clone().into()));
    //     println!();

    //     let exps = Experiments::gen(
    //         initial_expr,
    //         vec![], // TODO
    //         opts.beams.clone(),
    //         opts.extra_por.clone(),
    //         opts.timeout.clone(),
    //         limit,
    //     );

    //     all.add(exps);
    // }

    // println!("running...");
    // all.run("target/res_compression.csv");
}
