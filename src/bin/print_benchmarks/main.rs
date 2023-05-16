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

use babble::{
    ast_node::{Expr, Pretty},
    dreamcoder::{expr::DreamCoderOp, json::CompressionInput},
};
use clap::Parser;
use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
};

#[derive(Parser)]
#[clap(version, author, about)]
struct Opts {
    #[clap(long)]
    domain: Option<String>,
}

const BENCHMARK_PATH: &str = "harness/data/dreamcoder-benchmarks/benches";

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let opts: Opts = Opts::parse();

    let mut benchmark_dirs = Vec::new();
    for entry in fs::read_dir(BENCHMARK_PATH)? {
        let path = entry?.path();
        if fs::metadata(&path)?.is_dir() {
            benchmark_dirs.push(path);
        }
    }

    benchmark_dirs.sort_unstable();

    let mut domains: BTreeMap<_, Vec<_>> = BTreeMap::new();

    for benchmark_dir in &benchmark_dirs {
        let dir_name = benchmark_dir.file_name().unwrap().to_str().unwrap();
        let (domain, _benchmark_name) = dir_name.split_once('_').unwrap();
        domains
            .entry(domain)
            .or_default()
            .push(benchmark_dir.as_path());
    }

    if let Some(domain) = &opts.domain {
        print_domain(domain, &domains[domain.as_str()])?;
    } else {
        for (domain, benchmarks) in domains {
            print_domain(domain, &benchmarks)?;
        }
    }

    Ok(())
}

fn print_domain(domain: &str, benchmarks: &[&Path]) -> anyhow::Result<()>
where
{
    println!("domain: {domain}");

    let mut tasks: BTreeMap<String, BTreeSet<Expr<DreamCoderOp>>> = BTreeMap::new();

    for benchmark_path in benchmarks {
        let mut inputs = Vec::new();

        for entry in fs::read_dir(benchmark_path)? {
            let path = entry?.path();
            if fs::metadata(&path)?.is_file() {
                inputs.push(path);
            }
        }

        for input in &inputs {
            let input = fs::read_to_string(input)?;
            let input: CompressionInput = serde_json::from_str(&input)?;

            for frontier in input.frontiers {
                let task = frontier.task.unwrap_or_else(|| "<unnamed>".to_string());
                let programs = tasks.entry(task).or_default();

                programs.extend(
                    frontier
                        .programs
                        .into_iter()
                        .map(|program| program.program.into()),
                );
            }
        }
    }

    for (task, exprs) in tasks {
        println!();
        println!("task: {task}");
        for expr in &exprs {
            println!("{}", Pretty(expr));
        }
    }

    Ok(())
}
