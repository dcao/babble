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
    dreamcoder::{expr::DreamCoderOp, json::CompressionInput},
    experiments::{
        cache::ExperimentCache, BeamExperiment, EqsatExperiment, Experiment, Rounds, Summary,
    },
    rewrites,
};
use clap::Parser;
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
    sync::Mutex,
};

use rayon::prelude::*;

#[allow(clippy::struct_excessive_bools)]
#[derive(Parser)]
#[clap(version, author, about)]
struct Opts {
    /// The input directory. If none is specified, defaults to `"harness/data/dreamcoder-benchmarks/benches"`.
    #[clap(parse(from_os_str))]
    file: Option<PathBuf>,

    #[clap(long)]
    domain: Option<String>,

    #[clap(long)]
    cache: Option<PathBuf>,

    /// File to dump the raw costs into
    #[clap(long, short)]
    output: PathBuf,

    #[clap(long)]
    beam_size: usize,
    #[clap(long)]
    lps: usize,
    #[clap(long)]
    rounds: usize,
    #[clap(long)]
    max_arity: usize,
    #[clap(long)]
    lib_iter_limit: usize,
    #[clap(long)] // should be bool, but I don't want flags
    use_all: usize,
    #[clap(long, value_parser = ["babble", "au", "eqsat"])]
    mode: String,
}

const BENCHMARK_PATH: &str = "harness/data/dreamcoder-benchmarks/benches";
const DSR_PATH: &str = "harness/data/benchmark-dsrs";

#[derive(Debug)]
struct Benchmark<'a> {
    name: &'a str,
    path: &'a Path,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Iteration {
    domain: String,
    benchmark: String,
    file: String,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Compression {
    initial_size: usize,
    final_size: usize,
    run_time: f32,
}

impl<'a, Op> From<&'a Summary<Op>> for Compression {
    fn from(summary: &'a Summary<Op>) -> Self {
        Self {
            initial_size: summary.initial_cost,
            final_size: summary.final_cost,
            run_time: summary.run_time.as_secs_f32(),
        }
    }
}

impl<'a, Op> From<&'a Option<Summary<Op>>> for Compression {
    fn from(summary: &'a Option<Summary<Op>>) -> Self {
        Self {
            initial_size: summary
                .as_ref()
                .map(|x| x.initial_cost)
                .unwrap_or_else(|| 1),
            final_size: summary.as_ref().map(|x| x.final_cost).unwrap_or_else(|| 1),
            run_time: summary
                .as_ref()
                .map(|x| x.run_time.as_secs_f32())
                .unwrap_or_else(|| 0.0),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct BenchResults {
    domain: String,
    benchmark: String,
    file: String,
    summary: Summary<DreamCoderOp>,
}

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let opts: Opts = Opts::parse();

    let cache = opts
        .cache
        .clone()
        .map_or_else(ExperimentCache::new, ExperimentCache::from_dir)?;

    println!("using cache: {}", cache.path().to_str().unwrap());

    let cache = Mutex::new(cache);

    let benchmark_path = opts.file.clone().unwrap_or(PathBuf::from(BENCHMARK_PATH));

    let mut benchmark_dirs = Vec::new();
    for entry in fs::read_dir(&benchmark_path)? {
        let path = entry?.path();
        if fs::metadata(&path)?.is_dir() {
            benchmark_dirs.push(path);
        }
    }

    benchmark_dirs.sort_unstable();

    let mut domains: BTreeMap<_, Vec<_>> = BTreeMap::new();

    for benchmark_dir in &benchmark_dirs {
        let dir_name = benchmark_dir.file_name().unwrap().to_str().unwrap();
        let (domain, benchmark_name) = dir_name.split_once('_').unwrap();
        domains.entry(domain).or_default().push(Benchmark {
            name: benchmark_name,
            path: benchmark_dir.as_path(),
        });
    }

    println!("domains:");
    for (domain, benchmarks) in &domains {
        println!("  {}: {} benchmark(s)", domain, benchmarks.len());
    }

    if let Some(domain) = &opts.domain {
        run_domain(domain, &opts, &domains[domain.as_str()], &cache)
    } else {
        for (domain, benchmarks) in domains {
            run_domain(domain, &opts, &benchmarks, &cache)
        }
    }

    Ok(())
}

fn run_domain(
    domain: &str,
    opts: &Opts,
    benchmarks: &[Benchmark<'_>],
    _cache: &Mutex<ExperimentCache<DreamCoderOp>>,
) {
    let results = Mutex::new(Vec::new());

    println!("domain: {}", domain);

    let dsr_file = PathBuf::from(DSR_PATH).join(format!("{}.rewrites", domain));
    let rewrites = rewrites::try_from_file(dsr_file)
        .unwrap()
        .unwrap_or_default();

    println!("  found {} domain-specific rewrites", rewrites.len());

    benchmarks.par_iter().for_each(|benchmark| {
        println!("  benchmark: {}", benchmark.name);
        let mut inputs = Vec::new();

        for entry in fs::read_dir(benchmark.path).unwrap() {
            let path = entry.unwrap().path();
            if fs::metadata(&path).unwrap().is_file() {
                inputs.push(path);
            }
        }

        inputs.sort();

        inputs.par_iter().for_each(|input| {
            let file = input.file_name().unwrap().to_str().unwrap();

            println!("    file: {}", file);

            let input = fs::read_to_string(input).unwrap();
            let input: CompressionInput = serde_json::from_str(&input).unwrap();

            let program_groups: Vec<Vec<Expr<_>>> = input
                .frontiers
                .iter()
                .cloned()
                .map(|frontier| -> Vec<Expr<DreamCoderOp>> {
                    let programs = frontier
                        .programs
                        .into_iter()
                        .map(|program| program.program.into());

                    if opts.use_all > 0 {
                        programs.collect()
                    } else {
                        programs.take(1).collect()
                    }
                })
                .collect();

            let summary = if opts.mode == "eqsat" {
                let experiment = Rounds::new(1, EqsatExperiment::new(rewrites.clone(), ()));
                experiment.run_multi_summary(program_groups)
            } else {
                let use_dsrs = match opts.mode.as_str() {
                    "babble" => true,
                    "au" => false,
                    m => panic!("bad mode: {}", m),
                };
                let experiment = Rounds::new(
                    opts.rounds,
                    BeamExperiment::new(
                        if use_dsrs { rewrites.clone() } else { vec![] },
                        opts.beam_size,
                        opts.beam_size,
                        opts.lps,
                        false,
                        (),
                        true,
                        Some(opts.max_arity),
                        opts.lib_iter_limit,
                    ),
                );
                experiment.run_multi_summary(program_groups)
            };

            let name = format!("{}_{}/{}", domain, benchmark.name, file);
            println!(
                "{:20}        {} -> {} (r {:.3}), with {:>3} libs in {:>8.3}s",
                name,
                summary.initial_cost,
                summary.final_cost,
                summary.compression_ratio(),
                summary.num_libs,
                summary.run_time.as_secs_f32(),
            );

            let bench_results = BenchResults {
                domain: domain.to_string(),
                benchmark: benchmark.name.to_string(),
                file: file.to_string(),
                summary,
            };

            let mut locked = results.lock().unwrap();
            locked.push(bench_results);
        });
    });

    let results = results.into_inner().unwrap();
    plot_raw_data(&results, opts).unwrap();
}

fn plot_raw_data(results: &[BenchResults], opts: &Opts) -> anyhow::Result<()> {
    let mut csv_writer = csv::Writer::from_path(&opts.output)?;
    csv_writer.serialize((
        "name",
        "iter",
        "initial cost",
        "final cost",
        "compression",
        "total time",
        "num libs",
    ))?;

    for result in results {
        csv_writer.serialize((
            format!("{}_{}", result.domain, result.benchmark),
            &result.file,
            result.summary.initial_cost,
            result.summary.final_cost,
            result.summary.initial_cost as f64 / result.summary.final_cost as f64,
            result.summary.run_time.as_secs_f32(),
            result.summary.num_libs,
        ))?;
    }

    csv_writer.flush()?;
    Ok(())
}
