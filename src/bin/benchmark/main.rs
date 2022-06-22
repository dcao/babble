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
    dreamcoder::{expr::DreamCoderOp, json::CompressionInput},
    experiments::{cache::ExperimentCache, BeamExperiment, Experiment, Summary},
    rewrites,
};
use clap::Clap;
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

#[allow(clippy::struct_excessive_bools)]
#[derive(Clap)]
#[clap(version, author, about)]
struct Opts {
    /// The input directory. If none is specified, defaults to `"data/dreamcoder-benchmarks/benches"`.
    #[clap(parse(from_os_str))]
    file: Option<PathBuf>,

    #[clap(long)]
    domain: Option<String>,

    #[clap(long)]
    benchmark: Option<String>,

    #[clap(long)]
    use_cache: bool,
}

const BENCHMARK_PATH: &str = "data/dreamcoder-benchmarks/benches";
const DSR_PATH: &str = "data/benchmark-dsrs";
const CACHE_DIR: &str = "cache";
const BEAM_SIZE: usize = 100;
const LPS: usize = 20;

#[derive(Debug)]
struct Benchmark<'a> {
    name: &'a str,
    path: &'a Path,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct DsrResults {
    domain: String,
    benchmark: String,
    file: String,
    summary_no_dsrs: Summary<DreamCoderOp>,
    summary_dsrs: Summary<DreamCoderOp>,
}

fn main() -> anyhow::Result<()> {
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

    let mut cache = ExperimentCache::new(&CACHE_DIR)?;

    if let Some(domain) = &opts.domain {
        run_domain(
            domain,
            &domains[domain.as_str()],
            &mut cache,
            opts.use_cache,
        )?;
    } else {
        for (domain, benchmarks) in domains {
            run_domain(domain, &benchmarks, &mut cache, opts.use_cache)?;
        }
    }

    Ok(())
}

fn run_domain<'a, I>(
    domain: &'a str,
    benchmarks: I,
    cache: &mut ExperimentCache<'_, DreamCoderOp>,
    use_cache: bool,
) -> anyhow::Result<()>
where
    I: IntoIterator<Item = &'a Benchmark<'a>>,
{
    let mut results = Vec::new();

    println!("domain: {}", domain);

    let dsr_file = PathBuf::from(DSR_PATH).join(format!("{}.rewrites", domain));
    let rewrites = rewrites::try_from_file(dsr_file)?.unwrap_or_default();

    println!("  found {} domain-specific rewrites", rewrites.len());

    for benchmark in benchmarks {
        println!("  benchmark: {}", benchmark.name);
        let mut inputs = Vec::new();

        for entry in fs::read_dir(benchmark.path)? {
            let path = entry?.path();
            if fs::metadata(&path)?.is_file() {
                inputs.push(path);
            }
        }

        inputs.sort_unstable();

        for input in &inputs {
            let file_name = input
                .file_name()
                .and_then(|f| f.to_str())
                .and_then(|f| f.strip_suffix(".json"))
                .unwrap();

            let experiment_id = format!("{}-{}-{}", &domain, &benchmark.name, &file_name);
            let experiment_dsrs_id = format!("{}-dsrs", experiment_id);
            let experiment_no_dsrs_id = format!("{}-no-dsrs", experiment_id);

            print!("    file: {}", file_name);

            if use_cache
                && cache.contains(&experiment_dsrs_id)
                && cache.contains(&experiment_no_dsrs_id)
            {
                println!(" [cached]")
            } else {
                println!()
            }

            let input = fs::read_to_string(input)?;
            let input: CompressionInput = serde_json::from_str(&input)?;

            let mut programs = Vec::new();
            for frontier in input.frontiers {
                // TODO: Put these in the same eclass
                for program in frontier.programs {
                    programs.push(program.program.into());
                }
            }

            let experiment_dsrs = BeamExperiment::new(
                rewrites.clone(),
                BEAM_SIZE,
                BEAM_SIZE,
                LPS,
                1,
                false,
                (),
                true,
                None,
            );

            let experiment_no_dsrs =
                BeamExperiment::new([], BEAM_SIZE, BEAM_SIZE, LPS, 1, false, (), true, None);

            let summary_dsrs = if use_cache {
                cache.get_or_insert_with(&experiment_dsrs_id, || {
                    experiment_dsrs.run_summary(programs.clone())
                })?
            } else {
                let summary = experiment_dsrs.run_summary(programs.clone());
                cache.insert(experiment_dsrs_id, &summary)?;
                summary
            };

            let summary_no_dsrs = if use_cache {
                cache.get_or_insert_with(&experiment_no_dsrs_id, || {
                    experiment_no_dsrs.run_summary(programs.clone())
                })?
            } else {
                let summary = experiment_no_dsrs.run_summary(programs.clone());
                cache.insert(experiment_no_dsrs_id, &summary)?;
                summary
            };

            let result = DsrResults {
                domain: domain.to_owned(),
                benchmark: benchmark.name.to_owned(),
                file: file_name.to_owned(),
                summary_no_dsrs,
                summary_dsrs,
            };

            results.push(result);
        }
    }

    plot_dsr_impact(results)?;

    Ok(())
}

fn plot_dsr_impact<I>(results: I) -> anyhow::Result<()>
where
    I: IntoIterator<Item = DsrResults>,
{
    let mut csv_writer = csv::Writer::from_path("plot/data/dsr-impact.csv")?;
    csv_writer.serialize(("benchmark", "percent improvement"))?;

    for (i, result) in results.into_iter().enumerate() {
        let percent_improved = result
            .summary_dsrs
            .percent_improved(&result.summary_no_dsrs);
        csv_writer.serialize((i, percent_improved))?;
    }

    csv_writer.flush()?;

    Ok(())
}
