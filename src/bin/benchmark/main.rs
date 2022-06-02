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
    dreamcoder::json::CompressionInput,
    experiments::{BeamExperiment, Experiment},
    extract::beam::LibsPerSel,
};
use clap::Clap;
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    fs,
    io::{self, Write},
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
}

const BENCHMARK_PATH: &str = "data/dreamcoder-benchmarks/benches";
const DSR_PATH: &str = "data/benchmark-dsrs";

#[derive(Debug)]
struct Benchmark<'a> {
    name: &'a str,
    path: &'a Path,
}

#[derive(Debug, Serialize, Deserialize)]
struct DsrResults<'a> {
    domain: &'a str,
    benchmark: &'a str,
    file: &'a str,
    initial_cost: usize,
    final_cost_dsrs: usize,
    final_cost_no_dsrs: usize,
    millis_dsrs: u128,
    millis_no_dsrs: u128,
}

mod rewrites {
    use egg::{Analysis, FromOp, Language, Pattern, Rewrite};

    pub(crate) fn parse_dsrs<L: Language + FromOp + Sync + Send + 'static, A: Analysis<L>>(
        file: &str,
    ) -> Vec<Rewrite<L, A>> {
        let mut rewrites = Vec::new();
        for line in file
            .lines()
            .map(|line| line.trim())
            .filter(|line| !line.is_empty())
        {
            let (name, rewrite) = line.split_once(':').unwrap();
            let (lhs, rhs) = rewrite.split_once("=>").unwrap();
            let name = name.trim();
            let lhs = lhs.trim();
            let rhs = rhs.trim();
            let lhs: Pattern<L> = lhs.parse().unwrap();
            let rhs: Pattern<L> = rhs.parse().unwrap();
            rewrites.push(Rewrite::new(name, lhs, rhs).unwrap());
        }
        rewrites
    }
}

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

    let mut csv_writer = csv::WriterBuilder::new().from_path("benchmarks.csv")?;

    if let Some(domain) = &opts.domain {
        run_domain(domain, &domains[domain.as_str()], &mut csv_writer)?;
    } else {
        for (domain, benchmarks) in domains {
            run_domain(domain, &benchmarks, &mut csv_writer)?;
        }
    }

    csv_writer.flush()
}

fn run_domain<'a, I, W: Write>(
    domain: &'a str,
    benchmarks: I,
    csv_writer: &'a mut csv::Writer<W>,
) -> io::Result<()>
where
    I: IntoIterator<Item = &'a Benchmark<'a>>,
{
    println!("domain: {}", domain);

    let dsr_file = PathBuf::from(DSR_PATH).join(format!("{}.rewrites", domain));
    let rewrites = if dsr_file.exists() {
        let contents = fs::read_to_string(dsr_file)?;
        rewrites::parse_dsrs(&contents)
    } else {
        Vec::new()
    };

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
            let file_name = input.file_name().unwrap().to_str().unwrap();
            println!("    file: {}", file_name);

            let input = fs::read_to_string(input)?;
            let input: CompressionInput = serde_json::from_str(&input)?;

            let mut programs = Vec::new();
            for frontier in input.frontiers {
                for program in frontier.programs {
                    programs.push(program.program.into());
                }
            }

            let experiment_dsrs = BeamExperiment::new(
                rewrites.clone(),
                25,
                25,
                LibsPerSel::Unlimited,
                1,
                false,
                (),
                true,
            );
            let experiment_no_dsrs =
                BeamExperiment::new([], 25, 25, LibsPerSel::Unlimited, 1, false, (), true);

            let summary_dsrs = experiment_dsrs.run_summary(programs.clone());
            let summary_no_dsrs = experiment_no_dsrs.run_summary(programs);

            csv_writer.serialize(DsrResults {
                domain,
                benchmark: benchmark.name,
                file: file_name,
                initial_cost: summary_dsrs.initial_cost,
                final_cost_dsrs: summary_dsrs.final_cost,
                final_cost_no_dsrs: summary_no_dsrs.final_cost,
                millis_dsrs: summary_dsrs.duration.as_millis(),
                millis_no_dsrs: summary_no_dsrs.duration.as_millis(),
            })?;
        }
    }

    Ok(())
}
