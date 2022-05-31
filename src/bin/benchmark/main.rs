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
use std::{fs, io, path::PathBuf};

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

            println!("  file: {}", input.file_name().unwrap().to_str().unwrap());

            let input = fs::read_to_string(input)?;
            let input: CompressionInput = serde_json::from_str(&input)?;

            for frontier in input.frontiers {
                for program in frontier.programs {
                    programs.push(program.program.into());
                }
            }

            let experiment =
                BeamExperiment::new([], 25, 25, LibsPerSel::Unlimited, 1, false, (), true);
            let summary = experiment.run_summary(programs);
            println!("    {} programs", summary.num_exprs);
            println!("    initial cost: {}", summary.initial_cost);
            println!(
                "    final cost: {} (compression ratio: {:.4}, a {:.2}% reduction)",
                summary.final_cost,
                summary.compression_ratio(),
                summary.percent_reduction()
            );
            println!("    took {}ms", summary.duration.as_millis());
        }
    }

    Ok(())
}
