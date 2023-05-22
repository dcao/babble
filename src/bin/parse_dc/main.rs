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
#![allow(clippy::non_ascii_literal, clippy::cast_precision_loss)]

use babble::{
    ast_node::{AstNode, Expr},
    dreamcoder::{
        expr::DreamCoderOp,
        json::{CompressionInput, CompressionOutput, CompressionSummary},
    },
};
use clap::Parser;
use egg::RecExpr;
use std::{
    collections::{HashMap, HashSet},
    fs::{self, File},
    path::{Path, PathBuf},
};

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
    benchmark: Option<String>,

    #[clap(long)]
    cache: Option<PathBuf>,
}
const BENCHMARK_PATH: &str = "harness/data/dreamcoder-benchmarks/benches";

/// A cost calculation for Dream&shy;Coder exprs, with a few modifications.
/// In particular, when we have `Inlined` expressions (i.e. learned libs),
/// we want to count their full cost, but only the first time they're seen,
/// and only if the lib is newly learned (this is because when babble runs
/// on inputs with Inlined exprs, it just treats them as exprs of size 1).
fn calc_cost(
    expr: &RecExpr<AstNode<DreamCoderOp>>,
    new_libs: &HashSet<Expr<DreamCoderOp>>,
) -> (HashMap<Expr<DreamCoderOp>, usize>, usize) {
    let mut lib_costs = HashMap::new();

    let expr = expr.as_ref();
    let mut res = 0;

    // Start at the root.
    let mut q = vec![expr.len() - 1];

    while let Some(id) = q.pop() {
        let id = id;
        if let DreamCoderOp::Inlined(e) = &expr[id].operation() {
            if new_libs.contains(e) {
                let re = RecExpr::from(*e.clone());

                // Get the size of the lib
                let (libs, lib_size) = calc_cost(&re, new_libs);
                lib_costs.insert(*e.clone(), lib_size);

                // Combine our lib costs with this one
                lib_costs.extend(libs);
            }
        }

        res += 1;

        expr[id].iter().for_each(|x| q.push(usize::from(*x)));
    }

    (lib_costs, res)
}

fn main() -> anyhow::Result<()> {
    let opts: Opts = Opts::parse();

    let benchmark_path = opts.file.unwrap_or(PathBuf::from(BENCHMARK_PATH));

    println!("using bench path: {benchmark_path:?}");

    let mut benchmark_dirs = Vec::new();
    for entry in fs::read_dir(&benchmark_path)? {
        let path = entry?.path();
        if fs::metadata(&path)?.is_dir() {
            benchmark_dirs.push(path);
        }
    }

    benchmark_dirs.sort_unstable();

    let mut wtr = csv::Writer::from_path("harness/data_gen/dc_res.csv")?;
    wtr.serialize((
        "name",
        "iter",
        "initial_cost",
        "final_cost",
        "compression",
        "total time",
        "time without last rd",
        "mem",
    ))?;

    // For each directory we have, we use the following structure:
    // bench name
    // +- benchXXX_itY.json               (input - where we calculate initial cost)
    // +- out/dc
    //    +- raw/benchXXX_itY.json        (output programs - calculate final cost)
    //    +- processed/benchXXX_itY.json  (processed stats - find time etc.)

    for benchmark_dir in &benchmark_dirs {
        let mut inputs = Vec::new();

        for entry in fs::read_dir(benchmark_dir)? {
            let input_path = entry?.path();
            if fs::metadata(&input_path)?.is_file() {
                inputs.push(input_path);
            }
        }

        inputs.sort_unstable();

        for input_path in inputs {
            process_bench_file(&mut wtr, benchmark_dir, input_path)?;
        }
    }

    Ok(())
}

fn process_bench_file(
    wtr: &mut csv::Writer<File>,
    benchmark_dir: impl AsRef<Path>,
    input_path: impl AsRef<Path>,
) -> anyhow::Result<()> {
    let input_path = input_path.as_ref();
    let benchmark_dir = benchmark_dir.as_ref();
    // This is a benchXXX_itY.json file.
    let name = input_path.file_name().unwrap();

    let input_str = std::fs::read_to_string(input_path)?;
    let input: CompressionInput = serde_json::from_str(&input_str).unwrap();

    if !benchmark_dir.join("out/dc").exists() {
        return Ok(());
    }

    let out_path = fs::read_dir(benchmark_dir.join("out/dc"))?
        .next()
        .unwrap()?
        .path();

    let output_path = out_path.join("raw").join(name);
    let output_str = std::fs::read_to_string(output_path)?;
    let output: CompressionOutput = serde_json::from_str(&output_str).unwrap();

    let processed_path = out_path.join("processed").join(name);
    let processed_str = std::fs::read_to_string(processed_path)?;
    let processed: CompressionSummary = serde_json::from_str(&processed_str).unwrap();

    // Each of the frontiers represents a synthesis problem with multiple equivalent
    // possibilities.
    // We take the minimum cost of all the exprs in each frontier and all them
    // together.
    let mut new_libs = HashSet::new();

    let initial_cost: usize = {
        // Get all programs
        let progs = input
            .frontiers
            .into_iter()
            .map(|mut f| Expr::from(f.programs.remove(0).program));

        // For each program, we do a cost calc.
        // We accumulate the total library set.
        let mut libs = HashMap::new();
        let mut expr_cost = 0;

        for prog in progs {
            let (ls, ec) = calc_cost(&prog.into(), &new_libs);
            libs.extend(ls);
            expr_cost += ec;
        }

        expr_cost += libs.values().sum::<usize>();
        expr_cost + 1
    };

    // Next, we figure out which libs we just learned
    for inv in processed.inventions {
        // We need to extract the body of the inlined.
        let inl: Expr<DreamCoderOp> = inv.dreamcoder.into();
        if let DreamCoderOp::Inlined(e) = inl.0.operation() {
            new_libs.insert(*e.clone());
        } else {
            panic!("non-inlined expr learned as invention");
        }
    }

    // And calculate our final cost from the output!
    let final_cost: usize = {
        // Get all programs
        let progs = output
            .frontiers
            .into_iter()
            .map(|mut f| Expr::from(f.programs.remove(0).program));

        // For each program, we do a cost calc.
        // We accumulate the total library set.
        let mut libs = HashMap::new();
        let mut expr_cost = 0;

        for prog in progs {
            let (ls, ec) = calc_cost(&prog.into(), &new_libs);
            libs.extend(ls);
            expr_cost += ec;
        }

        expr_cost += libs.values().sum::<usize>();
        expr_cost + 1
    };

    // Write to our csv.
    let bench_name = benchmark_dir.file_name().unwrap();
    let cost_ratio = (initial_cost as f64) / (final_cost as f64);
    let total_time_seconds = processed.metrics.s_total;
    let bench_time_seconds =
        processed.metrics.ms_per_inv * (processed.num_inventions as f64) / 1000.0;
    println!(
        "{bench_name:?}/{name:?}:\t\
                 {initial_cost:>4} -> {final_cost:>4} (r {cost_ratio:>6.3}) \
                 in {bench_time_seconds:>5.1}/{total_time_seconds:>5.1}s",
    );
    wtr.serialize((
        bench_name.to_string_lossy(),
        name.to_string_lossy(),
        initial_cost,
        final_cost,
        cost_ratio,
        total_time_seconds,
        bench_time_seconds,
        processed.metrics.mem_peak_kb,
    ))?;

    Ok(())
}
