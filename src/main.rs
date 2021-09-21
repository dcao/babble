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

use babble::{dreamcoder::{self, json::CompressionInput}, list_lang, smiley_lang};
use clap::Clap;
use egg::Runner;
use std::{
    fs,
    io::{self, stdout, Read},
    path::PathBuf,
};

#[allow(clippy::struct_excessive_bools)]
#[derive(Clap)]
#[clap(version, author, about)]
struct Opts {
    /// The input file. If no file is specified, reads from stdin.
    #[clap(parse(from_os_str))]
    file: Option<PathBuf>,

    /// Output the intermediate e-graphs to egraphs/iteration-N.svg.
    #[clap(long)]
    dump_egraphs: bool,

    /// Enables DreamCoder compatibility mode.
    #[clap(long)]
    dreamcoder: bool,

    /// Enables pretty-printing of JSON output.
    #[clap(long)]
    pretty: bool,

    /// Evaluate the input file and output it as an SVG.
    #[clap(long)]
    svg: bool,
}

fn main() {
    env_logger::init();

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

    if opts.dreamcoder {
        let input: CompressionInput =
            serde_json::from_str(&input).expect("Error parsing JSON input");

        // TODO: Actually do something with the input

        let output = dreamcoder::run(input);

        if opts.pretty {
            serde_json::to_writer_pretty(io::stdout(), &output)
                .expect("Error pretty-printing JSON output");
            println!();
        } else {
            serde_json::to_writer(io::stdout(), &output).expect("Error printing JSON output");
        }
    } else {
        let expr = input.parse().expect("Input is not a valid expression");
        if opts.svg {
            // let value = smiley_lang::eval(&expr).expect("Failed to evaluate expression");
            // let picture = value.into_picture().expect("Result of evaluation is not a picture");
            // picture.write_svg(stdout()).expect("Error writing SVG");
        } else {
            let mut runner = Runner::default();
            if opts.dump_egraphs {
                runner = runner.with_hook(|runner| {
                    let iteration = runner.iterations.len();
                    let file = format!("egraphs/iteration-{}.svg", iteration);
                    runner.egraph.dot().to_svg(file).map_err(|e| e.to_string())
                });
            }

            let runner = runner.with_expr(&expr);
            list_lang::run_single(runner);
        }
    }
}
