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

use clap::Clap;
use dreamcoder::json::CompressionInput;
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
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

    let input: CompressionInput = serde_json::from_str(&input).expect("Error parsing JSON input");

    // TODO: Actually do something with the input

    let output = dreamcoder::run(input);

    if opts.pretty {
        serde_json::to_writer_pretty(io::stdout(), &output)
            .expect("Error pretty-printing JSON output");
        println!();
    } else {
        serde_json::to_writer(io::stdout(), &output).expect("Error printing JSON output");
    }
}
