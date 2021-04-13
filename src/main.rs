#![warn(
    clippy::all,
    clippy::pedantic,
    clippy::cargo,
    anonymous_parameters,
    elided_lifetimes_in_paths,
    missing_copy_implementations,
    missing_debug_implementations,
    single_use_lifetimes,
    trivial_casts,
    unreachable_pub,
    unused_lifetimes,
)]

use babble::smiley_lang;
use clap::Clap;
use std::fs;

/// Experimental library learning.
#[derive(Clap)]
#[clap(version = "0.1.0", author = "David Cao <david@cao.sh>")]
struct Opts {
    input: String,
}

fn main() {
    env_logger::init();

    let opts: Opts = Opts::parse();
    let f = opts.input;

    let content = fs::read(&f).unwrap_or_else(|e| panic!("failed to open bab file {} error {}", &f, e));
    let input = String::from_utf8_lossy(&content);

    let expr = smiley_lang::run_single(&input);

    println!("output:");
    println!("{}", expr);
}
