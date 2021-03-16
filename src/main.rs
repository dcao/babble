use babble::smiley_lang::*;
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

    let content = fs::read(&f).expect(&format!("error: failed to open bab file {}", &f));
    let input = String::from_utf8_lossy(&content);

    let expr = run_single(&input);

    println!("output:");
    println!("{}", expr);
}
