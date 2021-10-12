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

use babble::ast_node::AstNode;
use clap::Clap;
use egg::{RecExpr, Runner};
use lang::Smiley;
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
};

mod eval;
mod lang;
mod svg;

#[derive(Clap)]
#[clap(version, author, about)]
struct Opts {
    /// The input file. If no file is specified, reads from stdin.
    #[clap(parse(from_os_str))]
    file: Option<PathBuf>,

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

    let expr: RecExpr<AstNode<Smiley>> = input.parse().expect("Input is not a valid expression");
    if opts.svg {
        let expr = expr.into();
        let value = eval::eval(&expr).expect("Failed to evaluate expression");
        let picture = value
            .into_picture()
            .expect("Result of evaluation is not a picture");
        picture.write_svg(io::stdout()).expect("Error writing SVG");
    } else {
        let runner = Runner::default().with_expr(&expr);
        lang::run_single(runner);
    }
}
