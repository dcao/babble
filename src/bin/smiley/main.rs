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
    ast_node::{combine_exprs, Expr, Pretty},
    experiments::Experiments,
    sexp::Program,
};
use clap::Clap;
use egg::{AstSize, CostFunction, RecExpr};
use std::{
    convert::TryInto,
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

    /// Whether to learn "library functions" with no arguments.
    #[clap(long)]
    learn_constants: bool,

    /// Maximum arity of functions to learn.
    #[clap(long)]
    max_arity: Option<usize>,

    /// Do not use domain-specific rewrites
    #[clap(long)]
    no_dsr: bool,

    /// The beam sizes to use for the beam extractor
    #[clap(long)]
    beams: Vec<usize>,

    /// The timeouts to use for the ILP extractor
    #[clap(long)]
    timeout: Vec<u64>,

    /// The number of libs to learn at a time
    #[clap(long)]
    lps: Vec<usize>,

    /// The number of rounds of lib learning to run
    #[clap(long, default_value_t = 1)]
    rounds: usize,

    /// Whether to use the additional partial order reduction step
    #[clap(long)]
    extra_por: Vec<bool>,
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

    // Parse a list of exprs
    let prog: Vec<Expr<_>> = Program::parse(&input)
        .expect("Failed to parse program")
        .0
        .into_iter()
        .map(|x| {
            x.try_into()
                .expect("Input is not a valid list of expressions")
        }) // Vec<Sexp> -> Vec<Expr>
        .collect();

    if opts.svg {
        let expr: Expr<_> = combine_exprs(prog).into();
        let value = eval::eval(&expr).expect("Failed to evaluate expression");
        let picture = value
            .into_picture()
            .expect("Result of evaluation is not a picture");
        picture.write_svg(io::stdout()).expect("Error writing SVG");
    } else {
        // For the sake of pretty printing
        {
            let initial_expr: RecExpr<_> = combine_exprs(prog.clone());
            let initial_cost = AstSize.cost_rec(&initial_expr);

            println!("Initial expression (cost {}):", initial_cost);
            println!("{}", Pretty(&Expr::from(initial_expr.clone())));
            println!();
        }

        let dsrs = if opts.no_dsr {
            vec![]
        } else {
            vec![
                egg::rewrite!("circle rotate"; "circle" => "(rotate 90 circle)"),
                egg::rewrite!("circle scale"; "circle" => "(scale 1 circle)"),
            ]
        };

        let exps = Experiments::gen(
            prog,
            vec![],
            dsrs,
            opts.beams.clone(),
            opts.lps.clone(),
            opts.rounds,
            opts.extra_por.clone(),
            opts.timeout.clone(),
            (),
            opts.learn_constants,
            opts.max_arity,
        );

        println!("running...");
        exps.run("target/res_smiley.csv");
    }
}
