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
    ast_node::{AstNode, Expr},
    learn::LearnedLibrary,
    sexp::Sexp, extract::{beam::{PartialLibCost, less_dumb_extractor}, lift_libs, true_cost},
};
use clap::Clap;
use egg::{AstSize, CostFunction, EGraph, Extractor, RecExpr, Rewrite, Runner};
use log::info;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    convert::TryInto,
    fs,
    io::{self, Read},
    path::PathBuf,
};

use crate::lang::Smiley;

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

    let expr: Expr<_> = Sexp::parse(&input)
        .expect("Failed to parse sexp")
        .try_into()
        .expect("Input is not a valid expression");

    if opts.svg {
        let value = eval::eval(&expr).expect("Failed to evaluate expression");
        let picture = value
            .into_picture()
            .expect("Result of evaluation is not a picture");
        picture.write_svg(io::stdout()).expect("Error writing SVG");
    } else {
        let initial_expr: RecExpr<_> = expr.into();
        let initial_cost = AstSize.cost_rec(&initial_expr);

        println!("Initial expression (cost {}):", initial_cost);
        println!("{}", initial_expr.pretty(100));
        println!();

        println!("stage one");
        let mut aeg = EGraph::new(PartialLibCost::new(100, 100));
        let root = aeg.add_expr(&initial_expr);
        aeg.rebuild();

        let learned_lib = LearnedLibrary::from(&aeg);
        let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();
        let egraph = Runner::<_, _, ()>::new(PartialLibCost::new(100, 100))
            .with_egraph(aeg.clone())
            .with_iter_limit(1)
            .run(lib_rewrites.iter())
            .egraph;

        let mut cs = egraph[egraph.find(root)].data.clone();
        cs.set.sort_unstable_by_key(|elem| elem.full_cost);

        println!("learned libs");
        let all_libs: Vec<_> = learned_lib.libs().collect();
        for x in &cs.set {
            for lib in &x.libs {
                println!("{}: {}", lib.0, &all_libs[lib.0 .0]);
            }
            println!("upper bound ('full') cost: {:?}", x);
            println!();
        }


        println!("extracting (with duplicate libs)");
        let (lifted, final_cost) = cs
            .set
            .par_iter()
            .map(|ls| {
                // Add the root combine node again
                let mut fin = Runner::<_, _, ()>::new(PartialLibCost::new(0, 0))
                    .with_egraph(aeg.clone())
                    .with_iter_limit(1)
                    .run(
                        lib_rewrites
                            .iter()
                            .enumerate()
                            .filter(|(i, _)| ls.libs.iter().any(|x| *i == x.0 .0))
                            .map(|x| x.1),
                    )
                    .egraph;

                // let extractor = Extractor::new(&fin, NoLibCost);
                // let (_, best) = extractor.find_best(fin.find(root));
                // println!();

                // println!("{:#?}", fin[root]);
                // println!("{:#?}", fin[17.into()]);
                // println!("{:#?}", fin[31.into()]);

                let best = less_dumb_extractor(&fin, root);

                // println!("extracting (before lib lifting)");
                // println!("{}", best.pretty(100));
                // println!();

                let lifted = lift_libs(best);
                let final_cost = true_cost(lifted.clone()) - 1;

                (lifted, final_cost)
            })
            .min_by_key(|x| x.1)
            .unwrap();

        // println!("{}", lifted.pretty(100));
        println!("{}", lifted.pretty(100));
        println!("final cost: {}", final_cost);
        println!();

        // let egraph = runner.egraph;
        // info!("Number of nodes: {}", egraph.total_size());
        // let (final_cost, final_expr) = Extractor::new(&egraph, AstSize).find_best(root);

        // println!("Final expression (cost {}):", final_cost);
        // println!("{}", final_expr.pretty(100));
        // println!();

        // #[allow(clippy::cast_precision_loss)]
        // let compression_ratio = (initial_cost as f64) / (final_cost as f64);
        // println!("Compression ratio: {:.2}", compression_ratio);
    }
}
