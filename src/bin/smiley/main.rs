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
    ast_node::{Expr, Pretty},
    extract::{
        beam::{LibExtractor, PartialLibCost},
        lift_libs, true_cost,
    },
    learn::LearnedLibrary,
    sexp::Sexp,
};
use clap::Clap;
use egg::{AstSize, CostFunction, EGraph, RecExpr, Runner};
// use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
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
        // println!("{}", initial_expr.pretty(100));
        println!("{}", Pretty(&Expr::from(initial_expr.clone())));
        println!();

        println!("stage one");
        let mut aeg = EGraph::new(PartialLibCost::new(100, 100));
        let root = aeg.add_expr(&initial_expr);
        aeg.rebuild();

        let runner = Runner::<_, _, ()>::new(PartialLibCost::new(100, 100))
            .with_egraph(aeg)
            .run(&[
                egg::rewrite!("scale 1"; "circle" => "(scale 1 circle)"),
                egg::rewrite!("rotate circle"; "circle" => "(rotate 90 circle)"),
            ]);

        let aeg = runner.egraph;

        let learned_lib = LearnedLibrary::from(&aeg);
        let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();
        let egraph = Runner::<_, _, ()>::new(PartialLibCost::new(100, 100))
            .with_egraph(aeg.clone())
            .with_iter_limit(1)
            .run(lib_rewrites.iter())
            .egraph;

        let mut cs = egraph[egraph.find(root)].data.clone();
        cs.set.sort_unstable_by_key(|elem| elem.full_cost);

        // println!("learned libs");
        // let all_libs: Vec<_> = learned_lib.libs().collect();
        // for x in &cs.set {
        //     for lib in &x.libs {
        //         println!("{}: {}", lib.0, &all_libs[lib.0 .0]);
        //     }
        //     println!("upper bound ('full') cost: {:?}", x);
        //     println!();
        // }

        println!("extracting (with duplicate libs)");
        let (lifted, final_cost) = cs
            .set
            .iter()
            .map(|ls| {
                // Add the root combine node again
                let fin = Runner::<_, _, ()>::new(PartialLibCost::new(0, 0))
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

                // let best = less_dumb_extractor(&fin, root);

                let mut extractor = LibExtractor::new(&fin);
                let best = extractor.best(root);

                let lifted = lift_libs(best);
                let final_cost = true_cost(lifted.clone()) - 1;

                (lifted, final_cost)
            })
            .min_by_key(|x| x.1)
            .unwrap();

        println!("{}", Pretty(&Expr::from(lifted)));
        println!("final cost: {}", final_cost);
        println!();
    }
}
