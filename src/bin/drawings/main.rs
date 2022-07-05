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

use crate::lang::Drawing;
use babble::{
    ast_node::{combine_exprs, Expr, Pretty},
    experiments::{plumbing, Experiments},
    rewrites,
    sexp::Program,
};
use clap::Parser;
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

#[derive(Parser)]
#[clap(version, author, about)]
struct Opts {
    /// The file with training programs. If no file is specified, reads from stdin.
    #[clap(parse(from_os_str))]
    file: Option<PathBuf>,

    /// CSV path to output stuff to
    #[clap(long, default_value = "harness/data_gen/res_drawing.csv")]
    output: String,

    /// The file with test programs. If no file is specified, just compresses training data.
    #[clap(parse(from_os_str))]
    test_file: Option<PathBuf>,

    /// Evaluate the input file and output it as an SVG.
    #[clap(long)]
    svg: bool,

    /// Evaluate the input file and output all evaluations of that lib
    #[clap(long)]
    eval_lib: Option<String>,

    /// Whether to learn "library functions" with no arguments.
    #[clap(long)]
    learn_constants: bool,

    /// Optional file with domain-specific rewrites.
    #[clap(long)]
    dsr: Option<PathBuf>,

    /// Maximum arity of functions to learn.
    #[clap(long)]
    max_arity: Option<usize>,

    /// The beam sizes to use for the beam extractor
    #[clap(long)]
    beams: Vec<usize>,

    /// The number of libs to learn at a time
    #[clap(long)]
    lps: Vec<usize>,

    /// The number of rounds of lib learning to run
    #[clap(long, default_value_t = 1)]
    rounds: usize,

    /// The number of programs to use
    #[clap(long)]
    limit: Option<usize>,

    /// Whether to use the additional partial order reduction step
    #[clap(long)]
    extra_por: Vec<bool>,
}

fn find_apps(exprs: Vec<Expr<Drawing>>, lib: Option<babble::learn::LibId>) -> Vec<Expr<Drawing>> {
    let mut res = Vec::new();

    // We want to find every (sub)expr which is an application to a lib;
    // either the specific lib given to us as the argument Some(lib), or
    // all libs if given None.

    fn walk(expr: &Expr<Drawing>, lib: Option<babble::learn::LibId>, res: &mut Vec<Expr<Drawing>>) {
        // We check what kind of operation this expr is
        match expr.as_ref().as_binding_expr() {
            Some(babble::teachable::BindingExpr::Apply(e, _arg)) => {
                // Check if this is a curried app to a fn
                let mut cur = e;
                let mut q = vec![];

                // Infinitely loop
                loop {
                    // The first argument will be another application, a LibVar,
                    // or something else.
                    match cur.as_ref().as_binding_expr() {
                        Some(babble::teachable::BindingExpr::Apply(ie, arg)) => {
                            // Recurse over body and continue
                            q.push(arg);
                            cur = ie;
                        }
                        Some(babble::teachable::BindingExpr::LibVar(lv)) => {
                            if let Some(tl) = lib {
                                if tl != lv {
                                    // Not what we're looking for.
                                    // Give up and break out of the loop
                                    break;
                                }
                            }

                            // If we're here, we found a successful app of a lib
                            // to args!
                            // Add it to our exprs, analyze everything in the q,
                            // then return
                            res.push(expr.clone());

                            for i in q {
                                walk(i, lib, res);
                            }

                            return;
                        }
                        _ => {
                            // Got something else, give up
                            break;
                        }
                    }
                }
            }
            _ => {}
        }

        // Recursively walk over children
        for child in expr.as_ref().args() {
            walk(child, lib, res);
        }
    }

    for expr in exprs {
        walk(&expr, lib, &mut res);
    }

    res
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
    let mut prog: Vec<Expr<Drawing>> = Program::parse(&input)
        .expect("Failed to parse training set")
        .0
        .into_iter()
        .map(|x| {
            x.try_into()
                .expect("Training input is not a valid list of expressions")
        }) // Vec<Sexp> -> Vec<Expr>
        .collect();

    if let Some(limit) = opts.limit {
        if limit < prog.len() {
            prog.truncate(limit);
        }
    }

    // If test file is specified, parse it as a list of exprs:
    let test_prog: Option<Vec<Expr<Drawing>>> = opts.test_file.map(|f| {
        let input = fs::read_to_string(&f).expect("Error reading test file");
        Program::parse(&input)
            .expect("Failed to parse test set")
            .0
            .into_iter()
            .map(|x| {
                x.try_into()
                    .expect("Test input is not a valid list of expressions")
            })
            .collect()
    });

    if opts.svg {
        if let Some(limit) = opts.limit {
            if limit > prog.len() {
                prog.truncate(limit);
            }
        }

        let expr: Expr<_> = combine_exprs(prog).into();
        let value = eval::eval(&expr).expect("Failed to evaluate expression");
        let picture = value
            .into_picture()
            .expect("Result of evaluation is not a picture");
        picture.write_svg(io::stdout()).expect("Error writing SVG");
    } else if let Some(l) = opts.eval_lib {
        // We assume the input file is the RecExpr output of a babble evaluation
        // It should have libs and all that.
        // Since it's been libified, it's just one expr.
        let expr = RecExpr::from(prog[0].clone());

        // Now, we want to do some plumbing stuff.
        // First, we split the program into its libs and progs
        let libs = plumbing::libs(expr.as_ref());
        let progs = plumbing::exprs(expr.as_ref());

        // With the progs, find apps
        let tgt = Some(babble::learn::LibId(usize::from_str_radix(&l, 10).unwrap()));
        let mut new_progs = find_apps(progs, tgt);

        if let Some(limit) = opts.limit {
            if limit > prog.len() {
                new_progs.truncate(limit);
            }
        }

        // Hack to pretty print the fn
        log::info!(
            "{}",
            Pretty(&Expr::from(RecExpr::from(libs[&tgt.unwrap()].clone())))
        );

        // Recombine and eval
        let fin = plumbing::combine(libs, new_progs);

        let value = eval::eval(&fin).expect(&format!("lib {} doesn't produce pictures", l));
        let picture = value
            .into_picture()
            .expect("Result of evaluation is not a picture");
        picture.write_svg(io::stdout()).expect("Error writing SVG");
    } else {
        // For the sake of pretty printing
        {
            let initial_expr: RecExpr<_> = combine_exprs(prog.clone());
            let initial_cost = AstSize.cost_rec(&initial_expr);

            println!("Training expression (cost {}):", initial_cost);
            println!("{}", Pretty(&Expr::from(initial_expr.clone())));
            println!();

            // If test expressions are specified, print them too:
            if let Some(test_prog) = test_prog.clone() {
                let test_expr: RecExpr<_> = combine_exprs(test_prog);
                let test_cost = AstSize.cost_rec(&test_expr);
                println!("Test expression (cost {}):", test_cost);
                println!("{}", Pretty(&Expr::from(test_expr.clone())));
                println!();
            }
        }

        // If dsr file is specified, read it:
        let dsrs = if let Some(dsr_path) = opts.dsr {
            match rewrites::from_file(dsr_path) {
                Ok(dsrs) => dsrs,
                Err(e) => {
                    eprintln!("Error reading dsr file: {}", e);
                    std::process::exit(1);
                }
            }
        } else {
            vec![]
        };

        let exps = Experiments::gen(
            prog,
            test_prog.unwrap_or_default(),
            dsrs,
            opts.beams.clone(),
            opts.lps.clone(),
            opts.rounds,
            opts.extra_por.clone(),
            vec![],
            (),
            opts.learn_constants,
            opts.max_arity,
        );

        println!("running...");
        exps.run(&opts.output);
    }
}
