//! The AST defining the smiley language.

// TODO: Remove this once define_language! allows doc strings
#![allow(missing_docs)]

use crate::rewrites;
use egg::{define_language, CostFunction, Extractor, Id, Language, RecExpr, Runner};
use ordered_float::NotNan;

/// E-graphs in the `Smiley` language.
pub type EGraph = egg::EGraph<Smiley, ()>;

/// Rewrites in the `Smiley` language.
pub type Rewrite = egg::Rewrite<Smiley, ()>;

/// Number constants in the `Smiley` language.
pub type Constant = NotNan<f64>;

define_language! {
    /// A smiley expression
    pub enum Smiley {
        // Meta:
        "let" = Let([Id; 3]),
        "fn" = Fn([Id; 2]),
        "app" = App([Id; 2]),
        "var" = Var(Id),

        // Shapes:
        "circle" = Circle,
        "line" = Line,

        "move" = Move([Id; 3]),
        "scale" = Scale([Id; 2]),
        "rotate" = Rotate([Id; 2]),
        "+" = Compose(Box<[Id]>),

        Signed(i32),
        Float(Constant),
        Symbol(egg::Symbol),
    }
}

/// Get a `Vec` of all the rewrite rules.
#[must_use]
pub fn rules() -> Vec<Rewrite> {
    [
        *rewrites::INTRO_FN,
        *rewrites::ANTI_UNIFY,
        *rewrites::ROTATE_FN,
        *rewrites::ADJACENT,
    ]
    .concat()
}

struct ToySize;
impl CostFunction<Smiley> for ToySize {
    type Cost = f64;
    fn cost<C>(&mut self, enode: &Smiley, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            Smiley::Scale(_) | Smiley::Rotate(_) | Smiley::Move(_) => 16.0,
            _ => 1.0,
        };
        enode.fold(op_cost, |sum, id| sum + costs(id))
    }
}

/// Execute `EGraph` building and program extraction on a single expression
/// containing all of the programs to extract common fragments out of.
///
/// Programs should be in the form:
/// ```ignore
/// let e1 = (...)
/// let e2 = (...)
/// let e3 = (...)
/// ```
///
/// # Panics
/// Panics if `expr` is not a valid smiley expression.
#[must_use]
pub fn run_single(expr: &str) -> RecExpr<Smiley> {
    // Our list of rewrite rules is here
    let rules: &[Rewrite] = &rules();

    // First, parse the expression and build an egraph from it
    let expr = expr.parse().unwrap();
    let runner = Runner::default()
        // .with_scheduler(SimpleScheduler)
        // .with_iter_limit(1_000)
        // .with_node_limit(1_000_000)
        // .with_time_limit(core::time::Duration::from_secs(20))
        .with_expr(&expr)
        .run(rules);
    let (egraph, root) = (runner.egraph, runner.roots[0]);

    // Then, extract the best program from the egraph, starting at
    // the root
    let mut extractor = Extractor::new(&egraph, ToySize);
    let (best_cost, best) = extractor.find_best(root);

    println!("best cost: {}", best_cost);

    // Return the best program
    best
}

#[allow(missing_docs)]
mod test {
    use super::*;
    use egg::test_fn;

    test_fn! {
        test_intro_fn, rules(),
        "(let s1 (rotate 50 (move 2 4 (scale 3 line))) (let s2 (rotate 100 (move 2 4 (scale 3 circle))) (+ s1 s2)))"
        =>
        "(let s1 (app (fn s (app (fn r (rotate r (move 2 4 (scale 3 s)))) 50)) line) (let s2 (app (fn s (app (fn r (rotate r (move 2 4 (scale 3 s)))) 100)) circle) (+ s1 s2)))",
    }

    test_fn! {
        test_swap_fn, rules(),
        "(let s1 (app (fn s (app (fn r (rotate r (move 2 4 (scale 3 s)))) 50)) line) (let s2 (app (fn s (app (fn r (rotate r (move 2 4 (scale 3 s)))) 100)) circle) (+ s1 s2)))"
        =>
        "(let s1 (app (app (fn s (fn r (rotate r (move 2 4 (scale 3 s))))) line) 50) (let s2 (app (app (fn s (fn r (rotate r (move 2 4 (scale 3 s))))) circle) 100) (+ s1 s2)))",
    }

    test_fn! {
        test_extract_fn, rules(),
        "(let s1 (app (app (fn s (fn r (rotate r (move 2 4 (scale 3 s))))) line) 50) (let s2 (app (app (fn s (fn r (rotate r (move 2 4 (scale 3 s))))) circle) 100) (+ s1 s2)))"
        =>
        "(let f (fn s (fn r (rotate r (move 2 4 (scale 3 s))))) (let s1 (app (app f line) 50) (let s2 (app (app f circle) 100) (+ s1 s2))))",
    }

    test_fn! {
        test_swap_fn2, rules(),
        "(let s1 (rotate 50 (move 2 4 (scale 3 (app (fn s s) line)))) (let s2 (rotate 100 (move 2 4 (scale 3 (app (fn s s) circle)))) (+ s1 s2)))"
        =>
        "(let s1 (app (app (fn s (fn r (rotate r (move 2 4 (scale 3 s))))) line) 50) (let s2 (app (app (fn s (fn r (rotate r (move 2 4 (scale 3 s))))) circle) 100) (+ s1 s2)))",
    }

    test_fn! {
        test_paper_1, rules(),
        runner = Runner::default()
            // .with_scheduler(SimpleScheduler)
            .with_iter_limit(1000)
            .with_node_limit(1_000_000),
        r#"
(let s1 (+ (move 4 4 (scale 2 line)) (+ (move 3 2 line) (+ (move 4 3 (scale 9 circle)) (move 5 2 line))))
  (let s2 (+ (move 4 4 (scale 2 circle)) (+ (move 3 2 circle) (+ (move 4 3 (scale 9 circle)) (move 5 2 circle))))
    (+ s1 s2)))
"#
        =>
        r#"
(let s1 (+ (move 4 4 (scale 2 (app (fn s s) line))) (+ (move 3 2 (app (fn s s) line)) (+ (move 4 3 (scale 9 circle)) (move 5 2 (app (fn s s) line)))))
  (let s2 (+ (move 4 4 (scale 2 (app (fn s s) circle))) (+ (move 3 2 (app (fn s s) circle)) (+ (move 4 3 (scale 9 circle)) (move 5 2 (app (fn s s) circle)))))
    (+ s1 s2)))
"#
    }

    test_fn! {
        test_paper_2, rules(),
        runner = Runner::default()
            // .with_scheduler(SimpleScheduler)
            .with_iter_limit(1000)
            .with_node_limit(1_000_000),
        r#"
(let s1 (+ (move 4 4 (scale 2 (app (fn s s) line))) (+ (move 3 2 (app (fn s s) line)) (+ (move 4 3 (scale 9 circle)) (move 5 2 (app (fn s s) line)))))
  (let s2 (+ (move 4 4 (scale 2 (app (fn s s) circle))) (+ (move 3 2 (app (fn s s) circle)) (+ (move 4 3 (scale 9 circle)) (move 5 2 (app (fn s s) circle)))))
    (+ s1 s2)))
"#
        =>
        r#"
(let s1 (app (fn s (app (fn s (+ (move 4 4 (scale 2 s)) (+ (move 3 2 s) (+ (move 4 3 (scale 9 circle)) (move 5 2 (app (fn s s) line)))))) line)) line)
  (let s2 (+ (move 4 4 (scale 2 (app (fn s s) circle))) (+ (move 3 2 (app (fn s s) circle)) (+ (move 4 3 (scale 9 circle)) (move 5 2 (app (fn s s) circle)))))
    (+ s1 s2)))
"#
    }

    test_fn! {
        test_paper_3, rules(),
        runner = Runner::default()
            // .with_scheduler(SimpleScheduler)
            .with_iter_limit(1000)
            .with_node_limit(1_000_000),
        r#"
(let s1 (app (fn s (app (fn s (+ (move 4 4 (scale 2 s)) (+ (move 3 2 s) (+ (move 4 3 (scale 9 circle)) (move 5 2 (app (fn s s) line)))))) line)) line)
  (let s2 (+ (move 4 4 (scale 2 (app (fn s s) circle))) (+ (move 3 2 (app (fn s s) circle)) (+ (move 4 3 (scale 9 circle)) (move 5 2 (app (fn s s) circle)))))
    (+ s1 s2)))
"#
        =>
        r#"
(let s1 (app (fn s (+ (move 4 4 (scale 2 s)) (+ (move 3 2 s) (+ (move 4 3 (scale 9 circle)) (move 5 2 s))))) line)
  (let s2 (+ (move 4 4 (scale 2 (app (fn s s) circle))) (+ (move 3 2 (app (fn s s) circle)) (+ (move 4 3 (scale 9 circle)) (move 5 2 (app (fn s s) circle)))))
    (+ s1 s2)))
"#
    }
}
