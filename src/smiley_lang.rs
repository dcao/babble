//! The AST defining the smiley language.

use egg::{rewrite as rw, *};
use ordered_float::NotNan;

pub type EGraph = egg::EGraph<Smiley, ()>;
pub type Rewrite = egg::Rewrite<Smiley, ()>;
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
        "line" = Segment([Id; 3]),

        "move" = Move([Id; 3]),
        "scale" = Scale([Id; 2]),
        "rotate" = Rotate([Id; 2]),
        "+" = Compose([Id; 2]),

        Signed(i32),
        Float(Constant),
        Symbol(egg::Symbol),
    }
}

fn test_anti_unif_rules() -> Vec<Rewrite> {
    vec![
        rw!(
            "example";
            "(let f (fn x x) (let ?v1 (app f (circle ?r1 ?pos1)) (let ?v2 (app f (circle ?r2 ?pos2)) ?b)))" =>
            // Absorbing the circle requires absorbing the required arguments to circle
            "(let f (fn r (fn pos (circle r pos))) (let ?v1 (app (app f ?r1) ?pos1) (let ?v2 (app (app f ?r2) ?pos2) ?b)))"),
    ]
}

// Rules for testing if an egg var can bind an expr and its arguments
// e.g. can ?b bind "move 4 2"
fn test_binder_rules() -> Vec<Rewrite> {
    vec![
        rw!("test_binder"; "(rotate ?t (?b (scale ?v ?s)))" => "(rotate ?t (fn s (?b (scale s ?s))))"),
    ]
}

struct ToyCostFn;

impl CostFunction<Smiley> for ToyCostFn {
    type Cost = isize;

    fn cost<C>(&mut self, enode: &Smiley, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            Smiley::Fn(_) => -100,
            _ => 1,
        };
        enode.fold(op_cost, |sum, id| sum + costs(id))
    }
}

/// Execute EGraph building and program extraction on a single expression
/// containing all of the programs to extract common fragments out of.
///
/// Programs should be in the form:
/// ```
/// let e1 = (...)
/// let e2 = (...)
/// let e3 = (...)
/// ```
pub fn run_single(expr: &str) -> RecExpr<Smiley> {
    // Our list of rewrite rules is here
    let rules: &[Rewrite] = &test_binder_rules();

    // First, parse the expression and build an egraph from it
    let expr = expr.parse().unwrap();
    let runner = Runner::default().with_expr(&expr).run(rules);
    let (egraph, root) = (runner.egraph, runner.roots[0]);

    // Then, extract the best program from the egraph, starting at
    // the root
    let mut extractor = Extractor::new(&egraph, ToyCostFn);
    let (_best_cost, best) = extractor.find_best(root);

    // Return the best program
    best
}
