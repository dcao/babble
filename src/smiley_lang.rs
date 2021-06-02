//! The AST defining the smiley language.

// TODO: Remove this once define_language! allows doc strings
#![allow(missing_docs)]

use crate::{anti_unify::AntiUnifTgt, rewrites};
use egg::{define_language, CostFunction, Extractor, Id, IterationData, Language, RecExpr, Runner};
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
        "fn" = Fn(Id),
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

impl AntiUnifTgt for Smiley {
    fn lambda(body: Id) -> Self {
        Self::Fn(body)
    }

    fn is_lambda(node: &Self) -> bool {
        matches!(node, Self::Fn(..))
    }

    fn app(lambda: Id, arg: Id) -> Self {
        Self::App([lambda, arg])
    }

    fn lambda_arg(ix: usize) -> Self {
        Self::Symbol(egg::Symbol::from(format!("arg_{}", ix.to_string())))
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
#[must_use]
pub fn run_single<T: IterationData<Smiley, ()>>(runner: Runner<Smiley, (), T>) -> RecExpr<Smiley> {
    // Our list of rewrite rules is here
    let rules = rules();

    let runner = runner.run(&rules);
    let (egraph, root) = (runner.egraph, runner.roots[0]);

    // Then, extract the best program from the egraph, starting at
    // the root
    let mut extractor = Extractor::new(&egraph, ToySize);
    let (best_cost, best) = extractor.find_best(root);

    println!("best cost: {}", best_cost);

    // Return the best program
    best
}
