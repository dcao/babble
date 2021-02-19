//! The AST defining the smiley language.

use egg::{define_language, Id};
use ordered_float::NotNan;

pub type Constant = NotNan<f64>;

define_language! {
    /// A smiley expression
    pub enum Expr {
        "let" = Let([Id; 3]),
        "fn" = Lambda([Id; 2]),
        "var" = Var(Id),
        
        "circle" = Circle([Id; 2]),
        "segment" = Segment([Id; 3]),
        "translate" = Translate([Id; 2]),
        "scale" = Scale([Id; 2]),
        "rotate" = Rotate([Id; 2]),
        "+" = Compose([Id; 2]),

        "pos" = Position([Id; 2]),

        Signed(i32),
        Float(Constant),
        Symbol(egg::Symbol),
    }
}

