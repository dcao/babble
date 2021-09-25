//! The language of list transformations.

use crate::{
    ast_node::{Arity, AstNode},
    free_vars::{and, not_free_in, FreeVarAnalysis, FreeVars},
    learn::LearnedLibrary,
    teachable::Teachable,
};
use babble_macros::rewrite_rules;
use egg::{AstSize, Extractor, Rewrite, Runner, Symbol};
use lazy_static::lazy_static;
use std::{
    collections::HashSet,
    convert::Infallible,
    fmt::{self, Display, Formatter},
    num::ParseIntError,
    str::FromStr,
};
use thiserror::Error;

/// List operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ListOp {
    /// Add an element to the front of a list
    Cons,
    /// A boolean literal
    Bool(bool),
    /// A conditional expression
    If,
    /// An integer literal
    Int(i32),
    /// A function application
    Apply,
    /// A de Bruijn-indexed variable
    Var(DeBruijnIndex),
    /// An identifier
    Ident(Symbol),
    /// An anonymous function
    Lambda,
    /// A let-expression
    Let,
    /// A library function binding
    Lib,
}

impl Arity for ListOp {
    fn arity(&self) -> usize {
        match self {
            Self::Bool(_) | Self::Int(_) | Self::Var(_) | Self::Ident(_) => 0,
            Self::Lambda => 1,
            Self::Cons | Self::Apply => 2,
            Self::If | Self::Let | Self::Lib => 3,
        }
    }
}

impl Display for ListOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Cons => "cons",
            Self::If => "if",
            Self::Apply => "@",
            Self::Lambda => "λ",
            Self::Let => "let",
            Self::Lib => "lib",
            Self::Bool(b) => {
                return write!(f, "{}", b);
            }
            Self::Int(i) => {
                return write!(f, "{}", i);
            }
            Self::Var(index) => {
                return write!(f, "{}", index);
            }
            Self::Ident(ident) => {
                return write!(f, "{}", ident);
            }
        };
        f.write_str(s)
    }
}

/// A de Bruin index.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DeBruijnIndex(pub usize);

impl Display for DeBruijnIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl From<usize> for DeBruijnIndex {
    fn from(index: usize) -> Self {
        Self(index)
    }
}

impl From<DeBruijnIndex> for usize {
    fn from(index: DeBruijnIndex) -> Self {
        index.0
    }
}

/// An error when parsing a de Bruijn index.
#[derive(Clone, Debug, Error)]
pub enum ParseDeBruijnIndexError {
    /// Did not start with "$"
    #[error("expected de Bruijn index to start with '$")]
    NoLeadingDollar,
    /// Index is not a valid integer
    #[error(transparent)]
    InvalidIndex(ParseIntError),
}

impl FromStr for DeBruijnIndex {
    type Err = ParseDeBruijnIndexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(n) = s.strip_prefix('$') {
            let n = n.parse().map_err(ParseDeBruijnIndexError::InvalidIndex)?;
            Ok(DeBruijnIndex(n))
        } else {
            Err(ParseDeBruijnIndexError::NoLeadingDollar)
        }
    }
}

impl FromStr for ListOp {
    type Err = Infallible;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let op = match input {
            "cons" => Self::Cons,
            "if" => Self::If,
            "apply" | "@" => Self::Apply,
            "lambda" | "λ" => Self::Lambda,
            "let" => Self::Let,
            "lib" => Self::Lib,
            input => input
                .parse()
                .map(Self::Bool)
                .or_else(|_| input.parse().map(Self::Var))
                .unwrap_or_else(|_| Self::Ident(input.into())),
        };
        Ok(op)
    }
}

impl Teachable for ListOp {
    fn lambda<T>(body: T) -> AstNode<Self, T> {
        AstNode::new(Self::Lambda, [body])
    }

    fn is_lambda<T>(node: &AstNode<Self, T>) -> bool {
        node.operation() == &Self::Lambda
    }

    fn apply<T>(fun: T, arg: T) -> AstNode<Self, T> {
        AstNode::new(Self::Apply, [fun, arg])
    }

    fn var<T>(index: usize) -> AstNode<Self, T> {
        AstNode::new(Self::Var(DeBruijnIndex(index)), [])
    }

    fn var_index(&self) -> Option<usize> {
        if let Self::Var(DeBruijnIndex(index)) = *self {
            Some(index)
        } else {
            None
        }
    }

    fn ident<T>(name: Symbol) -> AstNode<Self, T> {
        AstNode::new(Self::Ident(name), [])
    }

    fn lib<T>(name: T, fun: T, body: T) -> AstNode<Self, T> {
        AstNode::new(Self::Lib, [name, fun, body])
    }
}

impl FreeVars for ListOp {
    fn ident_symbol(&self) -> Option<Symbol> {
        match *self {
            Self::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    fn free_vars(&self, children: &[&HashSet<Symbol>]) -> HashSet<Symbol> {
        match (self, children) {
            (&Self::Ident(ident), []) => {
                let mut result = HashSet::new();
                result.insert(ident);
                result
            }
            (Self::Let | Self::Lib, &[ident, val, body]) => &(body - ident) | val,
            (_, children) => children
                .iter()
                .flat_map(|child| child.iter())
                .copied()
                .collect(),
        }
    }
}

lazy_static! {
    static ref LIFT_LIB_REWRITES: &'static [Rewrite<AstNode<ListOp>, FreeVarAnalysis<ListOp>>] = rewrite_rules! {
        // TODO: Check for captures of de Bruijn variables and re-index if necessary.
        lift_lambda: "(lambda (lib ?x ?v ?e))" => "(lib ?x ?v (lambda ?e))";

        // Binary operators
        lift_cons_both: "(cons (lib ?x ?v ?e1) (lib ?x ?v ?e2))" => "(lib ?x ?v (cons ?e1 ?e2))";
        lift_cons_left: "(cons (lib ?x ?v ?e1) ?e2)" => "(lib ?x ?v (cons ?e1 ?e2))" if not_free_in("?e2", "?x");
        lift_cons_right: "(cons ?e1 (lib ?x ?v ?e2))" => "(lib ?x ?v (cons ?e1 ?e2))" if not_free_in("?e1", "?x");

        lift_apply_both: "(apply (lib ?x ?v ?e1) (lib ?x ?v ?e2))" => "(lib ?x ?v (apply ?e1 ?e2))";
        lift_apply_left: "(apply (lib ?x ?v ?e1) ?e2)" => "(lib ?x ?v (apply ?e1 ?e2))" if not_free_in("?e2", "?x");
        lift_apply_right: "(apply ?e1 (lib ?x ?v ?e2))" => "(lib ?x ?v (apply ?e1 ?e2))" if not_free_in("?e1", "?x");

        // Ternary operators
        lift_if_123: "(if (lib ?x ?v ?e1) (lib ?x ?v ?e2) (lib ?x ?v ?e3))" => "(lib ?x ?v (if ?e1 ?e2 ?e3))";
        lift_if_12: "(if (lib ?x ?v ?e1) (lib ?x ?v ?e2) ?e3)" => "(lib ?x ?v (if ?e1 ?e2 ?e3))" if not_free_in("?e3", "?x");
        lift_if_13: "(if (lib ?x ?v ?e1) ?e2 (lib ?x ?v ?e3))" => "(lib ?x ?v (if ?e1 ?e2 ?e3))" if not_free_in("?e2", "?x");
        lift_if_23: "(if ?e1 (lib ?x ?v ?e2) (lib ?x ?v ?e3))" => "(lib ?x ?v (if ?e1 ?e2 ?e3))" if not_free_in("?e1", "?x");
        lift_if_1: "(if (lib ?x ?v ?e1) ?e2 ?e3)" => "(lib ?x ?v (if ?e1 ?e2 ?e3))" if and(not_free_in("?e2", "?x"), not_free_in("?e3", "?x"));
        lift_if_2: "(if ?e1 (lib ?x ?v ?e2) ?e3)" => "(lib ?x ?v (if ?e1 ?e2 ?e3))" if and(not_free_in("?e1", "?x"), not_free_in("?e3", "?x"));
        lift_if_3: "(if ?e1 ?e2 (lib ?x ?v ?e3))" => "(lib ?x ?v (if ?e1 ?e2 ?e3))" if and(not_free_in("?e1", "?x"), not_free_in("?e2", "?x"));

        // Binding expressions
        lift_let_both: "(let ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
        lift_let_body: "(let ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
        lift_let_binding: "(let ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");

        lift_lib_both: "(lib ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
        lift_lib_body: "(lib ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
        lift_lib_binding: "(lib ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");
    }.leak();
}

/// Execute `EGraph` building and program extraction on a single expression
/// containing all of the programs to extract common fragments out of.
pub fn run_single(runner: Runner<AstNode<ListOp>, FreeVarAnalysis<ListOp>>) {
    let learned_lib = LearnedLibrary::from(&runner.egraph);
    let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();

    let mut runner = runner.with_iter_limit(1).run(lib_rewrites.iter());
    runner.stop_reason = None;

    let runner = runner
        .with_iter_limit(30)
        .with_time_limit(core::time::Duration::from_secs(40))
        .run(LIFT_LIB_REWRITES.iter());

    let extractor = Extractor::new(&runner.egraph, AstSize);
    let (cost, expr) = extractor.find_best(runner.roots[0]);

    eprintln!("Cost: {}\n", cost);
    eprintln!("{}", expr.pretty(100));
}
