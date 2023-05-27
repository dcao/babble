//! The language of list transformations.

use babble::{
    ast_node::{Arity, AstNode, Expr, Precedence, Printable, Printer},
    learn::{LibId, ParseLibIdError},
    teachable::{BindingExpr, DeBruijnIndex, Teachable},
};
use babble_macros::rewrite_rules;
use egg::{Rewrite, Symbol};
use lazy_static::lazy_static;
use std::{
    convert::Infallible,
    fmt::{self, Display, Formatter, Write},
    str::FromStr,
};

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
    /// A library function binding
    Lib(LibId),
    /// A reference to a lib var
    LibVar(LibId),
    /// A list
    List,
}

impl Arity for ListOp {
    fn min_arity(&self) -> usize {
        match self {
            Self::Bool(_)
            | Self::Int(_)
            | Self::Var(_)
            | Self::Ident(_)
            | Self::LibVar(_)
            | Self::List => 0,
            Self::Lambda => 1,
            Self::Cons | Self::Apply | Self::Lib(_) => 2,
            Self::If => 3,
        }
    }

    fn max_arity(&self) -> Option<usize> {
        match self {
            Self::List => None,
            other => Some(other.min_arity()),
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
            Self::List => "list",
            Self::Lib(ix) => {
                return write!(f, "lib {ix}");
            }
            Self::LibVar(ix) => {
                return write!(f, "{ix}");
            }
            Self::Bool(b) => {
                return write!(f, "{b}");
            }
            Self::Int(i) => {
                return write!(f, "{i}");
            }
            Self::Var(index) => {
                return write!(f, "{index}");
            }
            Self::Ident(ident) => {
                return write!(f, "{ident}");
            }
        };
        f.write_str(s)
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
            "list" => Self::List,
            input => input
                .parse()
                .map(Self::Bool)
                .or_else(|_| input.parse().map(Self::Var))
                .or_else(|_| input.parse().map(Self::Int))
                .or_else(|_| input.parse().map(Self::LibVar))
                .or_else(|_| {
                    input
                        .strip_prefix("lib ")
                        .ok_or(ParseLibIdError::NoLeadingL)
                        .and_then(|x| x.parse().map(Self::Lib))
                })
                .unwrap_or_else(|_| Self::Ident(input.into())),
        };
        Ok(op)
    }
}

impl Teachable for ListOp {
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T> {
        match binding_expr {
            BindingExpr::Lambda(body) => AstNode::new(Self::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => AstNode::new(Self::Apply, [fun, arg]),
            BindingExpr::Var(index) => AstNode::leaf(Self::Var(index)),
            BindingExpr::Lib(ix, bound_value, body) => {
                AstNode::new(Self::Lib(ix), [bound_value, body])
            }
            BindingExpr::LibVar(ix) => AstNode::leaf(Self::LibVar(ix)),
        }
    }

    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
        let binding_expr = match node.as_parts() {
            (Self::Lambda, [body]) => BindingExpr::Lambda(body),
            (Self::Apply, [fun, arg]) => BindingExpr::Apply(fun, arg),
            (&Self::Var(index), []) => BindingExpr::Var(index),
            (Self::Lib(ix), [bound_value, body]) => BindingExpr::Lib(*ix, bound_value, body),
            (Self::LibVar(ix), []) => BindingExpr::LibVar(*ix),
            _ => return None,
        };
        Some(binding_expr)
    }

    fn list() -> Self {
        Self::List
    }
}

impl Printable for ListOp {
    fn precedence(&self) -> Precedence {
        match self {
            Self::Bool(_) | Self::Int(_) | Self::Var(_) | Self::Ident(_) | Self::LibVar(_) => 60,
            Self::List => 50,
            Self::Apply => 40,
            Self::Cons => 30,
            Self::If => 20,
            Self::Lambda | ListOp::Lib(_) => 10,
        }
    }

    fn print_naked<W: Write>(expr: &Expr<Self>, printer: &mut Printer<W>) -> fmt::Result {
        match (expr.0.operation(), expr.0.args()) {
            (&Self::Int(i), []) => {
                write!(printer.writer, "{i}")
            }
            (&Self::Bool(b), []) => {
                write!(printer.writer, "{b}")
            }
            (&Self::Ident(ident), []) => {
                let name: &str = ident.into();
                if name == "empty" {
                    printer.writer.write_str("[]")
                } else {
                    printer.writer.write_str(ident.into())
                }
            }
            (&Self::Cons, [head, tail]) => {
                printer.print(head)?;
                printer.writer.write_str(" : ")?;
                printer.print_in_context(tail, printer.ctx_precedence - 1) // cons is right-associative
            }
            (&Self::If, [cond, then, els]) => {
                printer.writer.write_str("if ")?;
                printer.print_in_context(cond, 0)?; // children do not need parens
                printer.writer.write_str(" then ")?;
                printer.print_in_context(then, 0)?;
                printer.writer.write_str(" else ")?;
                printer.print_in_context(els, 0)
            }
            (&Self::List, ts) => {
                let elem = |p: &mut Printer<W>, i: usize| {
                    p.print_in_context(&ts[i], 0) // children do not need parens
                };
                printer.in_brackets(|p| p.indented(|p| p.vsep(elem, ts.len(), ",")))
            }
            (op, _) => write!(printer.writer, "{op} ???"),
        }
    }
}

lazy_static! {
    pub(crate) static ref LIFT_LIB_REWRITES: &'static [Rewrite<AstNode<ListOp>, ()>] = {
        let rules = rewrite_rules! {
            // TODO: Check for captures of de Bruijn variables and re-index if necessary.
            // lift_lambda: "(lambda (lib ?x ?v ?e))" => "(lib ?x ?v (lambda ?e))";

            // Binding expressions
        //     lift_let_both: "(let ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
        //     lift_let_body: "(let ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
        //     lift_let_binding: "(let ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");

        //     lift_lib_both: "(lib ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
        //     lift_lib_body: "(lib ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
        //     lift_lib_binding: "(lib ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");
        //
        };

        // rules.extend([
        //     LiftLib::rewrite("lift_list", ListOp::List),
        //     LiftLib::rewrite("lift_if", ListOp::If),
        //     LiftLib::rewrite("lift_cons", ListOp::Cons),
        //     LiftLib::rewrite("lift_apply", ListOp::Apply),
        // ]);

        rules.leak()
    };
}
