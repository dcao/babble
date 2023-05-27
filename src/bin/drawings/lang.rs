//! The AST defining the drawings language from [CogSci dataset](https://sites.google.com/view/language-abstraction/home).

use babble::{
    ast_node::{Arity, AstNode, Expr, Precedence, Printable, Printer},
    learn::{LibId, ParseLibIdError},
    teachable::{BindingExpr, DeBruijnIndex, Teachable},
};
use ordered_float::NotNan;
use std::{
    fmt::{self, Debug, Display, Formatter, Write},
    num::ParseIntError,
    str::FromStr,
};

/// The operations/AST nodes of the "drawings" language.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Drawing {
    /// Common lambda calculus constructs:
    /// A de Bruijn-indexed variable.
    Var(DeBruijnIndex),
    /// Create an anonymous, de Bruijn-indexed function.
    Lambda,
    /// A reference to a lib fn
    LibVar(LibId),
    /// Bind a lib fn within an expression.
    Lib(LibId),
    /// Apply a function to an argument.
    Apply,
    /// A top-level list of programs.
    List,
    /// Drawing-specific constructs:
    /// Number pi:
    Pi,
    /// A floating-point constant.
    Float(NotNan<f64>),
    /// Arithmetic operations:    
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Max,
    Sin,
    Cos,
    Tan,
    /// A unit circle.
    Circle,
    /// A unit-length horizontal line.
    Line,
    // A unit-length square.
    Square,
    // A scaled rectangle.
    Rect,
    /// An empty shape???
    Empty,
    /// A transformation matrix parametrized by scale, rotation, x-shift and y-shift.
    Matrix,
    /// Apply transformation matrix to a shape.
    Transform,
    /// Repeat a shape a number of times, applying a transform in between.
    Repeat,
    /// connect two shapes.
    Connect,
}

impl Debug for Drawing {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl Arity for Drawing {
    fn min_arity(&self) -> usize {
        match self {
            Self::Var(_)
            | Self::LibVar(_)
            | Self::Pi
            | Self::Float(_)
            | Self::Empty
            | Self::Circle
            | Self::Line
            | Self::Square => 0,
            Self::Lambda | Self::List | Self::Sin | Self::Cos | Self::Tan => 1,
            Self::Apply
            | Self::Lib(_)
            | Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Pow
            | Self::Max
            | Self::Rect
            | Self::Transform
            | Self::Connect => 2,
            Self::Repeat => 3,
            Self::Matrix => 4,
        }
    }

    fn max_arity(&self) -> Option<usize> {
        match self {
            Self::List => None,
            other => Some(other.min_arity()),
        }
    }
}

impl Display for Drawing {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(i) => write!(f, "{i}"),
            Self::Lambda => f.write_str("λ"),
            Self::LibVar(ix) => write!(f, "{ix}"),
            Self::Lib(ix) => write!(f, "lib-{ix}"),
            Self::Apply => f.write_str("@"),
            Self::List => f.write_str(":"),
            Self::Pi => f.write_str("π"),
            Self::Float(g) => Display::fmt(g, f),
            Self::Add => f.write_str("+"),
            Self::Sub => f.write_str("-"),
            Self::Mul => f.write_str("*"),
            Self::Div => f.write_str("/"),
            Self::Pow => f.write_str("^"),
            Self::Max => f.write_str("max"),
            Self::Sin => f.write_str("sin"),
            Self::Cos => f.write_str("cos"),
            Self::Tan => f.write_str("tan"),
            Self::Circle => f.write_str("c"),
            Self::Line => f.write_str("l"),
            Self::Square => f.write_str("r"),
            Self::Rect => f.write_str("r_s"),
            Self::Empty => f.write_str("empt"),
            Self::Matrix => f.write_str("M"),
            Self::Transform => f.write_str("T"),
            Self::Repeat => f.write_str("repeat"),
            Self::Connect => f.write_str("C"),
        }
    }
}

impl FromStr for Drawing {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let kind = match s {
            "lambda" | "λ" => Self::Lambda,
            "apply" | "@" => Self::Apply,
            "pi" | "π" => Self::Pi,
            "+" => Self::Add,
            "-" => Self::Sub,
            "/" => Self::Div,
            "*" => Self::Mul,
            "pow" | "^" => Self::Pow,
            "max" => Self::Max,
            "sin" => Self::Sin,
            "cos" => Self::Cos,
            "tan" => Self::Tan,
            "c" => Self::Circle,
            "l" => Self::Line,
            "r" => Self::Square,
            "r_s" => Self::Rect,
            "empt" => Self::Empty,
            "M" => Self::Matrix,
            "T" => Self::Transform,
            "repeat" => Self::Repeat,
            "C" => Self::Connect,
            ":" => Self::List,
            _ => {
                if let Ok(index) = s.parse::<DeBruijnIndex>() {
                    Self::Var(index)
                } else if let Ok(lv) = s.parse::<LibId>() {
                    Self::LibVar(lv)
                } else if let Ok(lv) = s
                    .strip_prefix("lib-")
                    .ok_or(ParseLibIdError::NoLeadingL)
                    .and_then(str::parse)
                {
                    Self::Lib(lv)
                } else if let Ok(f) = s.parse::<NotNan<f64>>() {
                    Self::Float(f)
                } else {
                    panic!("how to parse {}?", s)
                }
            }
        };
        Ok(kind)
    }
}

impl Teachable for Drawing {
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T> {
        match binding_expr {
            BindingExpr::Lambda(body) => AstNode::new(Self::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => AstNode::new(Self::Apply, [fun, arg]),
            BindingExpr::Var(index) => AstNode::leaf(Self::Var(index)),
            BindingExpr::LibVar(ix) => AstNode::leaf(Self::LibVar(ix)),
            BindingExpr::Lib(ix, bound_value, body) => {
                AstNode::new(Self::Lib(ix), [bound_value, body])
            }
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

impl Printable for Drawing {
    fn precedence(&self) -> Precedence {
        match self {
            Self::Var(_)
            | Self::LibVar(_)
            | Self::Pi
            | Self::Float(_)
            | Self::Empty
            | Self::Circle
            | Self::Line
            | Self::Square => 60,
            Self::List => 50,
            Self::Apply
            | Self::Max
            | Self::Sin
            | Self::Cos
            | Self::Tan
            | Self::Rect
            | Self::Matrix
            | Self::Transform
            | Self::Repeat
            | Self::Connect => 40,
            Self::Pow => 35,
            Self::Mul | Self::Div => 30,
            Self::Add | Self::Sub => 20,
            Self::Lambda | Self::Lib(_) => 10,
        }
    }

    fn print_naked<W: Write>(expr: &Expr<Self>, printer: &mut Printer<W>) -> fmt::Result {
        match (expr.0.operation(), expr.0.args()) {
            (&Self::Pi, []) => printer.writer.write_str("π"),
            (&Self::Float(f), []) => {
                write!(printer.writer, "{f}")
            }
            (&Self::Circle, []) => printer.writer.write_str("c"),
            (&Self::Line, []) => printer.writer.write_str("l"),
            (&Self::Square, []) => printer.writer.write_str("r"),
            (&Self::Empty, []) => printer.writer.write_str("empt"),
            (&Self::Rect, [w, h]) => {
                printer.writer.write_str("r_s ")?;
                printer.print(w)?;
                printer.writer.write_str(" ")?;
                printer.print(h)
            }
            (&Self::Add, [l, r]) => {
                printer.print(l)?;
                printer.writer.write_str(" + ")?;
                printer.print(r)
            }
            (&Self::Sub, [l, r]) => {
                printer.print(l)?;
                printer.writer.write_str(" - ")?;
                printer.print(r)
            }
            (&Self::Mul, [l, r]) => {
                printer.print(l)?;
                printer.writer.write_str(" * ")?;
                printer.print(r)
            }
            (&Self::Div, [l, r]) => {
                printer.print(l)?;
                printer.writer.write_str(" / ")?;
                printer.print(r)
            }
            (&Self::Pow, [l, r]) => {
                printer.print(l)?;
                printer.writer.write_str(" ^ ")?;
                printer.print(r)
            }
            (&Self::Max, [l, r]) => {
                printer.writer.write_str("max ")?;
                printer.print(l)?;
                printer.writer.write_str(" ")?;
                printer.print(r)
            }
            (&Self::Sin, [x]) => {
                printer.writer.write_str("sin ")?;
                printer.print(x)
            }
            (&Self::Cos, [x]) => {
                printer.writer.write_str("cos ")?;
                printer.print(x)
            }
            (&Self::Tan, [x]) => {
                printer.writer.write_str("tan ")?;
                printer.print(x)
            }
            (&Self::Matrix, [scale, rot, x, y]) => {
                printer.writer.write_str("M ")?;
                printer.print(scale)?;
                printer.writer.write_str(" ")?;
                printer.print(rot)?;
                printer.writer.write_str(" ")?;
                printer.print(x)?;
                printer.writer.write_str(" ")?;
                printer.print(y)
            }
            (&Self::Transform, [x, m]) => {
                printer.writer.write_str("T ")?;
                printer.print(x)?;
                printer.writer.write_str(" ")?;
                printer.print(m)
            }
            (&Self::Repeat, [x, n, t]) => {
                printer.writer.write_str("repeat ")?;
                printer.print(x)?;
                printer.writer.write_str(" ")?;
                printer.print(n)?;
                printer.writer.write_str(" ")?;
                printer.print(t)
            }
            (&Self::Connect, [x, y]) => {
                printer.writer.write_str("C ")?;
                printer.print(x)?;
                printer.writer.write_str(" ")?;
                printer.print(y)
            }
            (&Self::List, ts) => {
                let elem = |p: &mut Printer<W>, i: usize| {
                    p.print_in_context(&ts[i], 0) // children do not need parens
                };
                printer.in_brackets(|p| p.indented(|p| p.vsep(elem, ts.len(), ",")))
            }
            (op, _) => write!(printer.writer, "{op}"),
        }
    }
}
