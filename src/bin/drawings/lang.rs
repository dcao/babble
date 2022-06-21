//! The AST defining the drawings language from CogSci dataset.

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
  /// A floating-point constant.
  Float(NotNan<f64>),
  /// A de Bruijn-indexed variable.
  Var(DeBruijnIndex),
  /// A reference to a lib fn
  LibVar(LibId),
  /// A transformation matrix.
  Matrix,
  Mul,
  Div,
  Tan,
  /// A unit circle.
  Circle,
  /// A unit line.
  Line,
  /// Transform a picture.
  Transform,
  /// Scale a picture.
  Connect,
  List,
  /// Apply a function to an argument.
  Apply,
  /// Create an anonymous, de Bruijn-indexed function.
  Lambda,
  /// Bind a lib fn within an expression.
  Lib(LibId),
  /// Shift indices.
  Shift,
}

impl Debug for Drawing {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      write!(f, "{}", self)
  }
}

impl Arity for Drawing {
  fn min_arity(&self) -> usize {
      match self {
          | Self::Float(_)
          | Self::Var(_)
          | Self::Circle
          | Self::Line
          | Self::LibVar(_) => 0,
          Self::Lambda 
          | Self::Shift 
          | Self::Tan 
          | Self::List => 1,
          Self::Mul
          | Self::Div
          | Self::Transform
          | Self::Apply
          | Self::Connect 
          | Self::Lib(_) => 2,
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
          Self::Float(g) => Display::fmt(g, f),
          Self::Var(i) => write!(f, "{}", i),
          Self::Circle => f.write_str("c"),
          Self::Line => f.write_str("l"),
          Self::Connect => f.write_str("C"),
          Self::Apply => f.write_str("@"),
          Self::Lambda => f.write_str("λ"),
          Self::Lib(ix) => write!(f, "lib {}", ix),
          Self::LibVar(ix) => write!(f, "{}", ix),
          Self::Mul => f.write_str("*"),
          Self::Div => f.write_str("/"),
          Self::Tan => f.write_str("tan"),
          Self::Transform => f.write_str("T"),
          Self::Matrix => f.write_str("M"),
          Self::List => f.write_str(":"),
          Self::Shift => f.write_str("shift"),          
      }
  }
}

impl FromStr for Drawing {
  type Err = ParseIntError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
      let kind = match s {
          "c" => Self::Circle,
          "l" => Self::Line,
          "/" => Self::Div,
          "*" => Self::Mul,
          "tan" => Self::Tan,
          "lambda" | "λ" => Self::Lambda,
          "apply" | "@" => Self::Apply,
          "shift" => Self::Shift,
          "T" => Self::Transform,
          "M" => Self::Matrix,
          "C" => Self::Connect,                          
          _ => {
              if let Ok(index) = s.parse::<DeBruijnIndex>() {
                  Self::Var(index)
              } else if let Ok(lv) = s.parse::<LibId>() {
                  Self::LibVar(lv)
              } else if let Ok(lv) = s
                  .strip_prefix("lib ")
                  .ok_or(ParseLibIdError::NoLeadingL)
                  .and_then(|x| x.parse())
              {
                  Self::Lib(lv)
              } else if let Ok(f) = s.parse::<NotNan<f64>>() {
                  Self::Float(f)
              } else {
                  panic!("how to parse")
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
          BindingExpr::Shift(body) => AstNode::new(Self::Shift, [body]),
      }
  }

  fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
      let binding_expr = match node.as_parts() {
          (Self::Lambda, [body]) => BindingExpr::Lambda(body),
          (Self::Apply, [fun, arg]) => BindingExpr::Apply(fun, arg),
          (&Self::Var(index), []) => BindingExpr::Var(index),
          (Self::Lib(ix), [bound_value, body]) => BindingExpr::Lib(*ix, bound_value, body),
          (Self::LibVar(ix), []) => BindingExpr::LibVar(*ix),
          (Self::Shift, [body]) => BindingExpr::Shift(body),
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
          Self::Float(_)
          | Self::Var(_)
          | Self::LibVar(_)
          | Self::Circle
          | Self::Line => 60,
          Self::List => 50,
          Self::Tan | Self::Matrix | Self::Transform | Self::Connect | Self::Apply | Self::Shift => 40,
          Self::Mul | Self::Div => 30,
          Self::Lambda | Self::Lib(_) => 10,
        
      }
  }

  fn print_naked<W: Write>(expr: &Expr<Self>, printer: &mut Printer<W>) -> fmt::Result {
      match (expr.0.operation(), expr.0.args()) {
          (&Self::Float(f), []) => {
              write!(printer.writer, "{}", f)
          }
          (&Self::Circle, []) => printer.writer.write_str("c"),
          (&Self::Line, []) => printer.writer.write_str("l"),
          (&Self::Mul, [x, y]) => {
              printer.print(x)?;
              printer.writer.write_str(" * ")?;
              printer.print(y)
          }
          (&Self::Div, [x, y]) => {
              printer.print(x)?;
              printer.writer.write_str(" / ")?;
              printer.print(y)
          }
          (&Self::Tan, [x]) => {
              printer.writer.write_str("tan ")?;
              printer.print(x)
          }
          (&Self::Transform, [x, m]) => {
              printer.writer.write_str("T ")?;
              printer.print(x)?;
              printer.writer.write_str(" ")?;
              printer.print(m)
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
          (op, _) => write!(printer.writer, "{}", op),
      }
  }
}