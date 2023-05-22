use std::fmt::{self, Display, Write};

use crate::{
    ast_node::Expr,
    teachable::{BindingExpr, Teachable},
};

/// A wrapper around [`&'a Expr<Op>`] whose [`Display`] impl pretty-prints the
/// expression.
#[derive(Debug, Clone, Copy)]
pub struct Pretty<'a, Op>(pub &'a Expr<Op>);

impl<Op> Display for Pretty<'_, Op>
where
    Op: Printable + Teachable,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Printer::new(f).print(self.0)
    }
}

/// Operator precedence
pub type Precedence = u8;

/// A language whose expressions can be pretty-printed.
/// This is used for printing language-specific operations,
/// whereas the printing of binding expressions is implemented inside Printer.
pub trait Printable
where
    Self: Sized,
{
    /// Operator precedence:
    /// determines whether the expression with head `op` will be parenthesized
    fn precedence(&self) -> Precedence;

    /// Print `expr` into the printer's buffer without parentheses
    ///
    /// # Errors
    ///
    /// This function returns an error if the underlying writer fails, or if the
    /// expression is malformed
    fn print_naked<W: Write>(expr: &Expr<Self>, printer: &mut Printer<W>) -> fmt::Result;
}

/// Internal state of the pretty-printer
#[derive(Debug)]
pub struct Printer<W: Write> {
    /// Buffer where result is accumulated
    pub writer: W,
    /// Precedence level of the context
    /// (determines whether the next printed expression should be parenthesized)
    pub ctx_precedence: Precedence,
    /// Bound variables in current scope
    bindings: Vec<String>,
    // Current indentation level
    indentation: usize,
}

impl<W: Write> Printer<W> {
    /// Create a fresh printer for the top-level expression
    fn new(writer: W) -> Self {
        Self {
            writer,
            bindings: vec![],
            ctx_precedence: 0,
            indentation: 0,
        }
    }

    /// Print `expr` into the buffer at the current precedence level
    ///
    /// # Errors
    ///
    /// This function returns an error if the underlying writer fails, or if the
    /// expression is malformed
    pub fn print<Op: Printable + Teachable>(&mut self, expr: &Expr<Op>) -> fmt::Result {
        let op = expr.0.operation();
        let old_prec = self.ctx_precedence;
        let new_prec = op.precedence();
        self.ctx_precedence = new_prec;
        if new_prec <= old_prec {
            self.in_parens(|p| p.print_naked(expr))?;
        } else {
            self.print_naked(expr)?;
        }
        self.ctx_precedence = old_prec;
        Ok(())
    }

    /// Print `expr` into the buffer at precedence level `prec`: this function
    /// is used to implement associativity and bracket-like expressions, where
    /// the children should be printed at a lower precedence level than the
    /// expression itself
    ///
    /// # Errors
    ///
    /// This function returns an error if the underlying writer fails, or if the
    /// expression is malformed
    pub fn print_in_context<Op: Printable + Teachable>(
        &mut self,
        expr: &Expr<Op>,
        prec: Precedence,
    ) -> fmt::Result {
        let old_prec = self.ctx_precedence;
        self.ctx_precedence = prec;
        self.print(expr)?;
        self.ctx_precedence = old_prec;
        Ok(())
    }

    /// Print `expr` into the buffer (without parentheses)
    ///
    /// # Errors
    ///
    /// This function returns an error if the underlying writer fails, or if the
    /// expression is malformed.
    fn print_naked<Op: Printable + Teachable>(&mut self, expr: &Expr<Op>) -> fmt::Result {
        match expr.0.as_binding_expr() {
            Some(binding_expr) => {
                match binding_expr {
                    BindingExpr::Lambda(body) => {
                        self.writer.write_char('λ')?;
                        self.print_abstraction(body)
                    }
                    BindingExpr::Apply(fun, arg) => {
                        self.print_in_context(fun, self.ctx_precedence - 1)?; // app is left-associative
                        self.writer.write_char(' ')?;
                        self.print(arg)
                    }
                    BindingExpr::Var(index) => {
                        let name = self
                            .bindings
                            .get(self.bindings.len() - 1 - index.0)
                            .expect("unbound variable");
                        self.writer.write_str(name)
                    }
                    BindingExpr::Lib(ix, def, body) => {
                        self.with_binding("f", |p| {
                            write!(p.writer, "lib {ix} =")?; // print binding

                            p.indented(|p| {
                                p.new_line()?;
                                p.print_in_context(def, 0)
                            })?;
                            p.new_line()?;
                            p.writer.write_str("in")?;
                            p.indented(|p| {
                                p.new_line()?;
                                p.print_in_context(body, 0)
                            })
                        })
                    }
                    BindingExpr::LibVar(ix) => {
                        write!(self.writer, "{ix}")
                    }
                    BindingExpr::Shift(body) => match self.bindings.pop() {
                        None => {
                            panic!("Pretty printer encountered shift outside of abstraction")
                        }
                        Some(name) => {
                            self.print(body)?;
                            self.bindings.push(name);
                            Ok(())
                        }
                    },
                }
            }
            None => {
                // This is not a binding expr: use language-specific printing
                Op::print_naked(expr, self)
            }
        }
    }

    /// Print abstraction with body `body` without the "λ" symbol
    /// (this implements the syntactic sugar with nested abstractions)
    ///
    /// # Errors
    ///
    /// This function returns an error if the underlying writer fails, or if the
    /// expression is malformed.
    fn print_abstraction<Op: Printable + Teachable>(&mut self, body: &Expr<Op>) -> fmt::Result {
        self.with_binding("x", |p| {
            let fresh_var = p.bindings.last().unwrap(); // the name of the latest binding
            write!(p.writer, "{fresh_var} ")?; // print binding
            if let Some(BindingExpr::Lambda(inner_body)) = body.0.as_binding_expr() {
                p.print_abstraction(inner_body) // syntactic sugar: no λ needed here
            } else {
                p.writer.write_str("-> ")?; // done with the sequence of bindings: print ->
                p.print_in_context(body, 0) // body doesn't need parens
            }
        })
    }

    /// Add new line with current indentation
    ///
    /// # Errors
    ///
    /// This function returns an error if the underlying writer fails.
    pub fn new_line(&mut self) -> fmt::Result {
        write!(self.writer, "\n{}", " ".repeat(self.indentation * 2))
    }

    /// Print f(i) for i in 0..n on separate lines
    ///
    /// # Errors
    ///
    /// This function returns an error if the underlying writer fails, or if the
    /// closure returns an error.
    pub fn vsep<T: Fn(&mut Self, usize) -> fmt::Result>(
        &mut self,
        f: T,
        n: usize,
        sep: &str,
    ) -> fmt::Result {
        for i in 0..n {
            f(self, i)?;
            if i < n - 1 {
                self.writer.write_str(sep)?;
                self.new_line()?;
            };
        }
        Ok(())
    }

    /// print f() in parentheses
    ///
    /// # Errors
    ///
    /// This function returns an error if the underlying writer fails, or if the
    /// closure returns an error.
    pub fn in_parens<T: Fn(&mut Self) -> fmt::Result>(&mut self, f: T) -> fmt::Result {
        self.writer.write_char('(')?;
        f(self)?;
        self.writer.write_char(')')
    }

    /// print f() in brackets
    ///
    /// # Errors
    ///
    /// This function returns an error if the underlying writer fails, or if the
    /// closure returns an error.
    pub fn in_brackets<T: Fn(&mut Self) -> fmt::Result>(&mut self, f: T) -> fmt::Result {
        self.writer.write_char('[')?;
        f(self)?;
        self.writer.write_char(']')
    }

    /// print f() indented one more level
    ///
    /// # Errors
    ///
    /// This function returns an error if the underlying writer fails, or if the
    /// closure returns an error.
    pub fn indented<T: Fn(&mut Self) -> fmt::Result>(&mut self, f: T) -> fmt::Result {
        self.indentation += 1;
        f(self)?;
        self.indentation -= 1;
        Ok(())
    }

    /// print f() inside the scope of a binder
    ///
    /// # Errors
    ///
    /// This function returns an error if the underlying writer fails, or if the
    /// closure returns an error.
    fn with_binding<T: Fn(&mut Self) -> fmt::Result>(&mut self, prefix: &str, f: T) -> fmt::Result {
        self.bindings
            .push(format!("{prefix}{}", self.bindings.len()));
        f(self)?;
        self.bindings.pop();
        Ok(())
    }
}
