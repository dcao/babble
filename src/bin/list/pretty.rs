use crate::lang::ListOp;
use babble::ast_node::Expr;
use babble::teachable::DeBruijnIndex;

/// Pretty-print `expr` into a string
#[must_use]
pub fn pretty(expr: &Expr<ListOp>) -> String {
    let mut printer = Printer::default();
    printer.print(expr);
    printer.buf
}

/// Operator precedence
type Precedence = u8;

/// Internal state of the pretty-printer
struct Printer {
    /// Buffer where result is accumulated
    buf: String,
    /// Bound variables in current scope
    bindings: Vec<String>,
    /// Precedence level of the context
    /// (determines whether the next printed expression should be parenthesized)
    ctx_precedence: Precedence,
    // Current indentation level
    indentation: usize,
}

impl Default for Printer {
    /// Create a fresh printer for the top-level expression
    fn default() -> Self {
        Self {
            buf: String::new(),
            bindings: vec![],
            ctx_precedence: 0,
            indentation: 0,
        }
    }
}

impl Printer {
    /// Operator precedence:
    /// determines whether the expression with head `op` will be parenthesized
    fn op_precedence(op: &ListOp) -> Precedence {
        match op {
            ListOp::Bool(_) | ListOp::Int(_) | ListOp::Var(_) | ListOp::Ident(_) => 60,
            ListOp::List => 50,
            ListOp::Apply | ListOp::Shift => 40,
            ListOp::Cons => 30,
            ListOp::If => 20,
            ListOp::Lambda | ListOp::Lib => 10,
        }
    }

    /// Print `expr` into the buffer at the current precedence level
    fn print(&mut self, expr: &Expr<ListOp>) {
        let op = expr.0.operation();
        let old_prec = self.ctx_precedence;
        let new_prec = Self::op_precedence(op);
        self.ctx_precedence = new_prec;
        if new_prec <= old_prec {
            self.in_parens(|p| p.print_naked(expr));
        } else {
            self.print_naked(expr);
        }
        self.ctx_precedence = old_prec;
    }

    /// Print `expr` into the buffer at precedence level `prec`:
    /// this function is used to implement associativity and bracket-like expressions,
    /// where the children should be printed at a lower precedence level than the expression itself
    fn print_in_context(&mut self, expr: &Expr<ListOp>, prec: Precedence) {
        let old_prec = self.ctx_precedence;
        self.ctx_precedence = prec;
        self.print(expr);
        self.ctx_precedence = old_prec;
    }

    /// Print `expr` into the buffer (without parentheses)
    fn print_naked(&mut self, expr: &Expr<ListOp>) {
        match (expr.0.operation(), expr.0.args()) {
            (&ListOp::Int(i), []) => self.buf.push_str(&format!("{}", i)),
            (&ListOp::Bool(b), []) => self.buf.push_str(&format!("{}", b)),
            (&ListOp::Var(i), []) => {
                let name = self.binding_at_index(i);
                self.buf.push_str(&name);
            }
            (&ListOp::Ident(ident), []) => {
                let name: &str = ident.into();
                if name == "empty" {
                    self.buf.push_str("[]");
                } else {
                    self.buf.push_str(ident.into());
                }
            }
            (&ListOp::Cons, [head, tail]) => {
                self.print(head);
                self.buf.push_str(" : ");
                self.print_in_context(tail, self.ctx_precedence - 1); // cons is right-associative
            }
            (&ListOp::If, [cond, then, els]) => {
                self.buf.push_str("if ");
                self.print_in_context(cond, 0); // children do not need parens
                self.buf.push_str(" then ");
                self.print_in_context(then, 0);
                self.buf.push_str(" else ");
                self.print_in_context(els, 0);
            }
            (&ListOp::Apply, [fun, arg]) => {
                self.print_in_context(fun, self.ctx_precedence - 1); // app is left-associative
                self.buf.push(' ');
                self.print(arg);
            }
            (&ListOp::Lambda, [body]) => {
                self.buf.push('λ');
                self.print_abstraction(body);
            }
            (&ListOp::Shift, [body]) => {
                match self.bindings.pop() {
                    None => {
                        panic!("Pretty printer encountered shift outside of abstraction");
                    }
                    Some(name) => {
                        self.print(body);
                        self.bindings.push(name);
                    }
                };
            }
            (&ListOp::Lib, [def, body]) => {
                self.with_binding("f", |p| {
                    let fresh_var = p.binding_at_index(DeBruijnIndex(0)); // the name of the latest binding
                    p.buf.push_str(&format!("lib {} =", fresh_var)); // print binding
                    p.indented(|p| {
                        p.new_line();
                        p.print_in_context(def, 0);
                    });
                    p.new_line();
                    p.buf.push_str("in");
                    p.indented(|p| {
                        p.new_line();
                        p.print_in_context(body, 0);
                    });
                });
            }
            (&ListOp::List, ts) => {
                let elem = |p: &mut Self, i: usize| {
                    p.print_in_context(&ts[i], 0); // children do not need parens
                };
                self.in_brackets(|p| p.indented(|p| p.vsep(elem, ts.len(), ",")));
            }
            _ => self.buf.push_str("???"),
        }
    }

    /// Print abstraction with body `body` without the "λ" symbol
    /// (this implements the syntactic sugar with nested abstractions)
    fn print_abstraction(&mut self, body: &Expr<ListOp>) {
        self.with_binding("x", |p| {
            let fresh_var = p.binding_at_index(DeBruijnIndex(0)); // the name of the latest binding
            p.buf.push_str(&format!("{} ", fresh_var)); // print binding
            if let (&ListOp::Lambda, [inner_body]) = (body.0.operation(), body.0.args()) {
                p.print_abstraction(inner_body); // syntactic sugar: no λ needed here
            } else {
                p.buf.push_str("-> "); // done with the sequence of bindings: print ->
                p.print_in_context(body, 0); // body doesn't need parens
            };
        });
    }

    /// Add new line with current indentation
    fn new_line(&mut self) {
        self.buf
            .push_str(&format!("\n{}", " ".repeat(self.indentation * 2)));
    }

    // Print f(i) for i in 0..n on separate lines
    fn vsep<T: Fn(&mut Self, usize)>(&mut self, f: T, n: usize, sep: &str) {
        for i in 0..n {
            f(self, i);
            if i < n - 1 {
                self.buf.push_str(sep);
                self.new_line();
            };
        }
    }

    /// print f() in parentheses
    fn in_parens<T: Fn(&mut Self)>(&mut self, f: T) {
        self.buf.push('(');
        f(self);
        self.buf.push(')');
    }

    /// print f() in brackets
    fn in_brackets<T: Fn(&mut Self)>(&mut self, f: T) {
        self.buf.push('[');
        f(self);
        self.buf.push(']');
    }

    /// print f() indented one more level
    fn indented<T: Fn(&mut Self)>(&mut self, f: T) {
        self.indentation += 1;
        f(self);
        self.indentation -= 1;
    }

    /// Named variable that corresponds to de Bruijn index `idx`
    fn binding_at_index(&self, idx: DeBruijnIndex) -> String {
        // It's annoying that I have to clone the result here,
        // but the borrow checker is unhappy with reference
        match self.bindings.get(self.bindings.len() - idx.0 - 1) {
            None => {
                panic!("Pretty printer encountered unbound variable {}", idx);
            }
            Some(name) => name.clone(),
        }
    }

    /// print f() inside the scope of a binder
    fn with_binding<T: Fn(&mut Self)>(&mut self, prefix: &str, f: T) {
        self.bindings
            .push(format!("{}{}", prefix, self.bindings.len()));
        f(self);
        self.bindings.pop();
    }
}
