use crate::lang::ListOp;
use babble::teachable::DeBruijnIndex;
use babble::ast_node::Expr;

/// Pretty-print `expr` into a string
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
  /// Number of bound variables in current scope
  n_bindings: usize,
  /// Precedence level of the context 
  /// (determines whether the next printed expression should be parenthesized)
  ctx_precedence: Precedence,
  // Current indentation level
  indentation: usize
}

impl Default for Printer
{
    /// Create a fresh printer for the top-level expression
    fn default() -> Self {
        Self {buf : String::new(), n_bindings : 0, ctx_precedence : 0, indentation : 0 }
    }
}

impl Printer {
  /// Named variable that corresponds to DeBruijn index `idx`
  fn binding_at_index(&self, idx: DeBruijnIndex) -> String {
    format!("x{}", self.n_bindings - idx.0 - 1)    
  }

  /// Operator precedence:
  /// determines whether the expression with head `op` will be parenthesized
  fn op_precedence(op: &ListOp) -> Precedence {
      match op {
        ListOp::Bool(_) | ListOp::Int(_) | ListOp::Index(_) | ListOp::Ident(_) => 60,
        ListOp::List => 50,
        ListOp::Apply => 40,
        ListOp::Cons => 30,
        ListOp::If => 20,
        ListOp::Lambda | ListOp::Let | ListOp::Lib => 10,
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
      (&ListOp::Index(i), []) => {
        let name = self.binding_at_index(i);
        self.buf.push_str(&name);
      },
      (&ListOp::Ident(ident), []) => {
        let name: &str = ident.into();
        if name == "empty" {
          self.buf.push_str("[]");
        } else {
          self.buf.push_str(ident.into());
        }
      },
      (&ListOp::Cons, [head, tail]) => {
        self.print(head);
        self.buf.push_str(" : ");
        self.print_in_context(tail, self.ctx_precedence - 1); // cons is right-associative
      },
      (&ListOp::If, [cond, then, els]) => {
        self.buf.push_str("if ");
        self.print_in_context(cond, 0); // children do not need parens
        self.buf.push_str(" then ");
        self.print_in_context(then, 0);
        self.buf.push_str(" else ");
        self.print_in_context(els, 0);
      },      
      (&ListOp::Apply, [fun, arg]) => {
        self.print_in_context(fun, self.ctx_precedence - 1); // app is left-associative
        self.buf.push(' ');
        self.print(arg);
      },
      (&ListOp::Lambda, [body]) => {
        self.buf.push('λ');
        self.print_abstraction(body);
      },
      (&ListOp::Let, [name, def, body]) => {
        self.buf.push_str("let ");
        self.print(name);
        self.buf.push_str(" =");
        self.indented(|p| {
          p.new_line();
          p.print_in_context(def, 0);
        });        
        self.new_line();
        self.buf.push_str("in");
        self.indented(|p| {
          p.new_line();
          p.print_in_context(body, 0);
        });        
      }      
      (&ListOp::Lib, [name, def, body]) => {
        self.buf.push_str("lib ");
        self.print(name);
        self.buf.push_str(" =");
        self.indented(|p| {
          p.new_line();
          p.print_in_context(def, 0);
        });        
        self.new_line();
        self.buf.push_str("in");
        self.indented(|p| {
          p.new_line();
          p.print_in_context(body, 0);
        });        
      }
      (&ListOp::List, ts) => {
        let elem = |p: &mut Self, i: usize| {          
            p.print_in_context(&ts[i], 0); // children do not need parens            
        };        
        self.in_brackets(|p1|
          p1.indented(|p2|
            p2.vsep(elem, ts.len(), ",")
          )
        );
      },
      _ => self.buf.push_str("???")
    }
  }

  /// Print abstraction with body `body` without the "λ" symbol
  /// (this implements the syntactic sugar with nested abstractions)
  fn print_abstraction(&mut self, body: &Expr<ListOp>) {
    self.n_bindings += 1;                                     // one more binding in scope
    let fresh_var = self.binding_at_index(DeBruijnIndex(0));  // the name of the latest binding
    self.buf.push_str(&format!("{} ", fresh_var));            // print binding
    if let (&ListOp::Lambda, [inner_body]) = (body.0.operation(), body.0.args()) {
        self.print_abstraction(inner_body);                    // syntactic sugar: no λ needed here
    } else {
        self.buf.push_str("-> ");                              // done with the sequence of bindings: print ->
        self.print_in_context(body, 0);                        // body doesn't need parens
    }
    self.n_bindings -= 1;                                      // one fewer binding in scope
  }

  /// Add new line with current indentation
  fn new_line(&mut self) {
    self.buf.push_str(&format!("\n{}", " ".repeat(self.indentation * 2)));    
  }

  // Print f(i) for i in 0..n on separate lines
  fn vsep<T : Fn(&mut Self, usize)>(&mut self, f: T, n: usize, sep: &str) {    
    for i in 0 .. n {
      f(self, i);
      if i < n - 1 {
        self.buf.push_str(sep);
        self.new_line();
      };
    };
  }

  /// print f() in parentheses
  fn in_parens<T : Fn(&mut Self)>(&mut self, f: T) {
    self.buf.push('(');
    f(self);
    self.buf.push(')') ;
  }

  /// print f() in brackets
  fn in_brackets<T : Fn(&mut Self)>(&mut self, f: T) {
    self.buf.push('[');
    f(self);
    self.buf.push(']') ;
  }

  /// print f() indented one more level
  fn indented<T : Fn(&mut Self)>(&mut self, f: T) {
    self.indentation += 1;
    f(self);
    self.indentation -= 1;
  }

}
