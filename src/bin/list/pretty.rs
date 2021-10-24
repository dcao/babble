use crate::lang::ListOp;
use babble::teachable::DeBruijnIndex;
use babble::ast_node::Expr;

pub fn pretty(expr: &Expr<ListOp>) -> String {  
  let mut printer = Printer::top();
  printer.print(expr);
  printer.buf
}

type Precedence = u8;

struct Printer {
  buf: String,
  n_bindings: usize,
  ctx_precedence: Precedence
}

impl Printer {
  fn top() -> Self {
    Printer {buf : String::new(), n_bindings : 0, ctx_precedence : 0}
  }

  fn binding_at_index(&self, idx: DeBruijnIndex) -> String {
    format!("x{}", self.n_bindings - idx.0 - 1)    
  }

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

  fn print(&mut self, expr: &Expr<ListOp>) {
    let op = expr.0.operation();
    let old_prec = self.ctx_precedence;
    let new_prec = Self::op_precedence(op);
    self.ctx_precedence = new_prec; // Self::inner_precedence(op);
    if new_prec <= old_prec { self.buf.push('('); }      
    self.print_inner(expr);
    if new_prec <= old_prec { self.buf.push(')'); }
    self.ctx_precedence = old_prec;      
  }  

  fn print_inner(&mut self, expr: &Expr<ListOp>) {
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
        self.print_in_lambda(body);
      },
      (&ListOp::Lib, [name, def, body]) => {
        self.buf.push_str("lib ");
        self.print(name);
        self.buf.push_str(" = ");
        self.print_in_context(def, 0);
        self.buf.push_str(" in\n");
        self.print(body);
      }
      (&ListOp::List, ts) => {
        self.buf.push_str("[\n");
        for t in ts {
          self.print_in_context(t, 0); // children do not need parens
          self.buf.push_str(",\n");
        }
        self.buf.push(']');
      },
      _ => self.buf.push_str("expression!")
    }
  }

  fn print_in_lambda(&mut self, expr: &Expr<ListOp>) {
    self.n_bindings += 1;
    let fresh_var = self.binding_at_index(DeBruijnIndex(0));        
    self.buf.push_str(&format!("{} ", fresh_var));
    if let (&ListOp::Lambda, [body]) = (expr.0.operation(), expr.0.args()) {
        self.print_in_lambda(body);
    } else {
        self.buf.push_str("-> ");
        self.print_in_context(expr, 0); // body doesn't need parens
    }
    self.n_bindings -= 1;    
  }

  fn print_in_context(&mut self, expr: &Expr<ListOp>, prec: Precedence) {
    let old_prec = self.ctx_precedence;
    self.ctx_precedence = prec;
    self.print(expr);
    self.ctx_precedence = old_prec;
  }
}
