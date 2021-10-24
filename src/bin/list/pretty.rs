use crate::lang::ListOp;
use babble::teachable::DeBruijnIndex;
use babble::ast_node::Expr;

pub fn pretty(expr: &Expr<ListOp>) -> String {  
  let mut printer = Printer {buf : String::new(), n_bindings : 0};
  printer.print(expr);
  printer.buf
}

struct Printer {
  buf: String,
  n_bindings: usize
}

impl Printer {
  fn binding_at_index(&self, idx: DeBruijnIndex) -> String {
    format!("x{}", self.n_bindings - idx.0 - 1)    
  }

  fn gen_fresh_binding(&mut self) {
    self.n_bindings += 1;
  }

  fn print(&mut self, expr: &Expr<ListOp>) {
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
        self.print(tail);
      },
      (&ListOp::If, [cond, then, els]) => {
        self.buf.push_str("if ");
        self.print(cond);
        self.buf.push_str(" then ");
        self.print(then);
        self.buf.push_str(" else ");
        self.print(els);
      },
      (&ListOp::Apply, [fun, arg]) => {
        self.print(fun);
        self.buf.push(' ');
        self.print(arg);
      },
      (&ListOp::Lambda, [body]) => {
        self.gen_fresh_binding();
        let fresh_var = self.binding_at_index(DeBruijnIndex(0));        
        self.buf.push_str(&format!("λ{} -> ", fresh_var));
        self.print(body);
      },
      (&ListOp::Lib, [name, def, body]) => {
        self.buf.push_str("lib ");
        self.print(name);
        self.buf.push_str(" = ");
        self.print(def);
        self.buf.push_str(" in\n");
        self.print(body);
      }
      (&ListOp::List, ts) => {
        for t in ts {
          self.print(t);
          self.buf.push('\n');
        }
      },
      _ => self.buf.push_str("expression!")
    }
  }
}
