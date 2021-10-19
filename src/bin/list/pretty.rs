use crate::lang::ListOp;
use babble::ast_node::Expr;

pub fn pretty(expr: &Expr<ListOp>) -> String {  
  let mut printer = Printer {buf : String::new(), bindings : vec![]};
  printer.print(expr);
  printer.buf
}

struct Printer {
  buf: String,
  bindings: Vec<String>
}

impl Printer {
  fn print(&mut self, expr: &Expr<ListOp>) {
    match (expr.0.operation(), expr.0.args()) {
      (&ListOp::Int(i), []) => self.buf.push_str(&format!("{}", i)),
      (&ListOp::Bool(b), []) => self.buf.push_str(&format!("{}", b)),
      (&ListOp::Index(i), []) => self.buf.push_str(&self.bindings[self.bindings.len() - i.0 - 1]),
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
      (&ListOp::Apply, [fun, arg]) => {
        self.print(fun);
        self.buf.push(' ');
        self.print(arg);
      },
      (&ListOp::Lambda, [body]) => {
        let fresh_name = format!("x{}", self.bindings.len());
        self.buf.push_str(&format!("λ{} -> ", &fresh_name));
        self.bindings.push(fresh_name);      
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
