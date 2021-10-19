use crate::lang::ListOp;
use babble::ast_node::Expr;

pub fn pretty(expr: &Expr<ListOp>) -> String {
  let mut buf: String = String::new();
  print(&mut buf, expr, &mut vec![]);
  buf
}

fn print(buf: &mut String, expr: &Expr<ListOp>, bindings: &mut Vec<String>) {
  match (expr.0.operation(), expr.0.args()) {
    (&ListOp::Int(i), []) => buf.push_str(&format!("{}", i)),
    (&ListOp::Bool(b), []) => buf.push_str(&format!("{}", b)),
    (&ListOp::Index(i), []) => buf.push_str(&bindings[bindings.len() - i.0 - 1]),
    (&ListOp::Ident(ident), []) => {
      let name: &str = ident.into();
      if name == "empty" {
        buf.push_str("[]");
      } else {
        buf.push_str(ident.into());
      }
    },
    (&ListOp::Cons, [head, tail]) => {
      print(buf, head, bindings);
      buf.push_str(" : ");
      print(buf, tail, bindings);
    },
    (&ListOp::Apply, [fun, arg]) => {
      print(buf, fun, bindings);
      buf.push(' ');
      print(buf, arg, bindings);
    },
    (&ListOp::Lambda, [body]) => {
      let fresh_name = format!("x{}", bindings.len());
      buf.push_str(&format!("\\{} -> ", &fresh_name));
      bindings.push(fresh_name);      
      print(buf, body, bindings);
    },
    (&ListOp::Lib, [name, def, body]) => {
      buf.push_str("lib ");
      print(buf, name, bindings);
      buf.push_str(" = ");
      print(buf, def, bindings);
      buf.push_str(" in\n");
      print(buf, body, bindings);
    }
    (&ListOp::List, ts) => {
      for t in ts {
        print(buf, t, bindings);
        buf.push('\n');
      }
    },
    _ => buf.push_str("expression!")
  }
}
