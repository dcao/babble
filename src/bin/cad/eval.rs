use crate::lang::CAD;
use babble::ast_node::Expr;
use thiserror::Error;

#[derive(Clone, Debug)]
struct Context<'a> {
    args: Vec<Value<'a>>,
}

impl<'a> Context<'a> {
    fn new() -> Self {
        Self { args: Vec::new() }
    }

    fn with_arg(mut self, value: Value<'a>) -> Self {
        self.args.push(value);
        self
    }

    fn shift(mut self) -> Self {
        self.args.pop();
        self
    }

    fn get_index(&self, index: usize) -> &Value<'a> {
        &self.args[self.args.len() - (index + 1)]
    }

    fn eval(&self, expr: &'a Expr<CAD>) -> Result<Value<'a>, TypeError> {
        let result = match (expr.0.operation(), expr.0.args()) {
            _ => todo!(),
        };
        Ok(result)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[error("type mismatch")]
pub(crate) struct TypeError {
    expected: String,
}

#[derive(Debug, Clone)]
pub(crate) enum Value<'a> {
    Num(i32),
    Lambda(&'a Expr<CAD>),
}

pub(crate) fn eval(expr: &Expr<CAD>) -> Result<Value<'_>, TypeError> {
    Context::new().eval(expr)
}
