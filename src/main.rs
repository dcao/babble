use ast_node::AstNode;
use egg::{rewrite, Runner};
use my_lang::MyLang;

mod ast_node;
mod my_lang;

fn main() {
    env_logger::init();
    let expr = "(+ (+ (+ 0 0) 0))".parse().unwrap();
    let rewrites = [rewrite!("bad_rewrite"; "(+ ?x0 0)" => "?x0")];
    let runner: Runner<AstNode<MyLang>, ()> = Runner::default().with_expr(&expr);
    runner.run(&rewrites);
}
