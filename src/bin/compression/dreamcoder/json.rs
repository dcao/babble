//! The JSON interface to Dream&shy;Coder.

use serde::{Deserialize, Serialize};
use babble::ast_node::Expr;
use super::{expr::{DreamCoderOp, DcExpr}, types::Type};

/// The input format of the `compression` tool.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompressionInput {
    #[serde(rename = "CPUs")]
    pub cpus: u32,
    pub arity: u32,
    pub verbose: bool,

    #[serde(rename = "collect_data")]
    pub collect_data: bool,
    pub bs: u32, // what is this
    pub aic: u32,
    pub structure_penalty: u32,
    pub top_k: u32,

    #[serde(rename = "DSL")]
    pub dsl: Dsl,
    pub frontiers: Vec<Frontier>,
}

/// The output format of the `compression` tool.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
pub struct CompressionOutput {
    #[serde(rename = "DSL")]
    pub dsl: Dsl,
    pub frontiers: Vec<Frontier>,
}

/// The primitives and learned functions for the language.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Dsl {
    pub log_variable: f64,
    pub productions: Vec<Production>,
}

/// A primitive or learned function.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Production {
    pub log_probability: f64,
    pub expression: DcExpr,
}

/// A particular task for `compression` to examine.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Frontier {
    pub task: Option<String>,
    pub request: Type,
    pub programs: Vec<Program>,
}

/// A particular program that `compression` will try to compress.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Program {
    pub log_likelihood: f64,
    pub program: DcExpr,
}
