use crate::ast_node::Arity;
use std::{convert::Infallible, str::FromStr};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum MyLang {
    Zero,
    Plus,
}

impl Arity for MyLang {
    fn min_arity(&self) -> usize {
        match self {
            Self::Zero => 0,
            Self::Plus => 1,
        }
    }

    fn max_arity(&self) -> Option<usize> {
        match self {
            Self::Zero => Some(0),
            Self::Plus => None,
        }
    }
}

impl FromStr for MyLang {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" => Ok(Self::Zero),
            "+" => Ok(Self::Plus),
            _ => unreachable!(),
        }
    }
}
