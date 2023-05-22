//! Dream&shy;Coder's representation of Hindley-Milner types.

use std::fmt::{self, Debug, Display, Formatter};

use super::util;
use egg::Symbol;
use itertools::Itertools;
use serde::{Deserialize, Serialize};

#[allow(single_use_lifetimes)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
struct RawType<'a> {
    constructor: &'a str,
    arguments: Vec<RawType<'a>>,
}

/// The type of a program.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(from = "RawType<'_>")]
#[serde(into = "RawType<'_>")]
pub enum Type {
    /// A base type, e.g. `int`, `bool`, etc.
    Simple {
        /// The name of the type.
        name: Symbol,
    },

    /// A compound type (e.g. `list int`), formed from a named constructor and its arguments.
    Compound {
        /// The name of the constructor.
        constructor: Symbol,

        /// The arguments to the type constructor.
        arguments: Vec<Type>,
    },

    /// A function type, e.g. `int -> int`.
    Function {
        /// The type of the function's argument.
        from: Box<Type>,

        /// The return type of the function.
        to: Box<Type>,
    },
}

impl Type {
    /// Create a new simple type named `name`.
    #[must_use]
    pub fn simple<T: Into<Symbol>>(name: T) -> Self {
        let name = name.into();
        Self::Simple { name }
    }

    /// Create a new compound type with constructor `constructor` and arguments `arguments`.
    #[must_use]
    pub fn compound<T: Into<Symbol>>(constructor: T, arguments: Vec<Type>) -> Self {
        let constructor = constructor.into();
        Self::Compound {
            constructor,
            arguments,
        }
    }

    /// Create a new function type with argument type `from` and return type `to`.
    #[must_use]
    pub fn function(from: Type, to: Type) -> Self {
        let from = from.into();
        let to = to.into();
        Self::Function { from, to }
    }
}

impl<'a> From<RawType<'a>> for Type {
    fn from(raw_type: RawType<'a>) -> Self {
        let RawType {
            constructor,
            mut arguments,
        } = raw_type;

        if arguments.is_empty() {
            Self::simple(constructor)
        } else if constructor == "->" && arguments.len() == 2 {
            let to = arguments.pop().unwrap();
            let from = arguments.pop().unwrap();
            Self::function(from.into(), to.into())
        } else {
            let arguments = arguments.into_iter().map_into().collect();
            Self::compound(constructor, arguments)
        }
    }
}

impl From<Type> for RawType<'_> {
    fn from(typ: Type) -> Self {
        match typ {
            Type::Simple { name } => Self {
                constructor: name.as_str(),
                arguments: Vec::new(),
            },
            Type::Compound {
                constructor,
                arguments,
            } => {
                let constructor = constructor.as_str();
                let arguments = arguments.into_iter().map_into().collect();
                Self {
                    constructor,
                    arguments,
                }
            }
            Type::Function { from, to } => Self {
                constructor: "->",
                arguments: vec![(*from).into(), (*to).into()],
            },
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Simple { name } => write!(f, "{name}"),
            Type::Compound {
                constructor,
                arguments,
            } => util::parens(1, f, |f| {
                write!(f, "{constructor}")?;
                for argument in arguments {
                    write!(f, " {argument:.2}")?;
                }
                Ok(())
            }),
            Type::Function { from, to } => {
                util::parens(0, f, |f| write!(f, "{from:.1} -> {to:.0}"))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Type;
    #[test]
    fn type_display() {
        let typ = Type::function(
            Type::function(
                Type::simple("int"),
                Type::compound("list", vec![Type::simple("int")]),
            ),
            Type::function(
                Type::compound(
                    "list",
                    vec![Type::compound("list", vec![Type::simple("bool")])],
                ),
                Type::compound(
                    "result",
                    vec![
                        Type::simple("int"),
                        Type::compound("list", vec![Type::simple("string")]),
                    ],
                ),
            ),
        );
        let expected = "(int -> list int) -> list (list bool) -> result int (list string)";

        assert_eq!(typ.to_string(), expected);
    }
}
