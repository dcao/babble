use crate::lang::Smiley;
use babble::ast_node::Expr;
use egg::Symbol;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Clone, Debug)]
struct Context<'a> {
    ident_env: HashMap<Symbol, Value<'a>>,
    arg_env: Vec<Value<'a>>,
}

impl<'a> Context<'a> {
    fn new() -> Self {
        Self {
            ident_env: HashMap::new(),
            arg_env: Vec::new(),
        }
    }

    fn with_ident(mut self, ident: Symbol, value: Value<'a>) -> Self {
        self.ident_env.insert(ident, value);
        self
    }

    fn with_arg(mut self, value: Value<'a>) -> Self {
        self.arg_env.push(value);
        self
    }

    fn get_index(&self, index: usize) -> &Value<'a> {
        &self.arg_env[self.arg_env.len() - (index + 1)]
    }

    fn eval(&self, expr: &'a Expr<Smiley>) -> Result<Value<'a>, EvalError> {
        let result = match (expr.0.operation(), expr.0.args()) {
            (&Smiley::Int(i), []) => Value::Num(i.into()),
            (&Smiley::Float(f), []) => Value::Num(f.into()),
            (Smiley::Circle, []) => Value::Shapes(vec![Shape::Circle {
                center: (0.0, 0.0),
                radius: 1.0,
            }]),
            (Smiley::Line, []) => Value::Shapes(vec![Shape::Line {
                start: (-0.5, 0.0),
                end: (0.5, 0.0),
            }]),
            (Smiley::Ident(ident), []) => self.ident_env[ident].clone(),
            (&Smiley::Index(index), []) => self.get_index(index.0).clone(),
            (Smiley::Lambda, [body]) => Value::Lambda(body),
            (Smiley::Move, [x_offset, y_offset, expr]) => {
                let x_offset: f64 = self.eval(x_offset)?.get_num().ok_or(EvalError::TypeError)?;
                let y_offset: f64 = self.eval(y_offset)?.get_num().ok_or(EvalError::TypeError)?;
                let val = self.eval(expr)?;
                val.map_shapes(|shape| shape.translate(x_offset, y_offset))
            }
            (Smiley::Scale, [factor, expr]) => {
                let factor = self.eval(factor)?.get_num().ok_or(EvalError::TypeError)?;
                let val = self.eval(expr)?;
                val.map_shapes(|shape| shape.scale(factor))
            }
            (Smiley::Rotate, [angle, expr]) => {
                let angle = self.eval(angle)?.get_num().ok_or(EvalError::TypeError)?;
                let val = self.eval(expr)?;
                val.map_shapes(|shape| shape.rotate(angle))
            }
            (Smiley::Let | Smiley::Lib, [ident, val, body]) => {
                let ident = match *ident.0.operation() {
                    Smiley::Ident(ident) => ident,
                    _ => return Err(EvalError::SyntaxError),
                };
                let val = self.eval(val)?;
                let context = self.clone().with_ident(ident, val);
                context.eval(body)?
            }
            (Smiley::Apply, [fun, arg]) => {
                let body = self.eval(fun)?.get_body().ok_or(EvalError::TypeError)?;
                let arg = self.eval(arg)?;
                let context = self.clone().with_arg(arg);
                context.eval(body)?
            }
            (Smiley::Compose, [expr1, expr2]) => {
                let mut shapes1 = self
                    .eval(expr1)?
                    .into_shapes()
                    .ok_or(EvalError::TypeError)?;
                let shapes2 = self
                    .eval(expr2)?
                    .into_shapes()
                    .ok_or(EvalError::TypeError)?;
                shapes1.extend(shapes2);
                Value::Shapes(shapes1)
            }
            _ => unreachable!(),
        };
        Ok(result)
    }
}

impl Default for Context<'_> {
    fn default() -> Self {
        Self::new()
    }
}

/// The primitive components of a [`Picture`].
#[derive(Debug, Clone, Copy)]
pub(crate) enum Shape {
    /// A circle
    Circle {
        /// The circle's center point
        center: (f64, f64),
        /// The circle's radius
        radius: f64,
    },
    /// A line segment
    Line {
        /// The starting point of the line segment
        start: (f64, f64),
        /// The end point of the line segment
        end: (f64, f64),
    },
}

/// The result of evaluating an expression in the "Smiley" language.
#[derive(Debug, Clone)]
pub(crate) enum Value<'a> {
    /// A floating-point number
    Num(f64),
    /// A function and a reference to its body
    Lambda(&'a Expr<Smiley>),
    /// A collection of shapes
    Shapes(Vec<Shape>),
}

/// A picture, made up of [`Shape`]s
#[derive(Debug, Clone)]
pub(crate) struct Picture {
    pub(crate) shapes: Vec<Shape>,
}

/// An error encountered while attempting to evaluate a Smiley expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub(crate) enum EvalError {
    /// A syntax error.
    #[error("syntax error")]
    SyntaxError,
    /// A type error.
    #[error("type error")]
    TypeError,
}

impl<'a> Value<'a> {
    fn get_num(&self) -> Option<f64> {
        match *self {
            Self::Num(f) => Some(f),
            _ => None,
        }
    }

    fn get_body(&self) -> Option<&'a Expr<Smiley>> {
        match self {
            Self::Lambda(node) => Some(node),
            _ => None,
        }
    }

    fn into_shapes(self) -> Option<Vec<Shape>> {
        match self {
            Self::Shapes(shapes) => Some(shapes),
            _ => None,
        }
    }

    /// Try to convert this value into a picture. If the value isn't a
    /// collection of shapes, returns `None`.
    pub(crate) fn into_picture(self) -> Option<Picture> {
        let shapes = self.into_shapes()?;
        Some(Picture { shapes })
    }

    fn map_shapes<F>(self, f: F) -> Self
    where
        F: FnMut(Shape) -> Shape,
    {
        match self {
            Self::Shapes(shapes) => Self::Shapes(shapes.into_iter().map(f).collect()),
            other => other,
        }
    }
}

impl Shape {
    fn similarity_transform<F>(self, transform_point: F) -> Self
    where
        F: Fn(f64, f64) -> (f64, f64),
    {
        match self {
            Self::Circle {
                center: (x, y),
                radius,
            } => {
                let (x1, y1) = transform_point(x, y);
                let (x2, y2) = transform_point(x + radius, y);
                let new_radius = (x2 - x1).hypot(y2 - y1);
                Self::Circle {
                    center: (x1, y1),
                    radius: new_radius,
                }
            }
            Self::Line {
                start: (x1, y1),
                end: (x2, y2),
            } => Self::Line {
                start: transform_point(x1, y1),
                end: transform_point(x2, y2),
            },
        }
    }

    fn translate(self, x_offset: f64, y_offset: f64) -> Self {
        self.similarity_transform(|x, y| (x + x_offset, y + y_offset))
    }

    fn rotate(self, degrees: f64) -> Self {
        let (sin, cos) = degrees.to_radians().sin_cos();
        self.similarity_transform(|x, y| (x * cos - y * sin, x * sin + y * cos))
    }

    fn scale(self, factor: f64) -> Self {
        self.similarity_transform(|x, y| (x * factor, y * factor))
    }
}

/// Evaluate the expression `expr`.
///
/// # Errors
///
/// Returns `Err` if the expression is malformed or contains a type error.
pub(crate) fn eval(expr: &Expr<Smiley>) -> Result<Value<'_>, EvalError> {
    Context::new().eval(expr)
}
