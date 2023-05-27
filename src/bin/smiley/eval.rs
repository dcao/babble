use std::collections::HashMap;

use crate::lang::Smiley;
use babble::{ast_node::Expr, learn::LibId};
use nalgebra::{self, Matrix2, Rotation2, Vector2};
use thiserror::Error;

#[derive(Clone, Debug)]
struct Context<'a> {
    libs: HashMap<LibId, Value<'a>>,
    args: Vec<Value<'a>>,
}

impl<'a> Context<'a> {
    fn new() -> Self {
        Self {
            libs: HashMap::new(),
            args: Vec::new(),
        }
    }

    fn with_arg(mut self, value: Value<'a>) -> Self {
        self.args.push(value);
        self
    }

    fn with_lib(mut self, name: LibId, value: Value<'a>) -> Self {
        self.libs.insert(name, value);
        self
    }

    fn get_index(&self, index: usize) -> &Value<'a> {
        &self.args[self.args.len() - (index + 1)]
    }

    fn get_lib(&self, name: LibId) -> &Value<'a> {
        &self.libs[&name]
    }

    fn eval(&self, expr: &'a Expr<Smiley>) -> Result<Value<'a>, TypeError> {
        let result = match (expr.0.operation(), expr.0.args()) {
            (&Smiley::Int(i), []) => Value::Num(i.into()),
            (&Smiley::Float(f), []) => Value::Num(f.into()),
            (Smiley::Circle, []) => Value::Shapes(vec![Shape::Ellipse {
                transform: Matrix2::identity(),
                center: Vector2::zeros(),
            }]),
            (Smiley::Line, []) => Value::Shapes(vec![Shape::Line {
                start: Vector2::new(-1.0, 0.0),
                end: Vector2::new(1.0, 0.0),
            }]),
            (&Smiley::Var(index), []) => self.get_index(index.0).clone(),
            (&Smiley::LibVar(name), []) => self.get_lib(name).clone(),
            (Smiley::Lambda, [body]) => Value::Lambda(body),
            (Smiley::Move, [x_offset, y_offset, expr]) => {
                let x_offset: f64 = self.eval(x_offset)?.to_float()?;
                let y_offset: f64 = self.eval(y_offset)?.to_float()?;
                let val = self.eval(expr)?;
                val.map_shapes(|shape| shape.translate(x_offset, y_offset))
            }
            (Smiley::Scale, [factor, expr]) => {
                let factor = self.eval(factor)?.to_float()?;
                let val = self.eval(expr)?;
                val.map_shapes(|shape| shape.scale(factor))
            }
            (Smiley::ScaleX, [factor, expr]) => {
                let factor = self.eval(factor)?.to_float()?;
                let val = self.eval(expr)?;
                val.map_shapes(|shape| shape.scale_x(factor))
            }
            (Smiley::ScaleY, [factor, expr]) => {
                let factor = self.eval(factor)?.to_float()?;
                let val = self.eval(expr)?;
                val.map_shapes(|shape| shape.scale_y(factor))
            }
            (Smiley::Rotate, [angle, expr]) => {
                let angle = self.eval(angle)?.to_float()?;
                let val = self.eval(expr)?;
                val.map_shapes(|shape| shape.rotate(angle))
            }
            (&Smiley::Lib(name), [bound_value, body]) => {
                let bound_value = self.eval(bound_value)?;
                let context = self.clone().with_lib(name, bound_value);
                context.eval(body)?
            }
            (Smiley::Apply, [fun, arg]) => {
                let body = self.eval(fun)?.to_body()?;
                let arg = self.eval(arg)?;
                let context = self.clone().with_arg(arg);
                context.eval(body)?
            }
            (Smiley::Compose, exprs) => {
                let mut shapes = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    shapes.extend(self.eval(expr)?.into_shapes()?);
                }
                Value::Shapes(shapes)
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
    /// A line segment
    Line {
        /// The starting point of the line segment
        start: Vector2<f64>,
        /// The end point of the line segment
        end: Vector2<f64>,
    },
    /// An ellipse
    Ellipse {
        /// A linear transformation taking the unit circle to this ellipse,
        /// centered at the origin
        transform: Matrix2<f64>,

        /// The center of the ellipse
        center: Vector2<f64>,
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

/// A type error.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[error("type mismatch")]
pub(crate) struct TypeError {
    expected: String,
}

impl<'a> Value<'a> {
    fn to_float(&self) -> Result<f64, TypeError> {
        match *self {
            Self::Num(f) => Ok(f),
            _ => Err(TypeError {
                expected: "number".to_string(),
            }),
        }
    }

    fn to_body(&self) -> Result<&'a Expr<Smiley>, TypeError> {
        match self {
            Self::Lambda(node) => Ok(node),
            _ => Err(TypeError {
                expected: "function".to_string(),
            }),
        }
    }

    fn into_shapes(self) -> Result<Vec<Shape>, TypeError> {
        match self {
            Self::Shapes(shapes) => Ok(shapes),
            _ => Err(TypeError {
                expected: "shapes".to_string(),
            }),
        }
    }

    /// Try to convert this value into a picture. If the value isn't a
    /// collection of shapes, returns `None`.
    pub(crate) fn into_picture(self) -> Result<Picture, TypeError> {
        let shapes = self.into_shapes()?;
        Ok(Picture { shapes })
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
    fn translate(self, x_offset: f64, y_offset: f64) -> Self {
        let offset = Vector2::new(x_offset, y_offset);
        match self {
            Self::Line { start, end } => Self::Line {
                start: start + offset,
                end: end + offset,
            },
            Self::Ellipse { transform, center } => Self::Ellipse {
                transform,
                center: center + offset,
            },
        }
    }

    fn linear_transform(self, matrix: Matrix2<f64>) -> Self {
        match self {
            Self::Line { start, end } => Self::Line {
                start: matrix * start,
                end: matrix * end,
            },
            Self::Ellipse { transform, center } => Self::Ellipse {
                transform: matrix * transform,
                center,
            },
        }
    }

    fn rotate(self, degrees: f64) -> Self {
        let rotation_matrix = Rotation2::new(degrees.to_radians());
        self.linear_transform(rotation_matrix.into_inner())
    }

    fn scale(self, factor: f64) -> Self {
        self.linear_transform(factor * Matrix2::identity())
    }

    fn scale_x(self, factor: f64) -> Self {
        self.linear_transform(Matrix2::new(factor, 0.0, 0.0, 1.0))
    }

    fn scale_y(self, factor: f64) -> Self {
        self.linear_transform(Matrix2::new(1.0, 0.0, 0.0, factor))
    }
}

/// Evaluate the expression `expr`.
///
/// # Errors
///
/// Returns `Err` if the expression is malformed or contains a type error.
pub(crate) fn eval(expr: &Expr<Smiley>) -> Result<Value<'_>, TypeError> {
    Context::new().eval(expr)
}
