use crate::lang::Smiley;
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

    fn eval(&self, expr: &'a Expr<Smiley>) -> Result<Value<'a>, TypeError> {
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
            (&Smiley::Var(index), []) => self.get_index(index.0).clone(),
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
            (Smiley::Rotate, [angle, expr]) => {
                let angle = self.eval(angle)?.to_float()?;
                let val = self.eval(expr)?;
                val.map_shapes(|shape| shape.rotate(angle))
            }
            (Smiley::Lib(_), [bound_value, body]) => {
                let bound_value = self.eval(bound_value)?;
                let context = self.clone().with_arg(bound_value);
                context.eval(body)?
            }
            (Smiley::Apply, [fun, arg]) => {
                let body = self.eval(fun)?.to_body()?;
                let arg = self.eval(arg)?;
                let context = self.clone().with_arg(arg);
                context.eval(body)?
            }
            (Smiley::Compose, [expr1, expr2]) => {
                let mut shapes1 = self.eval(expr1)?.into_shapes()?;
                let shapes2 = self.eval(expr2)?.into_shapes()?;
                shapes1.extend(shapes2);
                Value::Shapes(shapes1)
            }
            (Smiley::Shift, [body]) => {
                let context = self.clone().shift();
                context.eval(body)?
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
pub(crate) fn eval(expr: &Expr<Smiley>) -> Result<Value<'_>, TypeError> {
    Context::new().eval(expr)
}
