use std::collections::HashMap;

use crate::lang::Drawing;
use babble::{ast_node::Expr, learn::LibId};
use nom::error;
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

    fn shift(mut self) -> Self {
        self.args.pop();
        self
    }

    fn get_index(&self, index: usize) -> &Value<'a> {
        &self.args[self.args.len() - (index + 1)]
    }

    fn get_lib(&self, name: LibId) -> &Value<'a> {
        &self.libs[&name]
    }

    fn eval(&self, expr: &'a Expr<Drawing>) -> Result<Value<'a>, TypeError> {
        let result = match (expr.0.operation(), expr.0.args()) {
            (&Drawing::Float(f), []) => Value::Num(f.into()),
            (&Drawing::Pi, []) => Value::Num(std::f64::consts::PI),
            (Drawing::Circle, []) => Value::Shapes(vec![Shape::Circle {
                center: (0.0, 0.0),
                radius: 1.0,
            }]),
            (Drawing::Line, []) => Value::Shapes(vec![Shape::Line {
                start: (-0.5, 0.0),
                end: (0.5, 0.0),
            }]),
            (Drawing::Square, []) => Value::Shapes(vec![
                Shape::Line {
                    start: (-0.5, -0.5),
                    end: (0.5, -0.5),
                },
                Shape::Line {
                    start: (0.5, -0.5),
                    end: (0.5, 0.5),
                },
                Shape::Line {
                    start: (0.5, 0.5),
                    end: (-0.5, 0.5),
                },
                Shape::Line {
                    start: (-0.5, 0.5),
                    end: (-0.5, -0.5),
                },
            ]),
            (&Drawing::Var(index), []) => self.get_index(index.0).clone(),
            (&Drawing::LibVar(name), []) => self.get_lib(name).clone(),
            (Drawing::Lambda, [body]) => Value::Lambda(body),
            (Drawing::Transform, [expr, mat]) => {
                let val = self.eval(expr)?;
                let (tx, ty, rot, sc) = match self.eval(mat) {
                    Ok(Value::Matrix(entries)) => (
                        entries.translate_x,
                        entries.translate_y,
                        entries.rotate,
                        entries.scale,
                    ),
                    _ => panic!("second argument to Transform must be matrix"),
                };
                val.map_shapes(|shape| shape.translate(tx, ty).rotate(rot).scale(sc))
            }
            (Drawing::Matrix, [sc, rot, x, y]) => {
                let s = self.eval(sc)?.to_float()?;
                let angle = self.eval(rot)?.to_float()?;
                let tran_x = self.eval(x)?.to_float()?;
                let tran_y = self.eval(y)?.to_float()?;
                Value::Matrix(Entries {
                    scale: s,
                    rotate: angle,
                    translate_x: tran_x,
                    translate_y: tran_y,
                })
            }
            (Drawing::Add, [x, y]) => {
                let ax = self.eval(x)?.to_float()?;
                let ay = self.eval(y)?.to_float()?;
                Value::Num(ax + ay)
            }
            (Drawing::Sub, [x, y]) => {
                let sx = self.eval(x)?.to_float()?;
                let sy = self.eval(y)?.to_float()?;
                Value::Num(sx - sy)
            }
            (Drawing::Mul, [x, y]) => {
                let mx = self.eval(x)?.to_float()?;
                let my = self.eval(y)?.to_float()?;
                Value::Num(mx * my)
            }
            (Drawing::Div, [x, y]) => {
                let mx = self.eval(x)?.to_float()?;
                let my = self.eval(y)?.to_float()?;
                Value::Num(mx / my)
            }
            (Drawing::Sin, [th]) => {
                let theta = self.eval(th)?.to_float()?;
                Value::Num(theta.sin())
            }
            (Drawing::Cos, [th]) => {
                let theta = self.eval(th)?.to_float()?;
                Value::Num(theta.cos())
            }
            (Drawing::Tan, [th]) => {
                let theta = self.eval(th)?.to_float()?;
                Value::Num(theta.tan())
            }
            (&Drawing::Lib(name), [bound_value, body]) => {
                let bound_value = self.eval(bound_value)?;
                let context = self.clone().with_lib(name, bound_value);
                context.eval(body)?
            }
            (Drawing::Apply, [fun, arg]) => {
                let body = self.eval(fun)?.to_body()?;
                let arg = self.eval(arg)?;
                let context = self.clone().with_arg(arg);
                context.eval(body)?
            }
            (Drawing::Connect, exprs) => {
                let mut shapes = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    shapes.extend(self.eval(expr)?.into_shapes()?);
                }
                Value::Shapes(shapes)
            }
            (Drawing::Shift, [body]) => {
                let context = self.clone().shift();
                context.eval(body)?
            }
            (Drawing::Repeat, [expr, times, mat]) => {
                let val = self.eval(expr)?;
                let t = match self.eval(times) {
                    Ok(Value::Num(f)) => f as usize,
                    _ => panic!("Second argument to repeat must be a number"),
                };
                let (tx, ty, rot, sc) = match self.eval(mat) {
                    Ok(Value::Matrix(entries)) => (
                        entries.translate_x,
                        entries.translate_y,
                        entries.rotate,
                        entries.scale,
                    ),
                    _ => panic!("second argument to Transform must be matrix"),
                };

                let mut shapes = Vec::with_capacity(t);
                let mut prev = val;
                for _i in 0..t {
                    shapes.extend(prev.clone().into_shapes().ok().unwrap());
                    prev = prev.map_shapes(|shape| shape.translate(tx, ty).rotate(rot).scale(sc));
                }

                shapes.dedup();
                Value::Shapes(shapes)
            }
            (Drawing::List, exprs) => {
                let mut shapes = Vec::with_capacity(exprs.len());
                for (index, expr) in exprs.iter().enumerate() {
                    let value = self.eval(expr)?;
                    // TODO: instead of using 10 we should compute the size of the drawing and translate by that amount
                    shapes.extend(
                        value
                            .map_shapes(|shape| shape.translate(0.0, (index as f64) * 10.0))
                            .into_shapes()?,
                    );
                }
                Value::Shapes(shapes)
            }
            unreach => unreachable!("{:?}", unreach),
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
#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone)]
pub(crate) struct Entries {
    pub(crate) scale: f64,
    pub(crate) rotate: f64,
    pub(crate) translate_x: f64,
    pub(crate) translate_y: f64,
}

/// The result of evaluating an expression in the "Drawing" language.
#[derive(Debug, Clone)]
pub(crate) enum Value<'a> {
    /// A floating-point number
    Num(f64),
    /// A function and a reference to its body
    Lambda(&'a Expr<Drawing>),
    /// A collection of shapes
    Shapes(Vec<Shape>),
    Matrix(Entries),
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

    fn to_body(&self) -> Result<&'a Expr<Drawing>, TypeError> {
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
pub(crate) fn eval(expr: &Expr<Drawing>) -> Result<Value<'_>, TypeError> {
    Context::new().eval(expr)
}
