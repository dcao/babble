use std::collections::HashMap;

use crate::lang::Drawing;
use babble::{ast_node::Expr, learn::LibId};
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

    #[allow(dead_code)]
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

    #[allow(clippy::too_many_lines)]
    fn eval(&self, expr: &'a Expr<Drawing>) -> Result<Value<'a>, TypeError> {
        match (expr.0.operation(), expr.0.args()) {
            (&Drawing::Float(f), []) => Ok(Value::Num(f.into())),
            (&Drawing::Pi, []) => Ok(Value::Num(std::f64::consts::PI)),
            (Drawing::Circle, []) => Ok(Value::Shapes(vec![Shape::Circle {
                center: (0.0, 0.0),
                radius: 1.0,
            }])),
            (Drawing::Line, []) => Ok(Value::Shapes(vec![Shape::Line {
                start: (0.0, 0.0),
                end: (1.0, 0.0),
            }])),
            (Drawing::Square, []) => Ok(Value::Shapes(vec![
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
            ])),
            (Drawing::Rect, [w, h]) => match (self.eval(w)?, self.eval(h)?) {
                (Value::Num(w), Value::Num(h)) => Ok(Value::Shapes(vec![
                    Shape::Line {
                        start: (-w / 2.0, -h / 2.0),
                        end: (w / 2.0, -h / 2.0),
                    },
                    Shape::Line {
                        start: (w / 2.0, -h / 2.0),
                        end: (w / 2.0, h / 2.0),
                    },
                    Shape::Line {
                        start: (w / 2.0, h / 2.0),
                        end: (-w / 2.0, h / 2.0),
                    },
                    Shape::Line {
                        start: (-w / 2.0, h / 2.0),
                        end: (-w / 2.0, -h / 2.0),
                    },
                ])),
                _ => Err(TypeError {
                    expected: "numbers".to_string(),
                }),
            },
            (Drawing::Empty, []) => Ok(Value::Shapes(vec![])),
            (&Drawing::Var(index), []) => Ok(self.get_index(index.0).clone()),
            (&Drawing::LibVar(name), []) => Ok(self.get_lib(name).clone()),
            (Drawing::Lambda, [body]) => Ok(Value::Lambda(body, self.args.clone())),
            (Drawing::Transform, [expr, mat]) => {
                let val = self.eval(expr)?;
                match self.eval(mat) {
                    Ok(Value::Matrix(entries)) => Ok(val.map_shapes(|shape| {
                        shape
                            .scale(entries.scale)
                            .rotate(entries.rotate)
                            .translate(entries.translate_x, entries.translate_y)
                    })),
                    _ => Err(TypeError {
                        expected: "second argument to Transform must be matrix".to_string(),
                    }),
                }
            }
            (Drawing::Matrix, [sc, rot, x, y]) => {
                let s = self.eval(sc)?.to_float()?;
                let angle = self.eval(rot)?.to_float()?;
                let tran_x = self.eval(x)?.to_float()?;
                let tran_y = self.eval(y)?.to_float()?;
                Ok(Value::Matrix(Entries {
                    scale: s,
                    rotate: angle,
                    translate_x: tran_x,
                    translate_y: tran_y,
                }))
            }
            (Drawing::Add, [x, y]) => {
                let ax = self.eval(x)?.to_float()?;
                let ay = self.eval(y)?.to_float()?;
                Ok(Value::Num(ax + ay))
            }
            (Drawing::Sub, [x, y]) => {
                let sx = self.eval(x)?.to_float()?;
                let sy = self.eval(y)?.to_float()?;
                Ok(Value::Num(sx - sy))
            }
            (Drawing::Mul, [x, y]) => {
                let mx = self.eval(x)?.to_float()?;
                let my = self.eval(y)?.to_float()?;
                Ok(Value::Num(mx * my))
            }
            (Drawing::Div, [x, y]) => {
                let mx = self.eval(x)?.to_float()?;
                let my = self.eval(y)?.to_float()?;
                Ok(Value::Num(mx / my))
            }
            (Drawing::Pow, [x, y]) => {
                let mx = self.eval(x)?.to_float()?;
                let my = self.eval(y)?.to_float()?;
                Ok(Value::Num(mx.powf(my)))
            }
            (Drawing::Max, [x, y]) => {
                let mx = self.eval(x)?.to_float()?;
                let my = self.eval(y)?.to_float()?;
                Ok(Value::Num(mx.max(my)))
            }
            (Drawing::Sin, [th]) => {
                let theta = self.eval(th)?.to_float()?;
                Ok(Value::Num(theta.sin()))
            }
            (Drawing::Cos, [th]) => {
                let theta = self.eval(th)?.to_float()?;
                Ok(Value::Num(theta.cos()))
            }
            (Drawing::Tan, [th]) => {
                let theta = self.eval(th)?.to_float()?;
                Ok(Value::Num(theta.tan()))
            }
            (&Drawing::Lib(name), [bound_value, body]) => {
                let bound_value = self.eval(bound_value)?;
                let context = self.clone().with_lib(name, bound_value);

                context.eval(body)
            }
            (Drawing::Apply, [fun, arg]) => {
                if let Value::Lambda(body, mut args) = self.eval(fun)? {
                    let arg = self.eval(arg)?;
                    let mut context = self.clone();
                    args.push(arg);
                    context.args = args;
                    context.eval(body)
                } else {
                    Err(TypeError {
                        expected: "function".to_string(),
                    })
                }
            }
            (Drawing::Connect, exprs) => {
                let mut shapes = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    shapes.extend(self.eval(expr)?.into_shapes()?);
                }
                Ok(Value::Shapes(shapes))
            }
            (Drawing::Repeat, [expr, times, mat]) => {
                let val = self.eval(expr)?;
                let t = match self.eval(times) {
                    Ok(Value::Num(f)) => f64_to_usize(f),
                    _ => panic!("Second argument to Repeat must be a non-negative number"),
                };
                let (tx, ty, rot, sc) = match self.eval(mat) {
                    Ok(Value::Matrix(entries)) => (
                        entries.translate_x,
                        entries.translate_y,
                        entries.rotate,
                        entries.scale,
                    ),
                    _ => panic!("Third argument to Repeat must be matrix"),
                };

                let mut shapes = Vec::with_capacity(t);
                let mut prev = val;
                for _i in 0..t {
                    shapes.extend(prev.clone().into_shapes().ok().unwrap());
                    prev = prev.map_shapes(|shape| shape.translate(tx, ty).rotate(rot).scale(sc));
                }

                shapes.dedup();
                Ok(Value::Shapes(shapes))
            }
            (Drawing::List, exprs) => {
                // Places the result of evaluating each of the `exprs` one under the other.
                let margin = 1.0;
                // We will put all the resulting shapes into this vector:
                let mut shapes = Vec::with_capacity(exprs.len());
                // The bounding box of the already placed shapes:
                let mut bbox = BoundingBox::EMPTY;
                for (i, expr) in exprs.iter().enumerate() {
                    let value = self.eval(expr)?;
                    // The bounding box of the current value, at the origin:
                    let new_box = Shape::shapes_bounding_box(&value.clone().into_shapes()?);
                    // Vertical shift to place the current value (sufficient to separate the bottom `bbox` from the new value by `margin`):
                    let shift = if i == 0 {
                        0.0
                    } else {
                        bbox.lower_left.1 - new_box.upper_right.1 - margin
                    };
                    let new_shapes = value
                        .map_shapes(|shape| shape.translate(0.0, shift))
                        .into_shapes()?;
                    // Update the global bounding box by a *shifted* bounding box of the new shapes:
                    bbox = bbox.union(&new_box.translate(0.0, shift));
                    shapes.extend(new_shapes);
                }
                Ok(Value::Shapes(shapes))
            }
            unreach => unreachable!("{:?}", unreach),
        }
    }
}

/// Converts an `f64` in the right range to a `usize` by rounding, or panics.
#[allow(
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss
)]
fn f64_to_usize(f: f64) -> usize {
    assert!(f >= 0.0, "expected a nonnegative number");
    assert!(
        f <= usize::MAX as f64,
        "number is too big to convert to an integer"
    );
    f.round() as usize
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
    /// Also contains bound vars at this point
    Lambda(&'a Expr<Drawing>, Vec<Value<'a>>),
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

/// A bounding box defined by its two corners.
#[derive(Clone, Debug)]
pub(crate) struct BoundingBox {
    pub(crate) lower_left: (f64, f64),
    pub(crate) upper_right: (f64, f64),
}

impl BoundingBox {
    /// Empty bounding box.
    const EMPTY: Self = Self {
        lower_left: (std::f64::MAX, std::f64::MAX),
        upper_right: (std::f64::MIN, std::f64::MIN),
    };

    /// Unions this bounding box with another.
    fn union(&self, other: &BoundingBox) -> BoundingBox {
        BoundingBox {
            lower_left: (
                self.lower_left.0.min(other.lower_left.0),
                self.lower_left.1.min(other.lower_left.1),
            ),
            upper_right: (
                self.upper_right.0.max(other.upper_right.0),
                self.upper_right.1.max(other.upper_right.1),
            ),
        }
    }

    /// Translates this bounding box by the given amount.
    fn translate(&self, x: f64, y: f64) -> BoundingBox {
        BoundingBox {
            lower_left: (self.lower_left.0 + x, self.lower_left.1 + y),
            upper_right: (self.upper_right.0 + x, self.upper_right.1 + y),
        }
    }
}

impl Value<'_> {
    fn to_float(&self) -> Result<f64, TypeError> {
        match *self {
            Self::Num(f) => Ok(f),
            _ => Err(TypeError {
                expected: "number".to_string(),
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

    fn rotate(self, radians: f64) -> Self {
        let (sin, cos) = radians.sin_cos();
        self.similarity_transform(|x, y| (x * cos - y * sin, x * sin + y * cos))
    }

    fn scale(self, factor: f64) -> Self {
        self.similarity_transform(|x, y| (x * factor, y * factor))
    }

    fn bounding_box(&self) -> BoundingBox {
        match self {
            Self::Circle {
                center: (x, y),
                radius,
            } => BoundingBox {
                lower_left: (x - radius, y - radius),
                upper_right: (x + radius, y + radius),
            },
            Self::Line {
                start: (x1, y1),
                end: (x2, y2),
            } => BoundingBox {
                lower_left: (x1.min(*x2), y1.min(*y2)),
                upper_right: (x1.max(*x2), y1.max(*y2)),
            },
        }
    }

    fn shapes_bounding_box(shapes: &[Shape]) -> BoundingBox {
        shapes.iter().fold(BoundingBox::EMPTY, |bbox, shape| {
            bbox.union(&shape.bounding_box())
        })
    }
}

impl Picture {
    /// Returns the bounding box of this picture.
    pub(crate) fn bounding_box(&self) -> BoundingBox {
        Shape::shapes_bounding_box(&self.shapes)
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
