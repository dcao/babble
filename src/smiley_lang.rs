//! The AST defining the smiley language.

use crate::{
    ast_node::{Arity, AstNode},
    eval::{self, Eval},
    free_vars::{and, not_free_in, FreeVarAnalysis, FreeVars},
    learn::LearnedLibrary,
    teachable::{BindingExpr, DeBruijnIndex, Teachable},
};
use babble_macros::rewrite_rules;
use egg::{AstSize, Extractor, Id, RecExpr, Rewrite, Runner, Symbol};
use lazy_static::lazy_static;
use ordered_float::NotNan;
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
    io::{self, Write},
    num::ParseIntError,
    ops::Index,
    str::FromStr,
};
use thiserror::Error;
use xml::{
    writer::{self, XmlEvent},
    EmitterConfig, EventWriter,
};

/// The operations/AST nodes of the "Smiley" language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Smiley {
    /// A signed integer constant.
    Int(i32),
    /// A floating-point constant.
    Float(NotNan<f64>),
    /// An identifier. This generally represents a named variable.
    Ident(Symbol),
    /// A de Bruijn-indexed variable. These are represented with a dollar sign
    /// followed by the index, i.e. `$0`, `$123`.
    Var(DeBruijnIndex),
    /// A unit circle.
    Circle,
    /// A unit line.
    Line,
    /// Translate a picture.
    Move,
    /// Scale a picture.
    Scale,
    /// Rotate a picture.
    Rotate,
    /// Union two pictures.
    Compose,
    /// Apply a function to an argument.
    Apply,
    /// Create an anonymous, de Bruijn-indexed function.
    Lambda,
    /// Bind a value to a name within an expression.
    Let,
    /// Bind a learned library function to a name within an expression. This is
    /// functionally identical to [`Smiley::Let`], but indicates that the
    /// function was learned through anti-unification. This creates a helpful
    /// visual distinction and allows rewrite rules to selectively target
    /// learned functions.
    Lib,
}

#[derive(Clone, Debug)]
struct Context<T> {
    ident_env: HashMap<Symbol, Value<T>>,
    arg_env: Vec<Value<T>>,
}

impl<T> Context<T> {
    fn new() -> Self {
        Self {
            ident_env: HashMap::new(),
            arg_env: Vec::new(),
        }
    }

    fn with_ident(mut self, ident: Symbol, value: Value<T>) -> Self {
        self.ident_env.insert(ident, value);
        self
    }

    fn with_arg(mut self, value: Value<T>) -> Self {
        self.arg_env.insert(0, value);
        self
    }
}

impl<T> Default for Context<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Copy> Eval<T> for Context<T> {
    type Op = Smiley;
    type Value = Value<T>;
    type Error = EvalError;

    fn eval_node<M>(
        &self,
        node: &AstNode<Self::Op, T>,
        nodes: &M,
    ) -> Result<Self::Value, Self::Error>
    where
        M: Index<T, Output = AstNode<Self::Op, T>>,
    {
        let result = match (node.operation(), node.children()) {
            (&Smiley::Int(i), []) => Value::Float(i.into()),
            (&Smiley::Float(f), []) => Value::Float(f.into()),
            (Smiley::Circle, []) => Value::Shapes(vec![Shape::Circle {
                center: (0.0, 0.0),
                radius: 1.0,
            }]),
            (Smiley::Line, []) => Value::Shapes(vec![Shape::Line {
                start: (-0.5, 0.0),
                end: (0.5, 0.0),
            }]),
            (Smiley::Ident(ident), []) => self.ident_env[ident].clone(),
            (&Smiley::Var(index), []) => self.arg_env[index.0].clone(),
            (Smiley::Lambda, &[body]) => Value::Lambda(body),
            (Smiley::Move, &[x_offset, y_offset, expr]) => {
                let x_offset: f64 = self
                    .eval_node(&nodes[x_offset], nodes)?
                    .as_float()
                    .ok_or(EvalError::TypeError)?;
                let y_offset: f64 = self
                    .eval_node(&nodes[y_offset], nodes)?
                    .as_float()
                    .ok_or(EvalError::TypeError)?;
                let val = self.eval_node(&nodes[expr], nodes)?;
                val.map_shapes(|shape| shape.translate(x_offset, y_offset))
            }
            (Smiley::Scale, &[factor, expr]) => {
                let factor = self
                    .eval_node(&nodes[factor], nodes)?
                    .as_float()
                    .ok_or(EvalError::TypeError)?;
                let val = self.eval_node(&nodes[expr], nodes)?;
                val.map_shapes(|shape| shape.scale(factor))
            }
            (Smiley::Rotate, &[angle, expr]) => {
                let angle = self
                    .eval_node(&nodes[angle], nodes)?
                    .as_float()
                    .ok_or(EvalError::TypeError)?;
                let val = self.eval_node(&nodes[expr], nodes)?;
                val.map_shapes(|shape| shape.rotate(angle))
            }
            (Smiley::Let | Smiley::Lib, &[ident, val, body]) => {
                let ident = nodes[ident].as_ident().ok_or(EvalError::SyntaxError)?;
                let val = self.eval_node(&nodes[val], nodes)?;
                self.clone()
                    .with_ident(ident, val)
                    .eval_node(&nodes[body], nodes)?
            }
            (Smiley::Apply, &[fun, arg]) => {
                let body = self
                    .eval_node(&nodes[fun], nodes)?
                    .as_lambda()
                    .ok_or(EvalError::TypeError)?;
                let arg = self.eval_node(&nodes[arg], nodes)?;
                self.clone().with_arg(arg).eval_node(&nodes[body], nodes)?
            }
            (Smiley::Compose, &[expr1, expr2]) => {
                let mut shapes1 = self
                    .eval_node(&nodes[expr1], nodes)?
                    .into_shapes()
                    .ok_or(EvalError::TypeError)?;
                let shapes2 = self
                    .eval_node(&nodes[expr2], nodes)?
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

/// The primitive components of a [`Picture`].
#[derive(Debug, Clone, Copy)]
pub enum Shape {
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
pub enum Value<T = Id> {
    /// A floating-point number
    Float(f64),
    /// A function and a reference to its body
    Lambda(T),
    /// A collection of shapes
    Shapes(Vec<Shape>),
}

/// A picture, made up of [`Shape`]s
#[derive(Debug, Clone)]
pub struct Picture {
    shapes: Vec<Shape>,
}

/// An error encountered while attempting to evaluate a Smiley expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum EvalError {
    /// A syntax error.
    #[error("syntax error")]
    SyntaxError,
    /// A type error.
    #[error("type error")]
    TypeError,
}

impl<T> AstNode<Smiley, T> {
    fn as_ident(&self) -> Option<Symbol> {
        match *self.operation() {
            Smiley::Ident(ident) => Some(ident),
            _ => None,
        }
    }
}

impl<T> Value<T> {
    fn as_float(&self) -> Option<f64> {
        match *self {
            Self::Float(f) => Some(f),
            _ => None,
        }
    }

    fn as_lambda(&self) -> Option<T>
    where
        T: Copy,
    {
        match *self {
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
    pub fn into_picture(self) -> Option<Picture> {
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

impl Picture {
    /// Output an SVG representation of this picture to the writer `writer`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if writing to `writer` produces an IO error.
    pub fn write_svg<W: Write>(&self, writer: W) -> io::Result<()> {
        let mut xml_writer = EmitterConfig::new()
            .perform_indent(true)
            .create_writer(writer);
        self.fmt_svg(&mut xml_writer).map_err(|e| match e {
            writer::Error::Io(e) => e,
            _ => unreachable!(),
        })
    }

    fn fmt_svg<W: Write>(&self, xml_writer: &mut EventWriter<W>) -> xml::writer::Result<()> {
        xml_writer.write(
            XmlEvent::start_element("svg")
                .default_ns("http://www.w3.org/2000/svg")
                .attr("viewBox", "-25 -25 50 50"),
        )?;
        xml_writer.write(XmlEvent::start_element("style"))?;
        xml_writer.write(XmlEvent::cdata(
            r#"
            svg {
              width: 100vmin;
              height: 100vmin;
              margin: 0 auto;
            }

            circle, line {
              fill: none;
              stroke: black;
              vector-effect: non-scaling-stroke;
            }
           "#,
        ))?;
        xml_writer.write(XmlEvent::end_element())?;

        for shape in &self.shapes {
            shape.fmt_svg(xml_writer)?;
        }

        xml_writer.write(XmlEvent::end_element())
    }
}

impl Shape {
    fn fmt_svg<W: Write>(&self, xml_writer: &mut EventWriter<W>) -> xml::writer::Result<()> {
        match self {
            Self::Circle {
                center: (cx, cy),
                radius: r,
            } => {
                xml_writer.write(
                    XmlEvent::start_element("circle")
                        .attr("cx", &cx.to_string())
                        .attr("cy", &cy.to_string())
                        .attr("r", &r.to_string()),
                )?;
                xml_writer.write(XmlEvent::end_element())
            }
            Self::Line {
                start: (x1, y1),
                end: (x2, y2),
            } => {
                xml_writer.write(
                    XmlEvent::start_element("line")
                        .attr("x1", &x1.to_string())
                        .attr("y1", &y1.to_string())
                        .attr("x2", &x2.to_string())
                        .attr("y2", &y2.to_string()),
                )?;
                xml_writer.write(XmlEvent::end_element())
            }
        }
    }

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
pub fn eval(expr: &RecExpr<AstNode<Smiley>>) -> Result<Value, EvalError> {
    let context = Context::new();
    eval::eval(&context, expr)
}

impl Arity for Smiley {
    fn min_arity(&self) -> usize {
        match self {
            Self::Int(_)
            | Self::Float(_)
            | Self::Ident(_)
            | Self::Var(_)
            | Self::Circle
            | Self::Line => 0,
            Self::Lambda => 1,
            Self::Scale | Self::Rotate | Self::Compose | Self::Apply => 2,
            Self::Move | Self::Let | Self::Lib => 3,
        }
    }
}

impl Display for Smiley {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(n) => n.fmt(f),
            Self::Float(g) => g.fmt(f),
            Self::Ident(s) => s.fmt(f),
            Self::Var(i) => write!(f, "${}", i),
            Self::Circle => f.write_str("circle"),
            Self::Line => f.write_str("line"),
            Self::Move => f.write_str("move"),
            Self::Scale => f.write_str("scale"),
            Self::Rotate => f.write_str("rotate"),
            Self::Compose => f.write_str("+"),
            Self::Apply => f.write_str("apply"),
            Self::Lambda => f.write_str("lambda"),
            Self::Let => f.write_str("let"),
            Self::Lib => f.write_str("lib"),
        }
    }
}

impl FromStr for Smiley {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let kind = match s {
            "circle" => Self::Circle,
            "line" => Self::Line,
            "lambda" => Self::Lambda,
            "scale" => Self::Scale,
            "move" => Self::Move,
            "rotate" => Self::Rotate,
            "apply" => Self::Apply,
            "let" => Self::Let,
            "lib" => Self::Lib,
            "+" => Self::Compose,
            _ => {
                if let Ok(index) = s.parse() {
                    Self::Var(index)
                } else if let Ok(n) = s.parse() {
                    Self::Int(n)
                } else if let Ok(f) = s.parse() {
                    Self::Float(f)
                } else {
                    Self::Ident(s.into())
                }
            }
        };
        Ok(kind)
    }
}

impl Teachable for Smiley {
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T> {
        match binding_expr {
            BindingExpr::Lambda(body) => AstNode::new(Self::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => AstNode::new(Self::Apply, [fun, arg]),
            BindingExpr::Index(index) => AstNode::leaf(Self::Var(DeBruijnIndex(index))),
            BindingExpr::Ident(ident) => AstNode::leaf(Self::Ident(ident)),
            BindingExpr::Lib { ident, value, body } => {
                AstNode::new(Self::Lib, [ident, value, body])
            }
        }
    }

    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
        let binding_expr = match node.as_parts() {
            (Self::Lambda, [body]) => BindingExpr::Lambda(body),
            (Self::Apply, [fun, arg]) => BindingExpr::Apply(fun, arg),
            (&Self::Var(DeBruijnIndex(index)), []) => BindingExpr::Index(index),
            (&Self::Ident(ident), []) => BindingExpr::Ident(ident),
            (Self::Lib, [ident, value, body]) => BindingExpr::Lib { ident, value, body },
            _ => return None,
        };
        Some(binding_expr)
    }
}

impl FreeVars for Smiley {
    fn ident_symbol(&self) -> Option<Symbol> {
        match *self {
            Self::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    fn free_vars(&self, children: &[&HashSet<Symbol>]) -> HashSet<Symbol> {
        match (self, children) {
            (&Self::Ident(ident), []) => {
                let mut result = HashSet::new();
                result.insert(ident);
                result
            }
            (Self::Let | Self::Lib, &[ident, val, body]) => &(body - ident) | val,
            (_, children) => children
                .iter()
                .flat_map(|child| child.iter().copied())
                .collect(),
        }
    }
}

lazy_static! {
    /// Rewrite rules which move containing expressions inside of
    /// [`Smiley::Lib`] expressions.
    static ref LIFT_LIB_REWRITES: &'static [Rewrite<AstNode<Smiley>, FreeVarAnalysis<Smiley>>] = rewrite_rules! {
        // TODO: Check for captures of de Bruijn variables and re-index if necessary.
        lift_lambda: "(lambda (lib ?x ?v ?e))" => "(lib ?x ?v (lambda ?e))";

        // (Effectively) unary operators
        lift_scale: "(scale ?a (lib ?x ?v ?e))" => "(lib ?x ?v (scale ?a ?e))";
        lift_rotate: "(rotate ?a (lib ?x ?v ?e))" => "(lib ?x ?v (rotate ?a ?e))";
        lift_move: "(move ?a ?b (lib ?x ?v ?e))" => "(lib ?x ?v (move ?a ?b ?e))";

        // Binary operators
        lift_compose_both: "(+ (lib ?x ?v ?e1) (lib ?x ?v ?e2))" => "(lib ?x ?v (+ ?e1 ?e2))";
        lift_compose_left: "(+ (lib ?x ?v ?e1) ?e2)" => "(lib ?x ?v (+ ?e1 ?e2))" if not_free_in("?e2", "?x");
        lift_compose_right: "(+ ?e1 (lib ?x ?v ?e2))" => "(lib ?x ?v (+ ?e1 ?e2))" if not_free_in("?e1", "?x");

        lift_apply_both: "(apply (lib ?x ?v ?e1) (lib ?x ?v ?e2))" => "(lib ?x ?v (apply ?e1 ?e2))";
        lift_apply_left: "(apply (lib ?x ?v ?e1) ?e2)" => "(lib ?x ?v (apply ?e1 ?e2))" if not_free_in("?e2", "?x");
        lift_apply_right: "(apply ?e1 (lib ?x ?v ?e2))" => "(lib ?x ?v (apply ?e1 ?e2))" if not_free_in("?e1", "?x");

        // Binding expressions
        lift_let_both: "(let ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
        lift_let_body: "(let ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
        lift_let_binding: "(let ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (let ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");

        lift_lib_both: "(lib ?x1 (lib ?x2 ?v2 ?v1) (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?v2", "?x1");
        lift_lib_body: "(lib ?x1 ?v1 (lib ?x2 ?v2 ?e))" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if and(not_free_in("?v1", "?x2"), not_free_in("?v2", "?x1"));
        lift_lib_binding: "(lib ?x1 (lib ?x2 ?v2 ?v1) ?e)" => "(lib ?x2 ?v2 (lib ?x1 ?v1 ?e))" if not_free_in("?e", "?x2");
    }.leak();
}

/// Execute `EGraph` building and program extraction on a single expression
/// containing all of the programs to extract common fragments out of.
pub fn run_single(runner: Runner<AstNode<Smiley>, FreeVarAnalysis<Smiley>>) {
    // let e1 = runner.egraph.lookup_expr(&"(scale 2 (move 5 7 (rotate 90 line)))".parse().unwrap()).unwrap();
    // let e2 = runner.egraph.lookup_expr(&"(scale 2 (move 5 7 (rotate 90 circle)))".parse().unwrap()).unwrap();
    // let e3 = runner.egraph.lookup_expr(&"(scale 2 (move 5 7 (rotate 90 (scale 3 line))))".parse().unwrap()).unwrap();

    let learned_lib = LearnedLibrary::from(&runner.egraph);
    let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();

    let mut runner = runner.with_iter_limit(1).run(lib_rewrites.iter());

    // runner.egraph.check_goals(e1, &["(lib ?f (lambda (scale 2 (move 5 7 (rotate 90 $0)))) (apply ?f line))".parse().unwrap()]);
    // runner.egraph.check_goals(e2, &["(lib ?f (lambda (scale 2 (move 5 7 (rotate 90 $0)))) (apply ?f circle))".parse().unwrap()]);
    // runner.egraph.check_goals(e3, &["(lib ?f (lambda (scale 2 (move 5 7 (rotate 90 $0)))) (apply ?f (scale 3 line)))".parse().unwrap()]);

    runner.stop_reason = None;
    // eprintln!("{:?}", DebugEGraph::new(&runner.egraph));

    let runner = runner
        .with_iter_limit(30)
        .with_time_limit(core::time::Duration::from_secs(40))
        .run(LIFT_LIB_REWRITES.iter());

    let extractor = Extractor::new(&runner.egraph, AstSize);
    let (cost, expr) = extractor.find_best(runner.roots[0]);

    eprintln!("Cost: {}\n", cost);
    eprintln!("{}", expr.pretty(100));
}
