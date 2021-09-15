//! The AST defining the smiley language.

use crate::{
    ast_node::{Arity, AstNode},
    eval::{self, Eval},
    learn::LearnedLibrary,
    teachable::Teachable,
};
use babble_macros::rewrite_rules;
use egg::{
    Analysis, AstSize, Condition, EClass, EGraph, Extractor, Id, Language, RecExpr, Rewrite,
    Runner, Subst, Symbol,
};
use lazy_static::lazy_static;
use ordered_float::NotNan;
use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    convert::TryInto,
    fmt::{self, Display, Formatter},
    io::{self, Write},
    iter::FromIterator,
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
    Var(usize),
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

impl<T: Clone> Eval<T> for Context<T> {
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
        let result = match node.as_parts() {
            (Smiley::Int(i), []) => Value::float(*i),
            (Smiley::Float(f), []) => Value::float(*f),
            (Smiley::Circle, []) => Value::circle((0.0, 0.0), 1.0),
            (Smiley::Line, []) => Value::line((-0.5, 0.0), (0.5, 0.0)),
            (Smiley::Ident(ident), []) => self.ident_env[ident].clone(),
            (Smiley::Var(index), []) => self.arg_env[*index].clone(),
            (Smiley::Lambda, [body]) => Value::lambda(*body),
            (Smiley::Move, [x_offset, y_offset, expr]) => {
                let x_offset: f64 = self
                    .eval_node(&nodes[*x_offset], nodes)?
                    .into_float()
                    .ok_or(EvalError::TypeError)?;
                let y_offset: f64 = self
                    .eval_node(&nodes[*y_offset], nodes)?
                    .into_float()
                    .ok_or(EvalError::TypeError)?;
                let val = self.eval_node(&nodes[*expr], nodes)?;
                val.translate(x_offset, y_offset)
            }
            (Smiley::Scale, [factor, expr]) => {
                let factor = self
                    .eval_node(&nodes[*factor], nodes)?
                    .into_float()
                    .ok_or(EvalError::TypeError)?;
                let val = self.eval_node(&nodes[*expr], nodes)?;
                val.scale(factor)
            }
            (Smiley::Rotate, [angle, expr]) => {
                let angle = self
                    .eval_node(&nodes[*angle], nodes)?
                    .into_float()
                    .ok_or(EvalError::TypeError)?;
                let val = self.eval_node(&nodes[*expr], nodes)?;
                val.rotate(angle)
            }
            (Smiley::Let | Smiley::Lib, [ident, val, body]) => {
                let ident = nodes[*ident].as_ident().ok_or(EvalError::SyntaxError)?;
                let val = self.eval_node(&nodes[*val], nodes)?;
                self.clone()
                    .with_ident(ident, val)
                    .eval_node(&nodes[*body], nodes)?
            }
            (Smiley::Apply, [fun, arg]) => {
                let body = self
                    .eval_node(&nodes[*fun], nodes)?
                    .into_lambda()
                    .ok_or(EvalError::TypeError)?;
                let arg = self.eval_node(&nodes[*arg], nodes)?;
                self.clone().with_arg(arg).eval_node(&nodes[body], nodes)?
            }
            (Smiley::Compose, [expr1, expr2]) => {
                let val1 = self.eval_node(&nodes[*expr1], nodes)?;
                let val2 = self.eval_node(&nodes[*expr2], nodes)?;
                val1.compose(val2)
            }
            _ => unreachable!(),
        };
        Ok(result)
    }
}

#[derive(Debug, Clone)]
enum SimpleValue<T = Id> {
    Float(f64),
    Circle { center: (f64, f64), radius: f64 },
    Line { from: (f64, f64), to: (f64, f64) },
    Lambda(T),
}

/// The result of evaluating a smiley expression.
#[derive(Debug, Clone)]
pub struct Value<T = Id> {
    simple_values: Vec<SimpleValue<T>>,
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
        match self.operation() {
            Smiley::Ident(ident) => Some(*ident),
            _ => None,
        }
    }
}

impl<T> Value<T> {
    fn lambda(body: T) -> Self {
        Self {
            simple_values: vec![SimpleValue::Lambda(body)],
        }
    }

    fn float<F: Into<f64>>(value: F) -> Self {
        Self {
            simple_values: vec![SimpleValue::Float(value.into())],
        }
    }

    fn circle(center: (f64, f64), radius: f64) -> Self {
        Self {
            simple_values: vec![SimpleValue::Circle { center, radius }],
        }
    }

    fn line(from: (f64, f64), to: (f64, f64)) -> Self {
        Self {
            simple_values: vec![SimpleValue::Line { from, to }],
        }
    }

    fn into_float(self) -> Option<f64> {
        self.into_single().and_then(SimpleValue::into_float)
    }

    fn into_lambda(self) -> Option<T> {
        self.into_single().and_then(SimpleValue::into_lambda)
    }

    fn into_single(self) -> Option<SimpleValue<T>> {
        let [simple_value]: [_; 1] = self.simple_values.try_into().ok()?;
        Some(simple_value)
    }

    /// Write this value as an SVG to the given writer.
    ///
    /// # Errors
    ///
    /// Returns `Err` if writing to `writer` produces an IO error.
    pub fn write_svg<W: Write>(&self, writer: W) -> io::Result<()> {
        let mut xml_writer = EmitterConfig::new()
            .perform_indent(true)
            .create_writer(writer);
        self.to_svg(&mut xml_writer).map_err(|e| match e {
            writer::Error::Io(e) => e,
            _ => unreachable!(),
        })
    }

    fn to_svg<W: Write>(&self, xml_writer: &mut EventWriter<W>) -> writer::Result<()> {
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

        for simple_value in &self.simple_values {
            simple_value.to_svg(xml_writer)?;
        }

        xml_writer.write(XmlEvent::end_element())
    }

    fn translate(self, x_offset: f64, y_offset: f64) -> Self {
        Self {
            simple_values: self
                .simple_values
                .into_iter()
                .map(|v| v.translate(x_offset, y_offset))
                .collect(),
        }
    }

    fn rotate(self, angle: f64) -> Self {
        Self {
            simple_values: self
                .simple_values
                .into_iter()
                .map(|v| v.rotate(angle))
                .collect(),
        }
    }

    fn scale(self, factor: f64) -> Self {
        Self {
            simple_values: self
                .simple_values
                .into_iter()
                .map(|v| v.scale(factor))
                .collect(),
        }
    }

    fn compose(mut self, other: Self) -> Self {
        self.simple_values.extend(other.simple_values);
        self
    }
}

impl<T> SimpleValue<T> {
    fn into_float(self) -> Option<f64> {
        match self {
            Self::Float(f) => Some(f),
            _ => None,
        }
    }

    fn into_lambda(self) -> Option<T> {
        match self {
            Self::Lambda(body) => Some(body),
            _ => None,
        }
    }

    fn to_svg<W: Write>(&self, xml_writer: &mut EventWriter<W>) -> writer::Result<()> {
        match self {
            Self::Float(_) => panic!("float"),
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
                from: (x1, y1),
                to: (x2, y2),
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
            Self::Lambda(_) => panic!("lambda"),
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
                from: (x1, y1),
                to: (x2, y2),
            } => Self::Line {
                from: transform_point(x1, y1),
                to: transform_point(x2, y2),
            },
            other => other,
        }
    }

    fn translate(self, x_offset: f64, y_offset: f64) -> Self {
        self.similarity_transform(|x, y| (x + x_offset, y + y_offset))
    }

    fn rotate(self, angle: f64) -> Self {
        let (sin, cos) = angle.to_radians().sin_cos();
        // Matrix multiplication
        // |cos -sin| |x|
        // |sin  cos| |y|
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
    fn arity(&self) -> usize {
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
                if let Some(i) = s.strip_prefix('$') {
                    Self::Var(i.parse()?)
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
    fn lambda<T>(body: T) -> AstNode<Self, T> {
        AstNode::from_parts(Self::Lambda, [body])
    }

    fn apply<T>(fun: T, arg: T) -> AstNode<Self, T> {
        AstNode::from_parts(Self::Apply, [fun, arg])
    }

    fn var<T>(index: usize) -> AstNode<Self, T> {
        AstNode::from_parts(Self::Var(index), [])
    }

    fn ident<T>(name: Symbol) -> AstNode<Self, T> {
        AstNode::from_parts(Self::Ident(name), [])
    }

    fn lib<T>(name: T, fun: T, body: T) -> AstNode<Self, T> {
        AstNode::from_parts(Self::Lib, [name, fun, body])
    }
}

/// Analysis which maintains a set of potentially free variables for each
/// e-class. For example, the set of potentially free variables for an e-class
/// representing the expressions `(app f circle)`, `(scale 1 x)`, and `line`
/// will be `{f, x}`.
#[derive(Default, Clone, Copy, Debug)]
pub struct SmileyAnalysis;

impl Analysis<AstNode<Smiley>> for SmileyAnalysis {
    type Data = HashSet<Symbol>;

    /// Set `a` to the union of `a` and `b`.
    fn merge(&self, a: &mut Self::Data, b: Self::Data) -> Option<Ordering> {
        if a.is_subset(&b) {
            if a.is_superset(&b) {
                Some(Ordering::Equal)
            } else {
                *a = b;
                Some(Ordering::Less)
            }
        } else if a.is_superset(&b) {
            Some(Ordering::Greater)
        } else {
            a.extend(b.into_iter());
            None
        }
    }

    /// Return all variables potentially free in `enode`.
    fn make(egraph: &EGraph<AstNode<Smiley>, Self>, enode: &AstNode<Smiley>) -> Self::Data {
        match enode.operation() {
            Smiley::Ident(var) => HashSet::from_iter([*var]),
            Smiley::Let | Smiley::Lib => {
                let [var, a, b]: [Id; 3] = enode.children().try_into().unwrap();
                let mut free = egraph[b].data.clone();
                for var in &egraph[var].data {
                    free.remove(var);
                }
                free.extend(&egraph[a].data);
                free
            }
            _ => enode
                .children()
                .iter()
                .flat_map(|child| &egraph[*child].data)
                .copied()
                .collect(),
        }
    }
}

/// Produces a `Condition` which is true if and only if the `Condition`s `p` and
/// `q` are both true. If `p` is false, this condition short-circuits and does
/// not check `q`.
fn and<L, A, P, Q>(p: P, q: Q) -> impl Condition<L, A>
where
    L: Language,
    A: Analysis<L>,
    P: Condition<L, A>,
    Q: Condition<L, A>,
{
    move |egraph: &mut EGraph<L, A>, id: Id, subst: &Subst| {
        p.check(egraph, id, subst) && q.check(egraph, id, subst)
    }
}

/// Produces a [`Condition`] which is true if and only if the variable matched
/// by `var` is not potentially free in the expression matched by `expr`. Both
/// `expr` and `var` must be pattern variables (e.g. "?e" and "?x").
///
/// # Panics
/// Panics if `var` matches something other than a single symbol.
fn not_free_in(
    expr: &'static str,
    var: &'static str,
) -> impl Condition<AstNode<Smiley>, SmileyAnalysis> {
    fn get_var_sym<D>(eclass: &EClass<AstNode<Smiley>, D>) -> Option<Symbol> {
        if eclass.nodes.len() == 1 {
            if let Smiley::Ident(var_sym) = eclass.nodes[0].operation() {
                return Some(*var_sym);
            }
        }
        None
    }

    let var_metavar = var.parse().unwrap();
    let expr_metavar = expr.parse().unwrap();
    move |egraph: &mut EGraph<_, SmileyAnalysis>, _, subst: &Subst| {
        let var_eclass = &egraph[subst[var_metavar]];
        let var_sym = get_var_sym(var_eclass).expect("not a variable");
        let free_vars = &egraph[subst[expr_metavar]].data;
        !free_vars.contains(&var_sym)
    }
}

lazy_static! {
    /// Rewrite rules which move containing expressions inside of
    /// [`Smiley::Lib`] expressions.
    static ref LIFT_LIB_REWRITES: &'static [Rewrite<AstNode<Smiley>, SmileyAnalysis>] = rewrite_rules! {
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
pub fn run_single(runner: Runner<AstNode<Smiley>, SmileyAnalysis>) {
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
