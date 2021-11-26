use std::collections::{HashMap, HashSet};

use egg::{Analysis, AstSize, EGraph, Id, Language, RecExpr};
use grb::prelude::*;

pub trait LpCostFunction<L: Language, N: Analysis<L>> {
    /// Returns the cost of the given e-node.
    ///
    /// This function may look at other parts of the e-graph to compute the cost
    /// of the given e-node.
    fn node_cost(&mut self, egraph: &EGraph<L, N>, eclass: Id, enode: &L) -> f64;
}

impl<L: Language, N: Analysis<L>> LpCostFunction<L, N> for AstSize {
    fn node_cost(&mut self, _egraph: &EGraph<L, N>, _eclass: Id, _enode: &L) -> f64 {
        1.0
    }
}

/// A structure to perform extraction using integer linear programming.
/// This uses the gurobi solver.
/// You must have it installed on your machine to use this feature.
pub struct LpExtractor<'a, L: Language, N: Analysis<L>> {
    egraph: &'a EGraph<L, N>,
    model: Model,
    vars: HashMap<Id, ClassVars>,
}

#[derive(Debug)]
struct ClassVars {
    active: grb::Var,
    order: grb::Var,
    nodes: Vec<grb::Var>,
}

impl<'a, L, N> LpExtractor<'a, L, N>
where
    L: Language + std::fmt::Display,
    N: Analysis<L>,
{
    /// Create an [`LpExtractor`] using costs from the given [`LpCostFunction`].
    /// See those docs for details.
    pub fn new<CF>(egraph: &'a EGraph<L, N>, mut cost_function: CF) -> Self
    where
        CF: LpCostFunction<L, N>,
    {
        let max_order = egraph.total_number_of_nodes() as f64 * 10.0;

        let env = grb::Env::new("").unwrap();
        let mut model = Model::with_env("model1", &env).unwrap();
        // model.set_param(param::OutputFlag, 0).unwrap();

        let vars: HashMap<Id, ClassVars> = egraph
            .classes()
            .map(|class| {
                let cvars = ClassVars {
                    active: add_binvar!(model, name: &format!("a{}", class.id)).unwrap(),
                    order:
                        add_intvar!(model, name: &format!("o{}", class.id), bounds: 0..max_order)
                            .unwrap(),
                    nodes: class
                        .nodes
                        .iter()
                        .enumerate()
                        .map(|(i, _)| {
                            add_binvar!(model, name: &format!("n{}-{}", class.id, i)).unwrap()
                        })
                        .collect(),
                };
                (class.id, cvars)
            })
            .collect();

        let mut cycles: HashSet<(Id, usize)> = Default::default();
        find_cycles(egraph, |id, i| {
            cycles.insert((id, i));
        });

        for (&id, class) in &vars {
            // class active == some node active
            // sum(for node_active in class) == class_active
            let ns = (&class.nodes).grb_sum();
            model.add_constr("", c!(ns == class.active)).unwrap();

            for (i, (node, &node_active)) in egraph[id].iter().zip(&class.nodes).enumerate() {
                if cycles.contains(&(id, i)) {
                    model.add_constr("", c!(node_active == 0.0)).unwrap();
                    continue;
                }

                for child in node.children() {
                    let child_active = vars[child].active;
                    // node active implies child active, encoded as:
                    //   node_active <= child_active
                    //   node_active - child_active <= 0
                    model.add_constr("", c!(node_active <= child_active)).unwrap();
                }
            }
        }

        // cost is the weighted sum of all the nodes
        let mut cost: Expr = 0.into();

        for class in egraph.classes() {
            for (node, &node_active) in class.iter().zip(&vars[&class.id].nodes) {
                cost = cost + node_active * cost_function.node_cost(egraph, class.id, node)
            }
        }

        model.set_objective(cost, Minimize).unwrap();

        Self {
            egraph,
            model,
            vars,
        }
    }

    /// Set the cbc timeout in seconds.
    pub fn timeout(&mut self, seconds: f64) -> &mut Self {
        // self.model.set_parameter("seconds", &seconds.to_string());
        self.model
            .get_env_mut()
            .set(param::TimeLimit, seconds)
            .unwrap();

        self
    }

    /// Extract a single rooted term.
    ///
    /// This is just a shortcut for [`LpExtractor::solve_multiple_using`].
    pub fn solve(&mut self, root: Id) -> RecExpr<L> {
        self.solve_multiple(&[root]).0
    }

    /// Extract (potentially multiple) roots
    pub fn solve_multiple(&mut self, roots: &[Id]) -> (RecExpr<L>, Vec<Id>) {
        let egraph = self.egraph;

        for root in roots {
            let root = &self.vars[&egraph.find(*root)];
            self.model.add_constr("", c!(root.active == 1.0)).unwrap();
            // model.add_constr("", c!(root.order == 0.0)).unwrap();
        }

        self.model.optimize().unwrap();
        log::info!(
            "Gurobi status {:?}",
            self.model.status()
        );

        let mut todo: Vec<Id> = roots.iter().copied().collect();
        let mut expr = RecExpr::default();
        // converts e-class ids to e-node ids
        let mut ids: HashMap<Id, Id> = HashMap::default();

        while let Some(&id) = todo.last() {
            if ids.contains_key(&id) {
                todo.pop();
                continue;
            }
            let id = egraph.find(id);
            let v = &self.vars[&id];
            assert!(self.model.get_obj_attr(attr::X, &v.active).unwrap() > 0.0);
            let node_idx = v.nodes.iter().position(|n| self.model.get_obj_attr(attr::X, n).unwrap() > 0.0).unwrap();
            let node = &self.egraph[id].nodes[node_idx];
            if node.all(|child| ids.contains_key(&child)) {
                let new_id = expr.add(node.clone().map_children(|i| ids[&i]));
                ids.insert(id, new_id);
                todo.pop();
            } else {
                todo.extend_from_slice(node.children())
            }
        }

        let root_idxs = roots.iter().map(|root| ids[&egraph.find(*root)]).collect();

        assert!(expr.is_dag(), "LpExtract found a cyclic term!: {:?}", expr);
        (expr, root_idxs)
    }
}

fn find_cycles<L, N>(egraph: &EGraph<L, N>, mut f: impl FnMut(Id, usize))
where
    L: Language,
    N: Analysis<L>,
{
    enum Color {
        White,
        Gray,
        Black,
    }
    type Enter = bool;

    let mut color: HashMap<Id, Color> = egraph.classes().map(|c| (c.id, Color::White)).collect();
    let mut stack: Vec<(Enter, Id)> = egraph.classes().map(|c| (true, c.id)).collect();

    while let Some((enter, id)) = stack.pop() {
        if enter {
            *color.get_mut(&id).unwrap() = Color::Gray;
            stack.push((false, id));
            for (i, node) in egraph[id].iter().enumerate() {
                for child in node.children() {
                    match &color[child] {
                        Color::White => stack.push((true, *child)),
                        Color::Gray => f(id, i),
                        Color::Black => (),
                    }
                }
            }
        } else {
            *color.get_mut(&id).unwrap() = Color::Black;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::LpExtractor;
    use egg::{SymbolLang as S, *};

    #[test]
    fn simple_lp_extract_two() {
        let mut egraph = EGraph::<S, ()>::default();
        let a = egraph.add(S::leaf("a"));
        let plus = egraph.add(S::new("+", vec![a, a]));
        let f = egraph.add(S::new("f", vec![plus]));
        let g = egraph.add(S::new("g", vec![plus]));

        let mut ext = LpExtractor::new(&egraph, AstSize);
        ext.timeout(10.0); // way too much time
        let (exp, ids) = ext.solve_multiple(&[f, g]);
        println!("{:?}", exp);
        println!("{}", exp);
        assert_eq!(exp.as_ref().len(), 4);
        assert_eq!(ids.len(), 2);
    }
}
