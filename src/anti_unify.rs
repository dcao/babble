//! Implements anti-unification between eclasses.

// This code is heavily adapted from the unscramble code:
// github.com/uwplse/unscramble
// with the main difference being that this code tries to anti-unify eclasses, rather than
// trying to anti-unify egraphs.
// Their code is licensed under MIT; it is reproduced below:
//
// MIT License
//
// Copyright (c) 2020 UW PLSE
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

/*

1. Construct mapping from cross product of ids to new ids in intersection. (id1, id2) -> id3
2. Construct a vector of vectors of enodes, with identical enode operators (using L.matches())???
3. Construct cross products of each [vector of enodes with identical ops] in the vector. Use mapping from step 1 to construct new child ids and destination eclass

[*(q2,q3)->q1, *(q3,q2)->q1] in egraph1: *_2 -> [([q2, q3], q1), ([q3, q2], q1)]

[*(r2,r3)->r1, *(r3,r2)->r1] in egraph2: *_2 -> [([r2, r3], r1), ([r3,r2], r1)]

intersection: *_2 -> [([q2r2, q3r3], q1r1)]

[
  *(q2,q3)->q1 x *(r2,r3)->r1 -> *(q2r2, q3r3)->q1r1
]

[defer] cycles might be bad since we need to do bottom up

*/

// For eclass anti-unification, we have to change some things about the algorithm.
// 1. We have to do more complex intersection to get places where we should insert
// arguments.
// After doing our intersection, if a referenced state doesn't exist, we have to create it
// as an argument.
// 2. We add an extra step after intersection which introduces the new intersected egraph as
// a let-bound function; the eclasses should each contain a new AST node which applies
// the let-bound function to the correct arguments.
// 3. We want to add some procedure to automatically pick eclasses to anti-unify. Our rule
// should be that we shouldn't try to anti-unify an eclass with another eclass which is a
// transitive child of the first eclass. (we only want to anti-unify distinct programs)

use egg::{Analysis, EClass, EGraph, Id, Language};
use indexmap::IndexMap;
use std::{collections::HashMap, string::String};

// type Transition = (Vec<Id>, Id);
type Transition<L> = (L, Id);
type ProdTransition<L> = (Transition<L>, Transition<L>);
// We use an IndexMap since we want to iterate from root to leaf in the eclasses.
type ProdWorklist<L> = IndexMap<String, Vec<ProdTransition<L>>>;

fn enode_children<L: Language>(enode: &L) -> Vec<Id> {
    let mut children = Vec::with_capacity(enode.len());
    enode.for_each(|child| {
        children.push(child);
    });
    children
}

fn enode_hash<L: Language>(enode: &L) -> String {
    format!("{}_{}", enode.display_op(), enode.len())
}

// We return an IndexMap so that all of the leaf nodes (nodes which don't
// reference other eclasses) are at the bottom.
fn enode_map<L: Language, N: Analysis<L>>(
    g: &EGraph<L, N>,
    s: &EClass<L, N::Data>,
) -> IndexMap<String, Vec<Transition<L>>> {
    let mut map: IndexMap<String, Vec<Transition<L>>> = IndexMap::new();
    // The list of eclasses we still need to visit.
    let mut wrk: Vec<&EClass<L, N::Data>> = vec![s];
    while let Some(class) = wrk.pop() {
        for node in class.iter() {
            let hash = enode_hash(node);
            // TODO: do we need to keep track of nodes/classes we've already seen?
            let vals = map.entry(hash).or_insert_with(Vec::new);
            // vals.push((node.children().to_vec(), class.id));
            vals.push((node.clone(), class.id));

            for child in enode_children(node) {
                wrk.push(&g[child]);
            }
        }
    }
    map
}

/// Compute the intersection of two [`EClass`]es.
pub fn intersect<L: Language, N: Analysis<L>>(
    g: &EGraph<L, N>,
    a: &EClass<L, N::Data>,
    b: &EClass<L, N::Data>,
) -> EGraph<L, ()> {
    let map_a = enode_map(g, a);
    println!("A map:");
    for (key, value) in &map_a {
        println!("  {}: {:?}", key, value);
    }
    let map_b = enode_map(g, b);
    println!("B map:");
    for (key, value) in &map_b {
        println!("  {}: {:?}", key, value);
    }

    // construct ProdWorklist
    // Since we're anti-unifying eclasses within the same egraph, we know that
    // references to identical expressions will have the same Id. (i.e. any
    // given expression won't appear twice with two different Ids in a given
    // egraph). Because of this, to anti-unify, we only want to push entries
    // where the enodes have different Ids.
    let mut worklist: ProdWorklist<L> = ProdWorklist::new();
    for (key, value_a) in &map_a {
        if let Some(value_b) = map_b.get(key) {
            for (t_a, t_b) in itertools::iproduct!(value_a.iter(), value_b.iter()) {
                if t_a.1 != t_b.1 {
                    let vals = worklist.entry(key.clone()).or_insert(vec![]);
                    vals.push((t_a.clone(), t_b.clone()));
                }
            }
        }
    }

    println!("generated worklist: {:?}", worklist);

    let mut intersection: EGraph<L, ()> = EGraph::new(()); /* TODO: transfer analysis */
    let mut prod_ids: HashMap<(Id, Id), Id> = HashMap::new();
    let mut did_something = true;

    while did_something {
        did_something = false;
        for value in worklist.values_mut() {
            let mut finished_idxs = vec![];
            for (idx, ((en1, parent_ec1), (en2, parent_ec2))) in value.iter().enumerate() {
                let children_pairs: Vec<_> = enode_children(en1)
                    .into_iter()
                    .zip(enode_children(en2))
                    .map(|(c1, c2)| prod_ids.get(&(c1, c2)))
                    .collect();
                if children_pairs.iter().all(Option::is_some) {
                    println!("map: {:?}", prod_ids);
                    /* add to new egraph */
                    let new_children: Vec<Id> = children_pairs
                        .into_iter()
                        .flatten()
                        .copied()
                        .collect();
                    // for (c1, c2) in en1.children().iter().zip(en2.children()) {
                    //   new_children.insert(c1, prod_ids[&(*c1, *c2)]);
                    // }
                    // let new_en1 = en1.clone().map_children(|c1| new_children[&c1]);
                    let mut new_en1 = en1.clone();
                    let mut i = 0;
                    new_en1.update_children(|_child| {
                        let new_child = new_children[i];
                        i += 1;
                        new_child
                    });

                    println!("Adding node: {:?}", &new_en1);
                    println!("  old -> new: {:?}", new_children);
                    let prod_parent = intersection.add(new_en1);
                    /* for (_, idv) in prod_ids.iter_mut() {
                      *idv = intersection.find(*idv);
                    } */
                    println!(
                        "  from: {:?}[{}] {:?}[{}]",
                        &en1, parent_ec1, &en2, parent_ec2
                    );
                    println!("  new parent: {}", &prod_parent);
                    did_something = true;
                    finished_idxs.push(idx);
                    let parent_inhabited = prod_ids.get(&(*parent_ec1, *parent_ec2));
                    if let Some(intersected_parent) = parent_inhabited {
                        println!("Merging {} and {}", prod_parent, intersected_parent);
                        let (new_parent, _) = intersection.union(prod_parent, *intersected_parent);
                        // go through prod_ids map and recanonicalize
                        /* for (_, idv) in prod_ids.iter_mut() {
                          *idv = intersection.find(*idv);
                        } */
                        prod_ids.insert((*parent_ec1, *parent_ec2), new_parent);
                        println!("  to {}", new_parent);
                    } else {
                        prod_ids.insert((*parent_ec1, *parent_ec2), prod_parent);
                    }
                }
            }
            while let Some(idx) = finished_idxs.pop() {
                value.remove(idx);
            }
        }
    }
    intersection
}

#[cfg(test)]
mod tests {
    use super::intersect;
    use crate::smiley_lang::Rewrite;
    use egg::Runner;

    #[test]
    fn test_anti_unif_1() {
        // Our list of rewrite rules is here
        let rules: &[Rewrite] = &[];

        // First, parse the expression and build an egraph from it
        let expr = "(+ (scale 0.5 (+ line circle)) (+ circle circle))"
            .parse()
            .unwrap();
        let runner = Runner::default()
            // .with_scheduler(SimpleScheduler)
            // .with_iter_limit(1_000)
            // .with_node_limit(1_000_000)
            // .with_time_limit(core::time::Duration::from_secs(20))
            .with_expr(&expr)
            .run(rules);
        let (egraph, _root) = (runner.egraph, runner.roots[0]);

        println!(
            "{:#?}",
            intersect(&egraph, &egraph[4.into()], &egraph[5.into()])
        );
    }
}
