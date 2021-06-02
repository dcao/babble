use crate::anti_unify::AUAnalysis;
use egg::{Applier, EGraph, Id, Language, Pattern, SearchMatches, Subst, Var};

#[derive(Debug, PartialEq, Clone)]
pub struct EnvPattern<L>(pub Pattern<L>);

impl<L, A> Applier<L, A> for EnvPattern<L>
where
    L: Language,
    A: AUAnalysis<L>,
{
    fn apply_one(&self, egraph: &mut EGraph<L, A>, eclass: Id, subst: &Subst) -> Vec<Id> {
        self.0.apply_one(egraph, eclass, subst)
    }

    fn vars(&self) -> Vec<Var> {
        Pattern::vars(&self.0)
    }

    fn apply_matches(&self, egraph: &mut EGraph<L, A>, matches: &[SearchMatches]) -> Vec<Id> {
        let mut added = vec![];
        for mat in matches {
            for subst in &mat.substs {
                let ids = self
                    .apply_one(egraph, mat.eclass, subst)
                    .into_iter()
                    .filter_map(|id| {
                        if egraph[id].data == egraph[mat.eclass].data {
                            let (to, did_something) = egraph.union(id, mat.eclass);
                            if did_something {
                                Some(to)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    });
                added.extend(ids)
            }
        }
        added
    }
}
