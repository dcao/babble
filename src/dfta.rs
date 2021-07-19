use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use itertools::Itertools;

/// {tag}({states}) -> {state}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Transition<Tag, State>(Tag, Vec<State>, State);

#[derive(Debug, Clone)]
pub(crate) struct Dfta<Tag, State>
where
    Tag: Copy + Eq + Hash,
    State: Copy + Eq + Hash,
{
    states: HashSet<State>,

    // Transitions, grouped by tag
    // Map<tag, Map<output_state, input_children>>
    transitions: HashMap<Tag, HashMap<State, HashSet<Vec<State>>>>,
}

impl<Tag, State> Default for Dfta<Tag, State>
where
    Tag: Copy + Eq + Hash,
    State: Copy + Eq + Hash,
{
    fn default() -> Self {
        Self {
            states: Default::default(),
            transitions: Default::default(),
        }
    }
}

impl<Tag, State> Dfta<Tag, State>
where
    Tag: Copy + Eq + Hash,
    State: Copy + Eq + Hash,
{
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn iter_states(&self) -> impl Iterator<Item = State> + '_ {
        self.states.iter().copied()
    }

    pub(crate) fn iter_transitions(&self) -> impl Iterator<Item = Transition<Tag, State>> + '_ {
        self.transitions.iter().flat_map(|(tag, states)| {
            states.iter().flat_map(move |(output, inputs)| {
                inputs
                    .iter()
                    .map(move |children| Transition(*tag, children.to_vec(), *output))
            })
        })
    }

    pub(crate) fn add_state(&mut self, state: State) {
        self.states.insert(state);
    }

    pub(crate) fn add_transition(&mut self, transition: Transition<Tag, State>) {
        let Transition(tag, children, output) = transition;
        self.transitions
            .entry(tag)
            .or_default()
            .entry(output)
            .or_default()
            .insert(children);
    }

    pub(crate) fn intersection<OtherState: Copy + Eq + Hash>(
        &self,
        other: &Dfta<Tag, OtherState>,
    ) -> Dfta<Tag, (State, OtherState)> {
        let mut states: HashSet<(State, OtherState)> =
            HashSet::with_capacity(self.states.len() * other.states.len());
        for state1 in &self.states {
            for state2 in &other.states {
                states.insert((*state1, *state2));
            }
        }

        let mut transitions: HashMap<
            Tag,
            HashMap<(State, OtherState), HashSet<Vec<(State, OtherState)>>>,
        > = HashMap::with_capacity(max(self.transitions.len(), other.transitions.len()));
        for (tag, state_map1) in &self.transitions {
            if let Some(state_map2) = other.transitions.get(tag) {
                let state_map = transitions.entry(*tag).or_default();
                for (output1, inputs1) in state_map1 {
                    for (output2, inputs2) in state_map2 {
                        let inputs = state_map.entry((*output1, *output2)).or_default();
                        for children1 in inputs1 {
                            for children2 in inputs2 {
                                if children1.len() == children2.len() {
                                    let children = children1
                                        .iter()
                                        .copied()
                                        .zip(children2.iter().copied())
                                        .collect();
                                    inputs.insert(children);
                                }
                            }
                        }
                    }
                }
            }
        }

        Dfta {
            states,
            transitions,
        }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;

    #[test]
    fn test_dfta() {
        // foo(1, 2) -> 3
        // bar(3, 2) -> 1
        let mut dfta = Dfta::new();

        let states = vec![1, 2, 3];
        for state in states {
            dfta.add_state(state);
        }

        let transitions = vec![
            Transition("foo", vec![1, 2], 3),
            Transition("bar", vec![3, 2], 1),
        ];
        for transition in transitions {
            dfta.add_transition(transition);
        }
    }

    #[test]
    fn test_intersection() {
        // foo(1, 2) -> 3
        // bar(3, 2) -> 1
        let mut dfta1 = Dfta::new();

        let states1 = vec![1, 2, 3];
        for state in states1 {
            dfta1.add_state(state);
        }

        let transitions1 = vec![
            Transition("foo", vec![1, 2], 3),
            Transition("bar", vec![3, 2], 1),
        ];
        for transition in transitions1 {
            dfta1.add_transition(transition);
        }

        let mut dfta2 = Dfta::new();

        // foo('a', 'b') -> 'c'
        // baz('b', 'a') -> 'c'
        let states2 = vec!['a', 'b', 'c'];
        for state in states2 {
            dfta2.add_state(state);
        }

        let transitions2 = vec![
            Transition("foo", vec!['a', 'b'], 'c'),
            Transition("baz", vec!['b', 'a'], 'c'),
        ];
        for transition in transitions2 {
            dfta2.add_transition(transition);
        }

        // foo((1, 'a'), (2, 'b')) -> (3, 'c')
        let dfta3 = dfta1.intersection(&dfta2);
        assert_eq!(
            dfta3.iter_transitions().collect::<Vec<_>>(),
            vec![Transition("foo", vec![(1, 'a'), (2, 'b')], (3, 'c'))]
        );
    }

    #[test]
    fn test_self_intersection() {
        // foo(1, 2) -> 3
        // foo(2, 2) -> 4
        // bar(3, 2) -> 1
        let mut dfta = Dfta::new();

        let states = vec![1, 2, 3, 4];
        for state in states {
            dfta.add_state(state);
        }

        let transitions = vec![
            Transition("foo", vec![1, 2], 3),
            Transition("foo", vec![2, 2], 4),
            Transition("bar", vec![3, 2], 1),
        ];
        for transition in transitions {
            dfta.add_transition(transition);
        }

        let self_intersection = dfta.intersection(&dfta);
        assert_eq!(
            self_intersection.iter_transitions().collect::<HashSet<_>>(),
            vec![
                Transition("foo", vec![(1, 1), (2, 2)], (3, 3)),
                Transition("foo", vec![(2, 2), (2, 2)], (4, 4)),
                Transition("bar", vec![(3, 3), (2, 2)], (1, 1)),

                Transition("foo", vec![(1, 2), (2, 2)], (3, 4)),
                Transition("foo", vec![(2, 1), (2, 2)], (4, 3)),
            ].into_iter().collect::<HashSet<_>>()
        );
    }
}
