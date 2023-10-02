use super::step::Step;
use crate::remote::Remote;
use crate::vm::{Interactive, VM};
use std::collections::HashMap;
use std::ops::Range;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    // Given a path, creates a list of steps that remove all the loops by making
    // them trivial (ie equal to the identity)
    pub fn decompose_trivialize_path<It>(&self, path: It) -> (Vec<(usize, usize)>, Vec<Step>)
    where
        It: Iterator<Item = (usize, usize)>,
    {
        let mut path: Vec<(usize, usize)> = path.collect();
        let mut steps: Vec<Step> = Vec::new();
        while let Some(lp) = self.decompose_trivialize_path_find_loop(&path) {
            let step = Step {
                start: (&path[..lp.start]).iter().copied().collect(),
                middle_left: (&path[lp.start..lp.end]).iter().copied().collect(),
                middle_right: Vec::new(),
                end: (&path[lp.end..]).iter().copied().collect(),
            };
            steps.push(step);
            path.splice(lp, std::iter::empty());
        }
        (path, steps)
    }

    // Returns a range in path that corresponds to a loop it there is one
    pub fn decompose_trivialize_path_find_loop(
        &self,
        path: &Vec<(usize, usize)>,
    ) -> Option<Range<usize>> {
        let mut starts: HashMap<usize, usize> = HashMap::new();
        for i in 0..path.len() {
            if let Some(start) = starts.get(&path[i].0) {
                return Some(*start..i);
            }
            starts.insert(path[i].0, i);
        }
        if let Some((src, mph)) = path.last() {
            let dst = self.graph.graph.edges[*src][*mph].0;
            if let Some(start) = starts.get(&dst) {
                return Some(*start..path.len());
            }
        }
        None
    }
}
