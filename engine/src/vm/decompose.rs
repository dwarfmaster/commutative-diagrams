mod planar;
mod step;

use crate::remote::Remote;
use crate::vm::{Interactive, VM};

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    pub fn planar_split(&mut self, face: usize) -> bool {
        // Find left and right sides
        let left = self.graph.faces[face].left.iter().scan(
            self.graph.faces[face].start,
            |st: &mut usize, mph: &usize| {
                let src = *st;
                *st = self.graph.edges[src][*mph].0;
                Some((src, *mph))
            },
        );
        let right = self.graph.faces[face].right.iter().scan(
            self.graph.faces[face].start,
            |st: &mut usize, mph: &usize| {
                let src = *st;
                *st = self.graph.edges[src][*mph].0;
                Some((src, *mph))
            },
        );

        // TODO decide to swap left and right depending on the orientation of
        // the face
        // TODO decide what to do if there are loops on thee target and
        // destination paths. We probably want to add goal to trivialize them,
        // and then keep going with the decomposition as follows.

        // Compute the steps
        let steps = self.planar_split_impl(left, right);
        if steps.is_empty() {
            return false;
        }

        // Realize steps as string
        let steps: String = itertools::Itertools::intersperse(
            steps
                .into_iter()
                .map(|step| self.decompose_step_to_string(step)),
            ";".to_string(),
        )
        .collect();

        // Register action
        self.insert_and_run(&format!(
            "decompose {} {}",
            self.graph.faces[face].label.name, steps
        ));
        true
    }
}
