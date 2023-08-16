mod planar;
mod step;
mod trivialize;

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

        // Remove loops
        let (left, left_steps) = self.decompose_trivialize_path(left);
        let (right, right_steps) = self.decompose_trivialize_path(right);

        // TODO decide to swap left and right depending on the orientation of
        // the face

        // Compute the steps
        let steps = self.planar_split_impl(left.into_iter(), right.into_iter());
        if steps.is_empty() {
            return false;
        }

        // Realize steps as string
        let steps = left_steps
            .into_iter()
            .chain(steps.into_iter())
            .chain(right_steps.into_iter().rev().map(|step| step.inv()));
        let steps: String = itertools::Itertools::intersperse(
            steps.map(|step| self.decompose_step_to_string(step)),
            ";".to_string(),
        )
        .collect();
        log::trace!("Decomposing with {}", steps);

        // Register action
        self.insert_and_run(&format!(
            "decompose {} {}",
            self.graph.faces[face].label.name, steps
        ));
        true
    }
}
