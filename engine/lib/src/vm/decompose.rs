mod orient;
mod planar;
mod step;
mod trivialize;

use crate::remote::Remote;
use crate::vm::{Interactive, VM};
use step::Step;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
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
        let (mut left, left_steps) = self.decompose_trivialize_path(left);
        let (mut right, right_steps) = self.decompose_trivialize_path(right);

        // Check orientation
        let orient = self.decompose_orient_sides(left.iter().copied(), right.iter().copied());
        log::trace!("Orientation: {}", orient);
        if !orient {
            std::mem::swap(&mut left, &mut right);
        }

        // Compute the steps
        let steps = self.planar_split_impl(left.into_iter(), right.into_iter());
        let steps: Vec<Step> = if orient {
            left_steps
                .into_iter()
                .chain(steps.into_iter())
                .chain(right_steps.into_iter().rev().map(|step| step.inv()))
                .collect()
        } else {
            left_steps
                .into_iter()
                .chain(steps.into_iter().rev().map(|step| step.inv()))
                .chain(right_steps.into_iter().rev().map(|step| step.inv()))
                .collect()
        };
        if steps.is_empty()
            || (steps.len() == 1 && steps[0].start.is_empty() && steps[0].end.is_empty())
        {
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
        log::trace!("Decomposing with steps {}", steps);
        self.insert_and_run(&format!(
            "decompose {} {}",
            self.graph.faces[face].label.name, steps
        ));
        true
    }
}
