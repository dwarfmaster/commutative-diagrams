use crate::remote::Remote;
use crate::vm::{Interactive, VM};

// Store a step
pub struct Step {
    pub start: Vec<(usize, usize)>,
    pub middle_left: Vec<(usize, usize)>,
    pub middle_right: Vec<(usize, usize)>,
    pub end: Vec<(usize, usize)>,
}

impl Step {
    pub fn new() -> Step {
        Step {
            start: Vec::new(),
            middle_left: Vec::new(),
            middle_right: Vec::new(),
            end: Vec::new(),
        }
    }

    pub fn inv(self) -> Step {
        Step {
            start: self.start,
            middle_left: self.middle_right,
            middle_right: self.middle_left,
            end: self.end,
        }
    }
}

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    pub fn decompose_step_to_string(&self, step: Step) -> String {
        let get_name =
            |(src, mph): &(usize, usize)| -> &str { self.graph.edges[*src][*mph].1.name.as_str() };
        let middle: String = std::iter::once("<")
            .chain(itertools::Itertools::intersperse(
                step.middle_left.iter().map(get_name),
                ":",
            ))
            .chain(std::iter::once(";"))
            .chain(itertools::Itertools::intersperse(
                step.middle_right.iter().map(get_name),
                ":",
            ))
            .chain(std::iter::once(">"))
            .collect();
        let step = itertools::Itertools::intersperse(
            step.start
                .iter()
                .map(get_name)
                .chain(std::iter::once(middle.as_str()))
                .chain(step.end.iter().map(get_name)),
            ":",
        )
        .collect();
        step
    }
}
