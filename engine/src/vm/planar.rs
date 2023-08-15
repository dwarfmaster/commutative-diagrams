use crate::remote::Remote;
use crate::vm::{Interactive, VM};

// Store a step
struct Step {
    start: Vec<(usize, usize)>,
    middle_left: Vec<(usize, usize)>,
    middle_right: Vec<(usize, usize)>,
    end: Vec<(usize, usize)>,
}

impl Step {
    fn new() -> Step {
        Step {
            start: Vec::new(),
            middle_left: Vec::new(),
            middle_right: Vec::new(),
            end: Vec::new(),
        }
    }
}

// Given a starting (exclusive) and end (inclusive) angle,
// compute a range of ids in the out_angles vector that corresponds to the
// outgoing edges between these angles.
fn range_for_angles(
    node: usize,
    out_angles: &mut Vec<Option<Vec<(f32, usize)>>>,
    min: f32,
    max: f32,
) -> Box<dyn Iterator<Item = usize>> {
    let start_id = out_angles[node]
        .as_ref()
        .unwrap()
        .binary_search_by(|(angle, _)| angle.partial_cmp(&min).unwrap())
        .map(|id| id + 1)
        .unwrap_or_else(|id| id);
    let end_id = out_angles[node]
        .as_ref()
        .unwrap()
        .binary_search_by(|(angle, _)| angle.partial_cmp(&max).unwrap())
        .map(|id| id + 1)
        .unwrap_or_else(|id| id);
    if max < min {
        Box::new((start_id..out_angles[node].as_ref().unwrap().len()).chain(0..end_id))
    } else {
        Box::new(start_id..end_id)
    }
}

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    // Given a face, decompose it along its planar faces using the layout. If
    // the layout is not planar, results may be unexpected, but not false. If
    // the layout is planar but the orientation of edges prevent a complete
    // decomposition, the decomposition should be a best-effort.
    pub fn planar_split(&mut self, face: usize) {
        // TODO decide to swap left and right depending on the orientation of
        // the face
        // TODO decide what to do if there are loops on thee target and
        // destination paths. We probably want to add goal to trivialize them,
        // and then keep going with the decomposition as follows.

        // The path is triplets of src, dst and mph ids. The dst of an entry
        // should be the src of the next one.
        let mut actual: Vec<(usize, usize, usize)> = self.graph.faces[face]
            .left
            .iter()
            .scan(
                self.graph.faces[face].start,
                |st: &mut usize, mph: &usize| {
                    let src = *st;
                    let dst = self.graph.edges[src][*mph].0;
                    *st = dst;
                    Some((src, dst, *mph))
                },
            )
            .collect();

        // For each node in the right sides, we remember the angle corresponding
        // to the outgoing edge, to use as max angle when exploring, thus making
        // sure we do not leave the face
        let mut max_angles: Vec<Option<f32>> = vec![None; self.graph.nodes.len()];
        {
            let mut src = self.graph.faces[face].start;
            for mph in &self.graph.faces[face].right {
                max_angles[src] = Some(self.compute_angle(
                    self.graph.nodes[src].2.pos.unwrap(),
                    self.graph.edges[src][*mph].1.control.unwrap(),
                ));
                src = self.graph.edges[src][*mph].0;
            }
        }

        // For each node, lazily order the outgoing edge by angle
        let mut out_angles: Vec<Option<Vec<(f32, usize)>>> = vec![None; self.graph.nodes.len()];
        // For each node, we remember wether they are one the source path or not
        let mut in_actual: Vec<Option<usize>> = vec![None; self.graph.nodes.len()];

        // Create steps
        let mut steps = Vec::new();
        loop {
            for (id, (src, dst, _)) in actual.iter().enumerate() {
                in_actual[*src] = Some(id);
                in_actual[*dst] = Some(id + 1);
            }

            let mut step = Step::new();
            if self.find_step(
                &actual,
                0,
                max_angles[actual[0].0].unwrap(),
                &mut step,
                &in_actual,
                &mut out_angles,
                &max_angles,
            ) {
                for (src, _, _) in &actual {
                    in_actual[*src] = None;
                }
                actual = step
                    .start
                    .iter()
                    .chain(step.middle_right.iter())
                    .chain(step.end.iter())
                    .map(|(src, mph)| (*src, self.graph.edges[*src][*mph].0, *mph))
                    .collect();
                steps.push(step);
            } else {
                break;
            }
        }

        // Realize steps as string
        let steps: String = itertools::Itertools::intersperse(
            steps.into_iter().map(|step| {
                let get_name = |(src, mph): &(usize, usize)| -> &str {
                    self.graph.edges[*src][*mph].1.name.as_str()
                };
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
            }),
            ";".to_string(),
        )
        .collect();

        // Register action
        self.insert_and_run(&format!(
            "decompose {} {}",
            self.graph.faces[face].label.name, steps
        ));
    }

    fn compute_angle(&self, pt: usize, control: usize) -> f32 {
        let pt = self.layout.get_pos(pt);
        let control = self.layout.get_pos(control);
        let dir = control - pt;
        dir.angle()
    }

    fn compute_out_angles(&self, nd: usize) -> Vec<(f32, usize)> {
        let mut r = self.graph.edges[nd]
            .iter()
            .enumerate()
            .map(|(mph, (_, lbl, _, _))| {
                (
                    self.compute_angle(self.graph.nodes[nd].2.pos.unwrap(), lbl.control.unwrap()),
                    mph,
                )
            })
            .collect::<Vec<_>>();
        r.sort_by(|(t1, _), (t2, _)| t1.partial_cmp(t2).unwrap());
        r
    }

    // Starting on node starting in the actual path, we take the rightmost
    // edge until we meet the actual path. We ignore dead-ends and backward
    // loops. We return the parallel path we found and the id in actual we
    // reached.
    fn find_parallel_path(
        &self,
        first: bool,
        starting: usize,
        node: usize,
        start_angle: f32,
        max_angle: f32,
        path: &mut Vec<(usize, usize)>,
        in_actual: &Vec<Option<usize>>,
        out_angles: &mut Vec<Option<Vec<(f32, usize)>>>,
        max_angles: &Vec<Option<f32>>,
    ) -> Option<usize> {
        // TODO fix loop case
        if !first {
            if let Some(id) = in_actual[node] {
                if id <= starting {
                    return None;
                } else {
                    return Some(id);
                }
            }
        }
        if out_angles[node].is_none() {
            out_angles[node] = Some(self.compute_out_angles(node));
        }
        let range = range_for_angles(node, out_angles, start_angle, max_angle);
        for out in range {
            let mph = out_angles[node].as_ref().unwrap()[out].1;
            let dst = self.graph.edges[node][mph].0;
            let in_angle = self.compute_angle(
                self.graph.nodes[dst].2.pos.unwrap(),
                self.graph.edges[node][mph].1.control.unwrap(),
            );
            let max_angle = max_angles[dst].unwrap_or(in_angle);
            path.push((node, mph));
            if let Some(id) = self.find_parallel_path(
                false, starting, dst, in_angle, max_angle, path, in_actual, out_angles, max_angles,
            ) {
                return Some(id);
            }
            path.pop();
        }
        None
    }

    // Starting with a node in the actual path, recursively try to find the last
    // parallel path along it. Operate by side effect on a step, returning true
    // if the step in not trivial.
    fn find_step(
        &self,
        actual: &Vec<(usize, usize, usize)>,
        pos: usize,
        max_angle: f32,
        step: &mut Step,
        in_actual: &Vec<Option<usize>>,
        out_angles: &mut Vec<Option<Vec<(f32, usize)>>>,
        max_angles: &Vec<Option<f32>>,
    ) -> bool {
        if pos >= actual.len() {
            return false;
        }
        let (src, dst, mph) = actual[pos];
        // First we call ourself recursively, in case there is a step later
        {
            let nangle = max_angles[dst].unwrap_or_else(|| {
                self.compute_angle(
                    self.graph.nodes[dst].2.pos.unwrap(),
                    self.graph.edges[src][mph].1.control.unwrap(),
                )
            });
            step.start.push((src, mph));
            if self.find_step(
                actual,
                pos + 1,
                nangle,
                step,
                in_actual,
                out_angles,
                max_angles,
            ) {
                return true;
            }
            step.start.pop();
        }
        // We try to find a parallel path starting from current node
        let min_angle = self.compute_angle(
            self.graph.nodes[src].2.pos.unwrap(),
            self.graph.edges[src][mph].1.control.unwrap(),
        );
        let mut path = Vec::new();
        if let Some(end) = self.find_parallel_path(
            true, pos, src, min_angle, max_angle, &mut path, in_actual, out_angles, max_angles,
        ) {
            step.middle_left = (&actual[pos..end])
                .iter()
                .map(|(src, _, mph)| (*src, *mph))
                .collect();
            step.middle_right = path;
            step.end = (&actual[end..])
                .iter()
                .map(|(src, _, mph)| (*src, *mph))
                .collect();
            return true;
        }
        // Otherwise we go back on the stack
        false
    }
}
