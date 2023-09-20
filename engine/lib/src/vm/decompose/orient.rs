use crate::remote::Remote;
use crate::vm::{Interactive, VM};
use egui::Pos2;

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    // Returns true if two parallel paths are oriented correctly for decomposition
    pub fn decompose_orient_sides<ItLeft, ItRight>(&self, left: ItLeft, right: ItRight) -> bool
    where
        ItLeft: Iterator<Item = (usize, usize)>,
        ItRight: Iterator<Item = (usize, usize)> + DoubleEndedIterator,
    {
        // Approximate the face by the polygon of nodes and control points, and
        // then use the method described here:
        // https://en.wikipedia.org/wiki/Curve_orientation
        let points: Vec<Pos2> = left
            .map(|(src, mph)| {
                [
                    self.layout.get_pos(self.graph.nodes[src].2.pos.unwrap()),
                    self.layout
                        .get_pos(self.graph.edges[src][mph].1.control.unwrap()),
                ]
            })
            .chain(right.rev().map(|(src, mph)| {
                let dst = self.graph.edges[src][mph].0;
                [
                    self.layout.get_pos(self.graph.nodes[dst].2.pos.unwrap()),
                    self.layout
                        .get_pos(self.graph.edges[src][mph].1.control.unwrap()),
                ]
            }))
            .flatten()
            .collect();

        if points.len() <= 2 {
            // This means there was at most one node, so all edge are loops that
            // have already been simplified away
            return true;
        }

        let mut on_hull = 0;
        for i in 1..points.len() {
            if points[i].x < points[on_hull].x
                || ((points[i].x - points[on_hull].x).abs() < 1e-9f32
                    && points[i].y < points[on_hull].y)
            {
                on_hull = i;
            }
        }
        let prev = if on_hull == 0 {
            points.len() - 1
        } else {
            on_hull - 1
        };
        let next = (on_hull + 1) % points.len();

        let det = ((points[on_hull].x - points[prev].x) * (points[next].y - points[prev].y))
            - ((points[next].x - points[prev].x) * (points[on_hull].y - points[prev].y));
        det > 0f32
    }
}
