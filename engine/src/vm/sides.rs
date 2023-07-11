use crate::graph::eq::Morphism;
use crate::remote::Remote;
use crate::vm::graph::EdgeLabel;
use crate::vm::{Graph, Interactive, VM};

fn on_path<F>(
    edges: &mut Vec<Vec<(usize, EdgeLabel, u64, Morphism)>>,
    mut node: usize,
    nexts: &[usize],
    f: F,
) where
    F: Fn(&mut EdgeLabel),
{
    for nid in 0..nexts.len() {
        let (dst, lbl, _, _) = &mut edges[node][nexts[nid]];
        f(lbl);
        node = *dst;
    }
}

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    pub fn show_face_impl(graph: &mut Graph, fce: usize) {
        on_path(
            &mut graph.edges,
            graph.faces[fce].start,
            &graph.faces[fce].left,
            |lbl| {
                lbl.style.left = true;
            },
        );
        on_path(
            &mut graph.edges,
            graph.faces[fce].start,
            &graph.faces[fce].right,
            |lbl| {
                lbl.style.right = true;
            },
        );
    }

    pub fn show_face(&mut self, fce: usize) {
        Self::show_face_impl(&mut self.graph, fce);
    }

    pub fn unshow_face_impl(graph: &mut Graph, fce: usize) {
        on_path(
            &mut graph.edges,
            graph.faces[fce].start,
            &graph.faces[fce].left,
            |lbl| {
                lbl.style.left = false;
            },
        );
        on_path(
            &mut graph.edges,
            graph.faces[fce].start,
            &graph.faces[fce].right,
            |lbl| {
                lbl.style.right = false;
            },
        );
    }

    pub fn unshow_face(&mut self, fce: usize) {
        Self::unshow_face_impl(&mut self.graph, fce);
    }

    pub fn deselect_face(&mut self) {
        if let Some(fce) = self.selected_face.take() {
            self.unshow_face(fce);
        }
    }

    pub fn select_face(&mut self, fce: usize) {
        if let Some(prev) = self.selected_face {
            if prev == fce {
                return;
            }
            self.unshow_face(prev);
        }
        self.show_face(fce);
        self.selected_face = Some(fce);
    }
}
