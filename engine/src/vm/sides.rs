use crate::data::Morphism;
use crate::vm;
use crate::vm::graph::EdgeLabel;
use crate::vm::VM;

fn on_path<F>(
    edges: &mut Vec<Vec<(usize, EdgeLabel, Morphism)>>,
    mut node: usize,
    nexts: &[usize],
    f: F,
) where
    F: Fn(&mut EdgeLabel),
{
    for nid in 0..nexts.len() {
        let (dst, lbl, _) = &mut edges[node][nexts[nid]];
        f(lbl);
        node = *dst;
    }
}

impl VM {
    pub fn show_face(&mut self, fce: usize) {
        on_path(
            &mut self.graph.edges,
            self.graph.faces[fce].start,
            vm::get_left_side(&self.graph.faces, fce),
            |lbl| {
                lbl.style.left = true;
            },
        );
        on_path(
            &mut self.graph.edges,
            self.graph.faces[fce].start,
            vm::get_right_side(&self.graph.faces, fce),
            |lbl| {
                lbl.style.right = true;
            },
        );
    }

    pub fn unshow_face(&mut self, fce: usize) {
        on_path(
            &mut self.graph.edges,
            self.graph.faces[fce].start,
            vm::get_left_side(&self.graph.faces, fce),
            |lbl| {
                lbl.style.left = false;
            },
        );
        on_path(
            &mut self.graph.edges,
            self.graph.faces[fce].start,
            vm::get_right_side(&self.graph.faces, fce),
            |lbl| {
                lbl.style.right = false;
            },
        );
    }
}
