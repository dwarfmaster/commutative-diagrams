use crate::data::Morphism;
use crate::graph::Face;
use crate::vm::graph::{EdgeLabel, FaceLabel};
use crate::vm::VM;

type Edges = Vec<Vec<(usize, EdgeLabel, Morphism)>>;
type Faces = Vec<Face<FaceLabel>>;

fn alias(edges: &mut Edges, left: bool, src: usize, prev: &Vec<usize>, new: &Vec<usize>) {
    let mut node = src;
    for nxt in prev {
        let (dst, label, _) = &mut edges[node][*nxt];
        if left {
            label.style.left = false;
        } else {
            label.style.right = false;
        }
        node = *dst;
    }
    let mut node = src;
    for nxt in new {
        let (dst, label, _) = &mut edges[node][*nxt];
        if left {
            label.style.left = true;
        } else {
            label.style.right = true;
        }
        node = *dst;
    }
}

pub fn get_left_side<'a>(faces: &'a Faces, fce: usize) -> &'a Vec<usize> {
    if let Some(left) = &faces[fce].label.left {
        left
    } else {
        &faces[fce].left
    }
}

pub fn get_right_side<'a>(faces: &'a Faces, fce: usize) -> &'a Vec<usize> {
    if let Some(right) = &faces[fce].label.right {
        right
    } else {
        &faces[fce].right
    }
}

impl VM {
    pub fn alias_left(&mut self, fce: usize, path: Vec<usize>) {
        let side = get_left_side(&self.graph.faces, fce);
        alias(
            &mut self.graph.edges,
            true,
            self.graph.faces[fce].start,
            side,
            &path,
        );
    }

    pub fn alias_right(&mut self, fce: usize, path: Vec<usize>) {
        let side = get_right_side(&self.graph.faces, fce);
        alias(
            &mut self.graph.edges,
            false,
            self.graph.faces[fce].start,
            side,
            &path,
        );
    }

    pub fn get_left_side<'a>(&'a self, fce: usize) -> &'a Vec<usize> {
        get_left_side(&self.graph.faces, fce)
    }

    pub fn get_right_side<'a>(&'a self, fce: usize) -> &'a Vec<usize> {
        get_right_side(&self.graph.faces, fce)
    }
}
