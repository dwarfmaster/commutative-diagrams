use crate::graph::Face;
use crate::vm::graph::FaceLabel;
use crate::vm::VM;

type Faces = Vec<Face<FaceLabel>>;

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
    pub fn get_left_side<'a>(&'a self, fce: usize) -> &'a Vec<usize> {
        get_left_side(&self.graph.faces, fce)
    }

    pub fn get_right_side<'a>(&'a self, fce: usize) -> &'a Vec<usize> {
        get_right_side(&self.graph.faces, fce)
    }
}
