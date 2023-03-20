use crate::data::{Equality, Morphism, Object};
use crate::graph::Face;
use crate::substitution::Substitution;
use crate::vm::{EdgeLabel, FaceLabel, NodeLabel};
use std::fmt::{Debug, Error, Formatter};

pub struct Updater<T> {
    pub direct: Box<dyn Fn(&mut T) + Send + Sync>,
    pub reverse: Box<dyn Fn(&mut T) + Send + Sync>,
}
impl<T> Debug for Updater<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.debug_struct("Updater<T>").finish()
    }
}

impl<T> Updater<T> {
    pub fn apply(&self, x: &mut T) {
        (self.direct)(x)
    }

    pub fn undo(&self, y: &mut T) {
        (self.reverse)(y)
    }

    pub fn inverse(&mut self) {
        std::mem::swap(&mut self.direct, &mut self.reverse);
    }
}

// Represent a revertible atomic operation on the VM. All actions should be
// compiled to a sequence of instructions to support rollback. Re-layouting is
// not considered an action, but something to be done after executing an
// instruction that may invalidate the layout. This allows making sure only one
// layout is done per sequence of instructions executed, and since it is
// deterministic it avoids storing the changes in locations as instructions.
#[derive(Debug)]
pub enum Instruction {
    InsertNode(Object),
    UpdateNode(usize, Object, Object),
    UpdateNodeLabel(usize, Updater<NodeLabel>),
    RenameNode(usize, String, String),
    InsertMorphism(usize, usize, Morphism),
    UpdateMorphism(usize, usize, Morphism, Morphism),
    RelocateMorphismSrc(usize, usize, usize),
    RelocateMorphismDst(usize, usize, usize, usize),
    UpdateMorphismLabel(usize, usize, Updater<EdgeLabel>),
    RenameMorphism(usize, usize, String, String),
    InsertFace(Face<FaceLabel>, Option<usize>), // Add a face, optionally indicating a parent
    UpdateFace(usize, Equality, Equality),
    UpdateFaceLabel(usize, Updater<FaceLabel>),
    RenameFace(usize, String, String),
    ExtendRefinements(Substitution),
}
