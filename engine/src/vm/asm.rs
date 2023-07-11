use crate::graph::eq::{Eq, Morphism};
use crate::graph::Face;
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
    InsertNode(/*object*/ u64, /*cat*/ u64),
    UpdateNode(usize, u64, u64),
    UpdateNodeLabel(usize, Updater<NodeLabel>),
    RenameNode(usize, String, String),
    InsertMorphism(usize, usize, u64, Morphism),
    UpdateMorphism(usize, usize, u64, u64),
    RelocateMorphismSrc(usize, usize, usize),
    RelocateMorphismDst(usize, usize, usize, usize),
    UpdateMorphismLabel(usize, usize, Updater<EdgeLabel>),
    RenameMorphism(usize, usize, String, String),
    InsertFace(Face<FaceLabel>), // Add a face
    UpdateFace(usize, Eq, Eq),
    RelocateFaceSrc(usize, usize, usize),
    RelocateFaceDst(usize, usize, usize),
    RelocateFaceLeft(usize, Vec<usize>, Vec<usize>),
    RelocateFaceRight(usize, Vec<usize>, Vec<usize>),
    UpdateFaceLabel(usize, Updater<FaceLabel>),
    RenameFace(usize, String, String),
}
