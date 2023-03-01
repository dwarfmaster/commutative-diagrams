use crate::data::{Equality, Morphism, Object};
use crate::graph::Face;
use crate::substitution::Substitution;
use crate::vm::{EdgeLabel, FaceLabel, NodeLabel};
use lens_rs::LensMut;
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

    pub fn from_lens<L, O>(val: &T, lens: L, new: O) -> Self
    where
        T: LensMut<L, O>,
        O: Clone + Send + Sync + 'static,
        L: Copy + Send + Sync + 'static,
    {
        let old = val.view_ref(lens).clone();
        let direct = move |v: &mut T| {
            *v.view_mut(lens) = new.clone();
        };
        let reverse = move |v: &mut T| {
            *v.view_mut(lens) = old.clone();
        };
        Self {
            direct: Box::new(direct),
            reverse: Box::new(reverse),
        }
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
    InsertMorphism(usize, usize, Morphism),
    UpdateMorphism(usize, usize, Morphism, Morphism),
    RelocateMorphismSrc(usize, usize, usize),
    RelocateMorphismDst(usize, usize, usize, usize),
    UpdateMorphismLabel(usize, usize, Updater<EdgeLabel>),
    InsertFace(Face<FaceLabel>),
    UpdateFace(usize, Equality, Equality),
    UpdateFaceLabel(usize, Updater<FaceLabel>),
    ExtendRefinements(Substitution),
}
