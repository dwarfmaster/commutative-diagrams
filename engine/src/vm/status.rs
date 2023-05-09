use crate::anyterm::IsTerm;
use crate::data::{ActualEquality, ActualProofObject};
use crate::vm::graph::FaceStatus;
use crate::vm::{Interactive, VM};
use std::ops::Deref;

impl<I: Interactive + Sync + Send> VM<I> {
    /// Auto compute and set a face status depending on the existentials in its
    /// term.
    pub fn set_face_status(&mut self, fce: usize) {
        if let ActualEquality::Atomic(data) = self.graph.faces[fce].eq.deref() {
            if let ActualProofObject::Existential(_) = data.pobj.deref() {
                self.graph.faces[fce].label.status = FaceStatus::Goal;
                return;
            }
        }

        self.graph.faces[fce].label.status = if self.graph.faces[fce]
            .eq
            .clone()
            .term()
            .existentials(&self.ctx)
            .is_empty()
        {
            FaceStatus::Hypothesis
        } else {
            FaceStatus::Refined
        };
    }

    // Recompute face status of all faces
    pub fn recompute_face_statuses(&mut self) {
        for fce in 0..self.graph.faces.len() {
            self.set_face_status(fce)
        }
    }
}
