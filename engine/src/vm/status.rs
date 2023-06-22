use crate::data::EvarStatus;
use crate::graph::eq::{BlockData, Eq};
use crate::remote::Remote;
use crate::vm::graph::FaceStatus;
use crate::vm::store::Context;
use crate::vm::{Interactive, VM};

fn from_estatus(status: EvarStatus) -> FaceStatus {
    match status {
        EvarStatus::Evar => FaceStatus::Goal,
        EvarStatus::Partial => FaceStatus::Refined,
        EvarStatus::Grounded => FaceStatus::Hypothesis,
    }
}

fn merge_statuses(s1: EvarStatus, s2: EvarStatus) -> EvarStatus {
    use EvarStatus::*;
    match (s1, s2) {
        (Grounded, Grounded) => Grounded,
        _ => Partial,
    }
}

impl<Rm: Remote> Context<Rm> {
    pub fn compute_eq_status(&mut self, eq: &Eq) -> EvarStatus {
        if eq.slices.len() == 1
            && eq.inp.comps.len() == eq.slices[0].inp.comps.len()
            && eq.outp.comps.len() == eq.slices[0].outp.comps.len()
            && eq.slices[0].blocks.len() == 1
            && eq.slices[0].blocks[0].0 == 0
            && eq.slices[0].blocks[0].1 == 0
            && eq.inp.comps.len() == eq.slices[0].blocks[0].2.inp.comps.len()
            && eq.outp.comps.len() == eq.slices[0].blocks[0].2.outp.comps.len()
        {
            if let BlockData::Direct(neq) = eq.slices[0].blocks[0].2.data {
                return self.get_stored_status(neq);
            } else if let BlockData::Inv(neq) = eq.slices[0].blocks[0].2.data {
                return self.get_stored_status(neq);
            }
        }

        let mut status = EvarStatus::Grounded;
        for slice in &eq.slices {
            for (_, _, blk) in &slice.blocks {
                use BlockData::*;
                status = match &blk.data {
                    Direct(eq) => merge_statuses(status, self.get_stored_status(*eq)),
                    Inv(eq) => merge_statuses(status, self.get_stored_status(*eq)),
                    Funct(f, eq) => merge_statuses(
                        status,
                        merge_statuses(self.get_stored_status(*f), self.compute_eq_status(eq)),
                    ),
                    Split => status,
                }
            }
        }
        status
    }
}

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    pub fn set_face_status(&mut self, fce: usize) {
        self.graph.faces[fce].label.status =
            from_estatus(self.ctx.compute_eq_status(&self.graph.faces[fce].eq));
    }

    // Recompute face status of all faces
    pub fn recompute_face_statuses(&mut self) {
        for fce in 0..self.graph.faces.len() {
            self.set_face_status(fce)
        }
    }
}
