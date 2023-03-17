use crate::anyterm::IsTerm;
use crate::data::ActualProofObject;
use crate::data::{ActualEquality, Equality, EqualityData};
use crate::graph::Face;
use crate::unification::unify;
use crate::vm::{GraphId, VM};

type Ins = crate::vm::asm::Instruction;

impl VM {
    // Create a new face of sides left_path and right_path, with equality a new
    // hole, marking the face fce as hidden and refining its equality with
    // left_eq <> ex <> right_eq^-1
    pub fn conjugate_face(
        &mut self,
        fce: usize,
        left_eq: Equality,
        right_eq: Equality,
        left_path: Vec<usize>,
        right_path: Vec<usize>,
    ) -> Option<usize> {
        use ActualEquality::*;
        assert_eq!(
            left_eq.left(&self.ctx),
            self.graph.faces[fce].eq.left(&self.ctx)
        );
        assert_eq!(
            right_eq.left(&self.ctx),
            self.graph.faces[fce].eq.right(&self.ctx)
        );

        // Construct hole
        let left = left_eq.right(&self.ctx);
        let right = right_eq.right(&self.ctx);
        let ex = self.ctx.new_existential();
        let hole = self.ctx.mk(Atomic(EqualityData {
            category: left.cat(&self.ctx),
            src: left.src(&self.ctx),
            dst: left.dst(&self.ctx),
            left,
            right,
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
        }));

        // Unify hole with term
        let new_eq = self
            .ctx
            .mk(Concat(
                left_eq,
                self.ctx
                    .mk(Concat(hole.clone(), self.ctx.mk(Inv(right_eq)))),
            ))
            .simpl(&self.ctx);
        assert!(new_eq.check(&self.ctx));
        let sigma = unify(
            &self.ctx,
            self.graph.faces[fce].eq.clone().term(),
            new_eq.term(),
        )?;
        self.register_instruction(Ins::ExtendRefinements(sigma));

        // Add new face
        let face = Face {
            start: self.graph.faces[fce].start,
            end: self.graph.faces[fce].end,
            left: left_path,
            right: right_path,
            eq: hole,
            label: self.graph.faces[fce].label.clone(),
        };
        assert!(face.check(&self.ctx, &self.graph));
        let face_id = self.graph.faces.len();
        self.register_instruction(Ins::InsertFace(face, Some(fce)));

        // Hide previous face
        self.hide(GraphId::Face(fce));

        Some(face_id)
    }
}
