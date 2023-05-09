use crate::anyterm::IsTerm;
use crate::data::ActualProofObject;
use crate::data::{ActualEquality, Equality, EqualityData};
use crate::graph::{Face, GraphId};
use crate::substitution::{Substitutable, Substitution};
use crate::unification::{unify, UnifOpts};
use crate::vm::graph::{FaceLabel, FaceStatus};
use crate::vm::{Interactive, VM};
use std::collections::HashSet;

type Ins = crate::vm::asm::Instruction;

impl<I: Interactive + Sync + Send> VM<I> {
    // Substitute sigma in the graph, and add it to the refinements
    pub fn refine(&mut self, sigma: Substitution) {
        // Substitute nodes
        for id in 0..self.graph.nodes.len() {
            let nd = self.graph.nodes[id].0.clone();
            let nsubst = nd.clone().subst(&self.ctx, &sigma);
            if nsubst != nd {
                self.register_instruction(Ins::UpdateNode(id, nd, nsubst));
            }
        }

        // Substitute morphisms
        for src in 0..self.graph.edges.len() {
            for edge in 0..self.graph.edges[src].len() {
                let mph = self.graph.edges[src][edge].2.clone();
                let msubst = mph.clone().subst(&self.ctx, &sigma);
                if msubst != mph {
                    self.register_instruction(Ins::UpdateMorphism(src, edge, mph, msubst));
                }
            }
        }

        // Substitute faces
        for id in 0..self.graph.faces.len() {
            let fce = self.graph.faces[id].eq.clone();
            let fsubst = fce.clone().subst(&self.ctx, &sigma).simpl(&self.ctx);
            if fsubst != fce {
                self.register_instruction(Ins::UpdateFace(id, fce, fsubst));
            }
        }

        self.register_instruction(Ins::ExtendRefinements(sigma));
        self.relabel();
    }

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
            UnifOpts {
                ground: HashSet::from([ex]),
                ..Default::default()
            },
        )?;
        self.refine(sigma);

        // Add new face
        let face = Face {
            start: self.graph.faces[fce].start,
            end: self.graph.faces[fce].end,
            left: left_path,
            right: right_path,
            eq: hole,
            label: FaceLabel {
                name: "".to_string(),
                parent: Some(fce),
                children: Vec::new(),
                label: self.graph.faces[fce].label.label.clone(),
                label_source: self.graph.faces[fce].label.label_source,
                hidden: false,
                status: FaceStatus::Goal,
                folded: self.graph.faces[fce].label.folded,
            },
        };
        assert!(face.check(&self.ctx, &self.graph));
        let face_id = self.graph.faces.len();
        self.register_instruction(Ins::InsertFace(face));

        // Hide previous face
        self.hide(GraphId::Face(fce));

        Some(face_id)
    }
}
