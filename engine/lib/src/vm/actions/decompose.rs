use crate::data::Feature;
use crate::graph::eq::{Eq, Morphism};
use crate::graph::Face;
use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::{FaceLabel, FaceStatus, Interactive, VM};

pub struct Step {
    pub start: Vec<(usize, usize)>,
    pub middle_left: Vec<(usize, usize)>,
    pub middle_right: Vec<(usize, usize)>,
    pub end: Vec<(usize, usize)>,
}

fn real_mph<Rm: Remote>(rm: &mut Rm, cat: u64, mph: &Morphism) -> u64 {
    let r = mph
        .comps
        .iter()
        .copied()
        .reduce(|(src, mid, m1), (_, dst, m2)| -> (u64, u64, u64) {
            let mph = rm
                .build(Feature::ComposeMph {
                    cat,
                    src,
                    mid,
                    dst,
                    m1,
                    m2,
                })
                .unwrap();
            (src, dst, mph)
        })
        .unwrap_or_else(|| {
            let obj = mph.src;
            let id = rm.build(Feature::Identity { cat, obj }).unwrap();
            (obj, obj, id)
        });
    r.2
}

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    // Look all visible faces for one with the right sides. If found, returns its equality
    // (potentially inverted).
    fn decompose_lookup(&self, left: &Morphism, right: &Morphism) -> Option<Eq> {
        for face in &self.graph.faces {
            if face.label.hidden {
                continue;
            }
            if face.eq.inp == *left && face.eq.outp == *right {
                return Some(face.eq.clone());
            } else if face.eq.outp == *left && face.eq.inp == *right {
                let mut eq = face.eq.clone();
                eq.inv();
                return Some(eq);
            }
        }
        None
    }

    // Convert a step into a equality, and returns it. Also return the equality
    // consisting only of the existential.
    fn decompose_step_to_eq(&mut self, parent: usize, step: Step) -> (Eq, Option<Face<FaceLabel>>) {
        assert!(step.middle_left.len() > 0 || step.middle_right.len() > 0);

        // Information about the existential part
        let (cat, ex_src, ex_dst) = {
            let (fmph, lmph) = if step.middle_left.is_empty() {
                (step.middle_right[0], step.middle_right.last().unwrap())
            } else {
                (step.middle_left[0], step.middle_left.last().unwrap())
            };
            (
                self.graph.nodes[fmph.0].1,
                fmph.0,
                self.graph.edges[lmph.0][lmph.1].0,
            )
        };

        // Basic information about the equality
        let (src_node, dst_node) = {
            let first_mph = if step.start.is_empty() {
                if step.middle_left.is_empty() {
                    step.middle_right[0]
                } else {
                    step.middle_left[0]
                }
            } else {
                step.start[0]
            };
            let last_mph = if let Some(last) = step.end.last() {
                last
            } else {
                if let Some(last) = step.middle_left.last() {
                    last
                } else {
                    step.middle_right.last().unwrap()
                }
            };
            (first_mph.0, self.graph.edges[last_mph.0][last_mph.1].0)
        };

        // Building the reflexivity
        let mk_comp = |(src, mph): (usize, usize)| {
            let dst = self.graph.edges[src][mph].0;
            (
                self.graph.nodes[src].0,
                self.graph.nodes[dst].0,
                self.graph.edges[src][mph].2,
            )
        };
        let mph = Morphism {
            src: self.graph.nodes[src_node].0,
            dst: self.graph.nodes[dst_node].0,
            comps: step
                .start
                .iter()
                .chain(step.middle_left.iter())
                .chain(step.end.iter())
                .copied()
                .map(mk_comp)
                .collect(),
        };
        let mut eq = Eq::refl(cat, mph);

        // Building the existential
        let in_mph = Morphism {
            src: self.graph.nodes[ex_src].0,
            dst: self.graph.nodes[ex_dst].0,
            comps: step.middle_left.iter().copied().map(mk_comp).collect(),
        };
        let in_mph_val = real_mph(&mut self.ctx.remote, cat, &in_mph);
        let out_mph = Morphism {
            src: self.graph.nodes[ex_src].0,
            dst: self.graph.nodes[ex_dst].0,
            comps: step.middle_right.iter().copied().map(mk_comp).collect(),
        };
        let out_mph_val = real_mph(&mut self.ctx.remote, cat, &out_mph);

        // Reuse existing face if found
        if let Some(face_eq) = self.decompose_lookup(&in_mph, &out_mph) {
            eq.append_at(step.start.len(), face_eq);
            return (eq, None);
        }
        let ex = self
            .ctx
            .remote
            .build(Feature::Equality {
                cat,
                src: in_mph.src,
                dst: in_mph.dst,
                left: in_mph_val,
                right: out_mph_val,
            })
            .unwrap();

        // Create the equality block
        let beq = Eq::atomic(cat, in_mph, out_mph, ex);
        eq.append_at(step.start.len(), beq.clone());
        let face = Face {
            start: ex_src,
            end: ex_dst,
            left: step.middle_left.iter().map(|(_, mph)| *mph).collect(),
            right: step.middle_right.iter().map(|(_, mph)| *mph).collect(),
            eq: beq,
            label: FaceLabel {
                label: "".to_string(),
                name: "".to_string(),
                hidden: false,
                parent: Some(parent),
                children: Vec::new(),
                status: FaceStatus::Goal,
                folded: false,
            },
        };

        // Finalizing the equality
        (eq, Some(face))
    }

    pub fn decompose_face(&mut self, fce: usize, steps: Vec<Step>) -> bool {
        assert!(steps.len() > 0);
        let cat = self.graph.nodes[self.graph.faces[fce].start].1;

        // Realize all the steps
        let (steps, exs): (Vec<Eq>, Vec<Option<Face<FaceLabel>>>) = steps
            .into_iter()
            .map(|step| self.decompose_step_to_eq(fce, step))
            .unzip();
        let to_unify = steps
            .into_iter()
            .reduce(|mut eq1, eq2| {
                eq1.append(eq2);
                eq1
            })
            .unwrap();
        if !self.unify_eq(cat, &self.graph.faces[fce].eq.clone(), &to_unify) {
            return false;
        }

        // Create new faces for all new existentials
        for ex in exs {
            if let Some(ex) = ex {
                self.register_instruction(asm::Instruction::InsertFace(ex));
            }
        }
        true
    }
}
