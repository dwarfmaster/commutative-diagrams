use crate::anyterm::IsTerm;
use crate::data::ActualProofObject;
use crate::data::{ActualEquality, Equality, EqualityData};
use crate::data::{ActualMorphism, Morphism};
use crate::graph::{Face, GraphId};
use crate::normalize;
use crate::remote::Remote;
use crate::unification::{unify, UnifOpts};
use crate::vm::graph::LabelSource;
use crate::vm::graph::{FaceLabel, FaceStatus};
use crate::vm::{Interactive, VM};
use std::collections::HashSet;

// TODO Fix when one of the sides is not normalised
// Ie: shrink_prefix and shrink_suffix normalize the sides of the equality when
// constructing. This fails when the realized morphism of the side of the face
// is not normalized (for example because it includes an edge which is a
// composition). In order to fix this, either the normalization should be done
// twice, once with the desired target, or I could finish the refactoring with
// the new equality type.

type Ins = crate::vm::asm::Instruction;

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    // Given an equality between left and right, and a morphism common with the
    // same destination as the source of the equality, we construct an equaltiy
    // between common > left(eq) and common > right(eq) normalised.
    fn shrink_prefix(&mut self, eq: Equality, common: Morphism) -> Equality {
        let neq = self.ctx.mk(ActualEquality::LAp(common, eq));
        let left = neq.left(&self.ctx);
        let (_, eql) = normalize::morphism(&mut self.ctx, left);
        let right = neq.right(&self.ctx);
        let (_, eqr) = normalize::morphism(&mut self.ctx, right);
        self.ctx.mk(ActualEquality::Concat(
            self.ctx.mk(ActualEquality::Inv(eql)),
            self.ctx.mk(ActualEquality::Concat(neq, eqr)),
        ))
    }

    // Same but with common after left/right
    fn shrink_suffix(&mut self, eq: Equality, common: Morphism) -> Equality {
        let neq = self.ctx.mk(ActualEquality::RAp(eq, common));
        let left = neq.left(&self.ctx);
        let (_, eql) = normalize::morphism(&mut self.ctx, left);
        let right = neq.right(&self.ctx);
        let (_, eqr) = normalize::morphism(&mut self.ctx, right);
        self.ctx.mk(ActualEquality::Concat(
            self.ctx.mk(ActualEquality::Inv(eql)),
            self.ctx.mk(ActualEquality::Concat(neq, eqr)),
        ))
    }

    // Find a common part of size size (or as big as possible if size is None)
    // at the start and end of the sides of equality fce. Return the length of
    // the found prefix and suffix.
    fn find_common(
        &mut self,
        fce: usize,
        size_prefix: Option<usize>,
        size_suffix: Option<usize>,
    ) -> (usize, usize) {
        let mut prefix: usize = 0;
        let mut remain_left: Vec<(usize, usize)> = Vec::new();
        let mut remain_right: Vec<(usize, usize)> = Vec::new();

        // First fill prefix and remain
        let keep_looking_left = |id: usize| -> bool {
            if let Some(size) = size_prefix {
                id < size
            } else {
                true
            }
        };

        let mut src = self.graph.faces[fce].start;
        for mph_id in 0..self.graph.faces[fce]
            .left
            .len()
            .min(self.graph.faces[fce].right.len())
        {
            let nxt_left = self.graph.faces[fce].left[mph_id];
            let nxt_right = self.graph.faces[fce].right[mph_id];
            if keep_looking_left(mph_id) && nxt_left == nxt_right {
                prefix += 1;
                src = self.graph.edges[src][nxt_left].0;
                continue;
            }
            remain_left = self.graph.faces[fce].left[mph_id..]
                .iter()
                .scan(
                    src,
                    |src: &mut usize, mph: &usize| -> Option<(usize, usize)> {
                        let prev_src = *src;
                        *src = self.graph.edges[*src][*mph].0;
                        Some((prev_src, *mph))
                    },
                )
                .collect();
            remain_right = self.graph.faces[fce].right[mph_id..]
                .iter()
                .scan(
                    src,
                    |src: &mut usize, mph: &usize| -> Option<(usize, usize)> {
                        let prev_src = *src;
                        *src = self.graph.edges[*src][*mph].0;
                        Some((prev_src, *mph))
                    },
                )
                .collect();
            break;
        }

        // Now find suffix
        let keep_looking_right = |id: usize| -> bool {
            if let Some(size) = size_suffix {
                id < size
            } else {
                true
            }
        };

        let mut suffix: usize = 0;
        for mph_id in (0..remain_left.len().min(remain_right.len())).rev() {
            let (src_left, mph_left) = remain_left.last().unwrap();
            let (src_right, mph_right) = remain_right.last().unwrap();
            if keep_looking_right(mph_id) && src_left == src_right && mph_left == mph_right {
                remain_left.pop();
                remain_right.pop();
                suffix += 1;
            } else {
                break;
            }
        }

        (prefix, suffix)
    }

    fn realize_morphism(&self, src: usize, mphs: &[usize]) -> Morphism {
        mphs.iter()
            .scan(src, |src: &mut usize, mph: &usize| -> Option<Morphism> {
                let ret = self.graph.edges[*src][*mph].2.clone();
                *src = self.graph.edges[*src][*mph].0;
                Some(ret)
            })
            .reduce(|mph1, mph2| -> Morphism { self.ctx.mk(ActualMorphism::Comp(mph1, mph2)) })
            .unwrap_or(
                self.ctx
                    .mk(ActualMorphism::Identity(self.graph.nodes[src].0.clone())),
            )
    }

    // May fail if fce is not an existential
    pub fn shrink(
        &mut self,
        fce: usize,
        prefix_size: Option<usize>,
        suffix_size: Option<usize>,
    ) -> bool {
        // If left and right are the same, we conclude by reflexivity
        let total_size = prefix_size
            .unwrap_or(std::usize::MAX)
            .saturating_add(suffix_size.unwrap_or(std::usize::MAX));
        if self.graph.faces[fce].eq.left(&self.ctx) == self.graph.faces[fce].eq.right(&self.ctx)
            && total_size >= self.graph.faces[fce].left.len()
        {
            let new_eq = self.ctx.mk(ActualEquality::Refl(
                self.graph.faces[fce].eq.left(&self.ctx),
            ));
            let sigma = unify(
                &self.ctx,
                self.graph.faces[fce].eq.clone().term(),
                new_eq.term(),
                Default::default(),
            );
            if let Some(sigma) = sigma {
                self.refine(sigma);
                return true;
            } else {
                return false;
            }
        }

        let (prefix_len, suffix_len) = self.find_common(fce, prefix_size, suffix_size);
        if prefix_size.map(|l| l != prefix_len).unwrap_or(false)
            || suffix_size.map(|l| l != suffix_len).unwrap_or(false)
        {
            return false;
        }

        // Nothing to do
        if prefix_len == 0 && suffix_len == 0 {
            return true;
        }

        // Find src and dst of the common part
        let mut src_id = self.graph.faces[fce].start;
        for i in 0..prefix_len {
            let mph = self.graph.faces[fce].left[i];
            src_id = self.graph.edges[src_id][mph].0;
        }
        let mut dst_id = src_id;
        for i in prefix_len..(self.graph.faces[fce].left.len() - suffix_len) {
            let mph = self.graph.faces[fce].left[i];
            dst_id = self.graph.edges[dst_id][mph].0;
        }

        // Left and right morphisms
        let left_range = prefix_len..(self.graph.faces[fce].left.len() - suffix_len);
        let left_slice = &self.graph.faces[fce].left[left_range.clone()];
        let right_range = prefix_len..(self.graph.faces[fce].right.len() - suffix_len);
        let right_slice = &self.graph.faces[fce].right[right_range.clone()];
        let left = self.realize_morphism(src_id, left_slice);
        let right = self.realize_morphism(src_id, right_slice);

        // We create the new equality
        let ex = self.ctx.new_existential();
        let new_eq = self.ctx.mk(ActualEquality::Atomic(EqualityData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
            category: self.graph.faces[fce].eq.cat(&self.ctx),
            src: self.graph.nodes[src_id].0.clone(),
            dst: self.graph.nodes[dst_id].0.clone(),
            left,
            right,
        }));
        let mut prev_eq = new_eq.clone();

        // Shrinking the suffix
        if suffix_len != 0 {
            let suffix = self.realize_morphism(
                dst_id,
                &self.graph.faces[fce].left[(self.graph.faces[fce].left.len() - suffix_len)..],
            );
            prev_eq = self.shrink_suffix(prev_eq, suffix);
        }

        // Shrinking the prefix
        if prefix_len != 0 {
            let prefix = self.realize_morphism(
                self.graph.faces[fce].start,
                &self.graph.faces[fce].left[0..prefix_len],
            );
            prev_eq = self.shrink_prefix(prev_eq, prefix);
        }

        // Unify with previous face
        assert!(new_eq.check(&self.ctx));
        let sigma = unify(
            &self.ctx,
            self.graph.faces[fce].eq.clone().term(),
            prev_eq.term(),
            UnifOpts {
                ground: HashSet::from([ex]),
                ..Default::default()
            },
        );
        if let Some(sigma) = sigma {
            self.refine(sigma);
        } else {
            return false;
        }

        // Create face
        let left_slice = &self.graph.faces[fce].left[left_range];
        let right_slice = &self.graph.faces[fce].right[right_range];
        let new_face = Face {
            start: src_id,
            end: dst_id,
            left: left_slice.to_vec(),
            right: right_slice.to_vec(),
            eq: new_eq,
            label: FaceLabel {
                label: "".to_string(),
                label_source: LabelSource::None,
                name: "".to_string(),
                hidden: false,
                parent: Some(fce),
                children: Vec::new(),
                status: FaceStatus::Goal,
                folded: self.graph.faces[fce].label.folded,
            },
        };
        self.register_instruction(Ins::InsertFace(new_face));
        self.hide(GraphId::Face(fce));
        self.relabel();
        true
    }
}
