use crate::data::Feature;
use crate::graph::eq::{Eq, Morphism};
use crate::graph::{Face, GraphId};
use crate::remote::{Remote, TermEngine};
use crate::vm::graph::{FaceLabel, FaceStatus, Graph};
use crate::vm::{Interactive, VM};

type Ins = crate::vm::asm::Instruction;

fn realize_morphism<Rm: TermEngine>(
    rm: &mut Rm,
    graph: &Graph,
    src: usize,
    mphs: &[usize],
) -> (u64, Morphism) {
    let cat = graph.nodes[src].1;
    let comps = mphs
        .iter()
        .scan(
            src,
            |src: &mut usize, mph: &usize| -> Option<(u64, u64, u64)> {
                let ret_src = graph.nodes[*src].0;
                let ret_dst = graph.nodes[graph.edges[*src][*mph].0].0;
                let ret = graph.edges[*src][*mph].2;
                *src = graph.edges[*src][*mph].0;
                Some((ret_src, ret_dst, ret))
            },
        )
        .collect::<Vec<_>>();
    let mph_term = comps
        .iter()
        .copied()
        .reduce(|(src, mid, m1), (_, dst, m2)| -> (u64, u64, u64) {
            let mph = rm
                .remote()
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
            let obj = graph.nodes[src].0;
            let id = rm.remote().build(Feature::Identity { cat, obj }).unwrap();
            (obj, obj, id)
        })
        .2;
    let src = graph.nodes[src].0;
    let dst = comps.last().map(|(_, d, _)| *d).unwrap_or(src);
    let mph = Morphism { src, dst, comps };
    (mph_term, mph)
}

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
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

    // May fail if fce is not an existential
    pub fn shrink(
        &mut self,
        fce: usize,
        prefix_size: Option<usize>,
        suffix_size: Option<usize>,
    ) -> bool {
        let cat = self.graph.nodes[self.graph.faces[fce].start].1;

        // If left and right are the same, we conclude by reflexivity
        let total_size = prefix_size
            .unwrap_or(std::usize::MAX)
            .saturating_add(suffix_size.unwrap_or(std::usize::MAX));
        if self.graph.faces[fce].eq.inp == self.graph.faces[fce].eq.outp
            && total_size >= self.graph.faces[fce].left.len()
        {
            let new_eq = Eq::refl(cat, self.graph.faces[fce].eq.inp.clone());
            let eq = self.graph.faces[fce].eq.clone();
            return self.unify_eq(cat, &new_eq, &eq);
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
        let (left, left_mph) = realize_morphism(&mut self.ctx, &self.graph, src_id, left_slice);
        let (right, right_mph) = realize_morphism(&mut self.ctx, &self.graph, src_id, right_slice);

        // We create the new equality
        log::trace!("Creating existential");
        let ex = self
            .ctx
            .remote
            .build(Feature::Equality {
                cat,
                src: self.graph.nodes[src_id].0,
                dst: self.graph.nodes[dst_id].0,
                left,
                right,
            })
            .unwrap();
        let new_eq = Eq::atomic(cat, left_mph, right_mph, ex);
        let mut prev_eq = new_eq.clone();

        // Shrinking the suffix
        if suffix_len != 0 {
            let (_, suffix) = realize_morphism(
                &mut self.ctx,
                &self.graph,
                dst_id,
                &self.graph.faces[fce].left[(self.graph.faces[fce].left.len() - suffix_len)..],
            );
            prev_eq.rap(&suffix);
        }

        // Shrinking the prefix
        if prefix_len != 0 {
            let (_, prefix) = realize_morphism(
                &mut self.ctx,
                &self.graph,
                self.graph.faces[fce].start,
                &self.graph.faces[fce].left[0..prefix_len],
            );
            prev_eq.lap(&prefix);
        }

        // Unify with previous face
        let eq = self.graph.faces[fce].eq.clone();
        if !self.unify_eq(cat, &eq, &prev_eq) {
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
