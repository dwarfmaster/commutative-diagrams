use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::{Interactive, VM};

type Ins = crate::vm::asm::Instruction;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    // Returns the face that is kept
    pub fn merge_faces(&mut self, mut fce1: usize, mut fce2: usize) -> usize {
        if self.graph.graph.faces[fce1].label.name > self.graph.graph.faces[fce2].label.name {
            std::mem::swap(&mut fce1, &mut fce2);
        }
        self.hide(GraphId::Face(fce2));
        fce1
    }

    // Replace an edge with a path in the graph
    pub fn splice_edge(&mut self, src: usize, mph: usize, mphs: &[usize]) {
        {
            let mut dst = src;
            for m in mphs {
                dst = self.graph.graph.edges[dst][*m].0;
            }
            assert_eq!(self.graph.graph.edges[src][mph].0, dst);
        }

        // Update equalities using the replaced morphism
        for fce in 0..self.graph.graph.faces.len() {
            // Left side
            let mut nsrc = self.graph.graph.faces[fce].start;
            let mut splice_at = Vec::new();
            for nxt in 0..self.graph.graph.faces[fce].left.len() {
                let m = self.graph.graph.faces[fce].left[nxt];
                if nsrc == src && m == mph {
                    splice_at.push(nxt);
                }
                nsrc = self.graph.graph.edges[nsrc][m].0;
            }
            if !splice_at.is_empty() {
                let mut nleft = self.graph.graph.faces[fce].left.clone();
                for splt in splice_at.iter().rev() {
                    nleft.splice(*splt..(*splt + 1), mphs.iter().copied());
                }
                self.register_instruction(Ins::RelocateFaceLeft(
                    fce,
                    self.graph.graph.faces[fce].left.clone(),
                    nleft,
                ));
            }

            // Right side
            let mut nsrc = self.graph.graph.faces[fce].start;
            let mut splice_at = Vec::new();
            for nxt in 0..self.graph.graph.faces[fce].right.len() {
                let m = self.graph.graph.faces[fce].right[nxt];
                if nsrc == src && m == mph {
                    splice_at.push(nxt);
                }
                nsrc = self.graph.graph.edges[nsrc][m].0;
            }
            if !splice_at.is_empty() {
                let mut nright = self.graph.graph.faces[fce].left.clone();
                for splt in splice_at.iter().rev() {
                    nright.splice(*splt..(*splt + 1), mphs.iter().copied());
                }
                self.register_instruction(Ins::RelocateFaceRight(
                    fce,
                    self.graph.graph.faces[fce].right.clone(),
                    nright,
                ));
            }
        }

        {
            let mut s = src;
            for m in mphs {
                self.hide(GraphId::Morphism(s, *m));
                s = self.graph.graph.edges[s][*m].0;
            }
        }
    }

    // Returns the edge that is kept
    pub fn merge_edges(&mut self, src: usize, mut mph1: usize, mut mph2: usize) -> usize {
        if self.graph.graph.edges[src][mph1].1.name > self.graph.graph.edges[src][mph2].1.name {
            std::mem::swap(&mut mph1, &mut mph2);
        }
        self.splice_edge(src, mph2, &[mph1]);
        mph1
    }

    // Return the node that is kept
    pub fn merge_nodes(&mut self, mut nd1: usize, mut nd2: usize) -> usize {
        if self.graph.graph.nodes[nd2].2.name > self.graph.graph.nodes[nd1].2.name {
            std::mem::swap(&mut nd1, &mut nd2);
        }
        // Update morphism starting at nd2
        for mph in (0..self.graph.graph.edges[nd2].len()).rev() {
            self.register_instruction(Ins::RelocateMorphismSrc(nd2, nd1, mph));
        }
        // Update morphisms ending at nd2
        for src in 0..self.graph.graph.nodes.len() {
            for mph in (0..self.graph.graph.edges[src].len()).rev() {
                if self.graph.graph.edges[src][mph].0 == nd2 {
                    self.register_instruction(Ins::RelocateMorphismDst(src, mph, nd2, nd1));
                }
            }
        }
        // Update faces starting/ending at nd2
        for fce in 0..self.graph.graph.faces.len() {
            if self.graph.graph.faces[fce].start == nd2 {
                self.register_instruction(Ins::RelocateFaceSrc(fce, nd2, nd1));
            }
            if self.graph.graph.faces[fce].end == nd2 {
                self.register_instruction(Ins::RelocateFaceDst(fce, nd2, nd1));
            }
        }
        self.hide(GraphId::Node(nd2));
        nd1
    }

    // Returns true if the two ids could be merged
    pub fn merge_dwim(&mut self, id1: GraphId, id2: GraphId) -> bool {
        use GraphId::*;
        match (id1, id2) {
            (Node(n1), Node(n2)) => {
                if self
                    .ctx
                    .remote
                    .unify(std::iter::once((
                        self.graph.graph.nodes[n1].0,
                        self.graph.graph.nodes[n2].0,
                    )))
                    .unwrap()
                {
                    self.ensure_morphisms_invariant();
                    self.change_state();
                    self.merge_nodes(n1, n2);
                    true
                } else {
                    false
                }
            }
            (Morphism(src1, mph1), Morphism(src2, mph2)) => {
                if self
                    .ctx
                    .remote
                    .unify(std::iter::once((
                        self.graph.graph.edges[src1][mph1].2,
                        self.graph.graph.edges[src2][mph2].2,
                    )))
                    .unwrap()
                {
                    self.ensure_morphisms_invariant();
                    self.change_state();
                    let mut src = src1;
                    let mut mph1 = mph1;
                    let mut mph2 = mph2;
                    if src1 != src2 {
                        let nmphs =
                            self.graph.graph.edges[src1].len() + self.graph.graph.edges[src2].len();
                        let msrc = self.merge_nodes(src1, src2);
                        src = msrc;
                        if src == src1 {
                            mph2 = nmphs - mph2 - 1;
                        } else {
                            mph1 = nmphs - mph1 - 1;
                        }
                    }
                    let dst1 = self.graph.graph.edges[src][mph1].0;
                    let dst2 = self.graph.graph.edges[src][mph2].0;
                    if dst1 != dst2 {
                        self.merge_nodes(dst1, dst2);
                    }
                    self.merge_edges(src, mph1, mph2);
                    true
                } else {
                    false
                }
            }
            (Face(f1), Face(f2)) => {
                let fce1 = &self.graph.graph.faces[f1];
                let fce2 = &self.graph.graph.faces[f2];
                if fce1.start == fce2.start
                    && fce1.end == fce2.end
                    && (fce1.left == fce2.left && fce1.right == fce2.right
                        || fce1.left == fce2.right && fce1.right == fce2.left)
                {
                    let eq1 = fce1.eq.clone();
                    let mut eq2 = fce2.eq.clone();
                    if fce1.left == fce2.right {
                        eq2.inv();
                    }
                    if self.unify_eq(fce1.eq.cat, &eq1, &eq2) {
                        self.merge_faces(f1, f2);
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::data::EvarStatus::Grounded;
    use crate::data::Feature;
    use crate::graph::{FaceParsed, GraphParsed};
    use crate::remote::Mock;
    use crate::vm::VM;

    #[test]
    fn merging() {
        let mut ctx = Mock::new();
        let cat = ctx.new_term("C".to_string(), None, Grounded);
        ctx.add_feat(cat, Feature::Category);
        let x = ctx.new_term("x".to_string(), None, Grounded);
        ctx.add_feat(x, Feature::Object { cat });
        let m = ctx.new_term("m".to_string(), None, Grounded);
        ctx.add_feat(
            m,
            Feature::Morphism {
                cat,
                src: x,
                dst: x,
            },
        );
        let mm = ctx.new_term("m o m".to_string(), None, Grounded);
        ctx.add_feat(
            mm,
            Feature::Morphism {
                cat,
                src: x,
                dst: x,
            },
        );
        ctx.add_feat(
            mm,
            Feature::ComposeMph {
                cat,
                src: x,
                mid: x,
                dst: x,
                m1: m,
                m2: m,
            },
        );
        let eq = ctx.new_term("H".to_string(), None, Grounded);
        ctx.add_feat(
            eq,
            Feature::Equality {
                cat,
                src: x,
                dst: x,
                left: m,
                right: mm,
            },
        );

        let face = FaceParsed {
            start: 0,
            end: 2,
            left: vec![2],
            right: vec![1, 0],
            eq,
            label: Default::default(),
        };
        let gr = GraphParsed {
            nodes: vec![
                (x, cat, Default::default()),
                (x, cat, Default::default()),
                (x, cat, Default::default()),
            ],
            edges: vec![
                vec![
                    (1, Default::default(), m, ()),
                    (1, Default::default(), m, ()),
                    (2, Default::default(), m, ()),
                ],
                vec![(2, Default::default(), m, ())],
                vec![],
            ],
            faces: vec![face],
        };
        ctx.set_graph(gr);
        let mut vm = VM::<Mock, ()>::start(ctx);

        let rmph = vm.merge_edges(0, 0, 1);
        assert_eq!(rmph, 0);
        assert_eq!(vm.graph.graph.faces[0].right[0], 0);
        assert_eq!(vm.graph.graph.edges[0][1].1.hidden, true);

        let rnode = vm.merge_nodes(2, 1);
        assert_eq!(rnode, 2);
        assert_eq!(vm.graph.graph.edges[0][0].0, 2);
        assert_eq!(vm.graph.graph.edges[0][1].0, 2);
        assert_eq!(vm.graph.graph.edges[2].len(), 1);
        assert_eq!(vm.graph.graph.edges[2][0].0, 2);
    }
}
