use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::{Interactive, VM};

type Ins = crate::vm::asm::Instruction;

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    // Returns the face that is kept
    pub fn merge_faces(&mut self, mut fce1: usize, mut fce2: usize) -> usize {
        if self.graph.faces[fce1].label.name > self.graph.faces[fce2].label.name {
            std::mem::swap(&mut fce1, &mut fce2);
        }
        assert!(self.graph.faces[fce1].eq == self.graph.faces[fce2].eq);
        self.hide(GraphId::Face(fce2));
        fce1
    }

    // Returns the edge that is kept
    pub fn merge_edges(&mut self, src: usize, mut mph1: usize, mut mph2: usize) -> usize {
        if self.graph.edges[src][mph1].1.name > self.graph.edges[src][mph2].1.name {
            std::mem::swap(&mut mph1, &mut mph2);
        }
        // We assume that in the proof assistant, the terms corresponding to the following are
        // equal:
        //  - self.graph.edges[src][mph1].2
        //  - self.graph.edges[src][mph2].2
        //  todo! call self.ctx.equalify to check ?

        // Update equalities using the replaced morphism
        for fce in 0..self.graph.faces.len() {
            // Left side
            let mut nleft = self.graph.faces[fce].left.clone();
            let mut nsrc = self.graph.faces[fce].start;
            let mut changed = false;
            for nxt in 0..self.graph.faces[fce].left.len() {
                let mph = self.graph.faces[fce].left[nxt];
                if nsrc == src && mph == mph2 {
                    changed = true;
                    nleft[nxt] = mph1;
                }
                nsrc = self.graph.edges[nsrc][mph].0;
            }
            if changed {
                self.register_instruction(Ins::RelocateFaceLeft(
                    fce,
                    self.graph.faces[fce].left.clone(),
                    nleft,
                ));
            }

            // Right side
            let mut nright = self.graph.faces[fce].right.clone();
            let mut nsrc = self.graph.faces[fce].start;
            let mut changed = false;
            for nxt in 0..self.graph.faces[fce].right.len() {
                let mph = self.graph.faces[fce].right[nxt];
                if nsrc == src && mph == mph2 {
                    changed = true;
                    nright[nxt] = mph1;
                }
                nsrc = self.graph.edges[nsrc][mph].0;
            }
            if changed {
                self.register_instruction(Ins::RelocateFaceRight(
                    fce,
                    self.graph.faces[fce].right.clone(),
                    nright,
                ));
            }
        }

        self.hide(GraphId::Morphism(src, mph2));
        mph1
    }

    // Return the node that is kept
    pub fn merge_nodes(&mut self, mut nd1: usize, mut nd2: usize) -> usize {
        if self.graph.nodes[nd2].2.name > self.graph.nodes[nd1].2.name {
            std::mem::swap(&mut nd1, &mut nd2);
        }
        // Update morphism starting at nd2
        for mph in (0..self.graph.edges[nd2].len()).rev() {
            self.register_instruction(Ins::RelocateMorphismSrc(nd2, nd1, mph));
        }
        // Update morphisms ending at nd2
        for src in 0..self.graph.nodes.len() {
            for mph in (0..self.graph.edges[src].len()).rev() {
                if self.graph.edges[src][mph].0 == nd2 {
                    self.register_instruction(Ins::RelocateMorphismDst(src, mph, nd2, nd1));
                }
            }
        }
        // Update faces starting/ending at nd2
        for fce in 0..self.graph.faces.len() {
            if self.graph.faces[fce].start == nd2 {
                self.register_instruction(Ins::RelocateFaceDst(fce, nd2, nd1));
            }
            if self.graph.faces[fce].end == nd2 {
                self.register_instruction(Ins::RelocateFaceDst(fce, nd2, nd1));
            }
        }
        self.hide(GraphId::Node(nd2));
        nd1
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
        assert_eq!(vm.graph.faces[0].right[0], 0);
        assert_eq!(vm.graph.edges[0][1].1.hidden, true);

        let rnode = vm.merge_nodes(2, 1);
        assert_eq!(rnode, 2);
        assert_eq!(vm.graph.edges[0][0].0, 2);
        assert_eq!(vm.graph.edges[0][1].0, 2);
        assert_eq!(vm.graph.edges[2].len(), 1);
        assert_eq!(vm.graph.edges[2][0].0, 2);
    }
}
