use crate::graph::GraphId;
use crate::normalizer::to_morphism;
use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::{Interactive, VM};

type Ins = asm::Instruction;

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    // Normalize morphism mph, and hide it if it changed
    pub fn split(&mut self, src: usize, mph: usize) {
        if let Some(path) = self.split_norm(src, mph) {
            self.hide(GraphId::Morphism(src, mph));

            // Replace mph by path in all equalities, the equality itself doesn't change
            for fce in 0..self.graph.faces.len() {
                let replace =
                    |node: &mut usize, nxt: &usize| -> Option<Box<dyn Iterator<Item = usize>>> {
                        let (dst, _, _, _) = &self.graph.edges[*node][*nxt];
                        let prev = *node;
                        *node = *dst;
                        if prev == src && *nxt == mph {
                            Some(Box::new(path.iter().map(|(_, m)| *m)))
                        } else {
                            Some(Box::new(std::iter::once(*nxt)))
                        }
                    };

                let left = self.graph.faces[fce]
                    .left
                    .iter()
                    .scan(self.graph.faces[fce].start, replace)
                    .flatten()
                    .collect::<Vec<_>>();
                let right = self.graph.faces[fce]
                    .right
                    .iter()
                    .scan(self.graph.faces[fce].start, replace)
                    .flatten()
                    .collect::<Vec<_>>();

                if left.len() != self.graph.faces[fce].left.len() {
                    self.register_instruction(Ins::RelocateFaceLeft(
                        fce,
                        self.graph.faces[fce].left.clone(),
                        left,
                    ));
                }
                if right.len() != self.graph.faces[fce].right.len() {
                    self.register_instruction(Ins::RelocateFaceRight(
                        fce,
                        self.graph.faces[fce].right.clone(),
                        right,
                    ));
                }
            }
        }
    }

    /// Normalize a morphism of the graph, then split it along composition and
    /// introduce the components as edges. Returns the new path as a sequence
    /// of edges in the graph if the edge wasn't already normal
    pub fn split_norm(&mut self, src: usize, mph: usize) -> Option<Vec<(usize, usize)>> {
        assert!(src < self.graph.nodes.len(), "src out of bounds");
        assert!(mph < self.graph.edges[src].len(), "mph out of bounds");

        let cat = self.graph.nodes[src].1;
        let sobj = self.graph.nodes[src].0;
        let dobj = self.graph.nodes[self.graph.edges[src][mph].0].0;
        let mobj = self.graph.edges[src][mph].2;

        let comps = to_morphism(&mut self.ctx, cat, sobj, dobj, mobj).comps;
        if comps.len() == 1 {
            return None;
        }

        let mut snode = src;
        let mut res = Vec::new();
        res.reserve(comps.len());
        for (_, _, m) in &comps {
            let (m, dnode) = self.insert_mph_at(snode, *m);
            res.push((snode, m));
            snode = dnode;
        }
        assert_eq!(snode, self.graph.edges[src][mph].0);
        Some(res)
    }
}

#[cfg(test)]
mod tests {
    use crate::data::EvarStatus::Grounded;
    use crate::data::Feature;
    use crate::graph::GraphImpl;
    use crate::remote::Mock;
    use crate::vm::VM;
    use std::default::Default;

    fn mk_ctx() -> (Mock, u64, u64, u64, u64, u64, u64, u64, u64, u64, u64) {
        let mut ctx = Mock::new();

        let cat = ctx.new_term("C".to_string(), None, Grounded);
        ctx.add_feat(cat, Feature::Category);

        let v = ctx.new_term("v".to_string(), None, Grounded);
        ctx.add_feat(v, Feature::Object { cat });
        let w = ctx.new_term("w".to_string(), None, Grounded);
        ctx.add_feat(w, Feature::Object { cat });
        let x = ctx.new_term("x".to_string(), None, Grounded);
        ctx.add_feat(x, Feature::Object { cat });
        let y = ctx.new_term("y".to_string(), None, Grounded);
        ctx.add_feat(y, Feature::Object { cat });
        let z = ctx.new_term("z".to_string(), None, Grounded);
        ctx.add_feat(z, Feature::Object { cat });

        let m1 = ctx.new_term("m1".to_string(), None, Grounded);
        ctx.add_feat(
            m1,
            Feature::Morphism {
                cat,
                src: v,
                dst: w,
            },
        );
        let m2 = ctx.new_term("m2".to_string(), None, Grounded);
        ctx.add_feat(
            m2,
            Feature::Morphism {
                cat,
                src: w,
                dst: x,
            },
        );
        let m3 = ctx.new_term("m3".to_string(), None, Grounded);
        ctx.add_feat(
            m3,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        let m4 = ctx.new_term("m4".to_string(), None, Grounded);
        ctx.add_feat(
            m4,
            Feature::Morphism {
                cat,
                src: y,
                dst: z,
            },
        );

        (ctx, cat, v, w, x, y, z, m1, m2, m3, m4)
    }

    #[test]
    fn split() {
        let (mut ctx, cat, v, w, x, y, z, m1, m2, m3, m4) = mk_ctx();

        let m34 = ctx.new_term("m34".to_string(), None, Grounded);
        ctx.add_feat(
            m34,
            Feature::Morphism {
                cat,
                src: x,
                dst: z,
            },
        );
        ctx.add_feat(
            m34,
            Feature::ComposeMph {
                cat,
                src: x,
                mid: y,
                dst: z,
                m1: m3,
                m2: m4,
            },
        );
        let m24 = ctx.new_term("m24".to_string(), None, Grounded);
        ctx.add_feat(
            m24,
            Feature::Morphism {
                cat,
                src: w,
                dst: z,
            },
        );
        ctx.add_feat(
            m24,
            Feature::ComposeMph {
                cat,
                src: w,
                mid: x,
                dst: z,
                m1: m2,
                m2: m34,
            },
        );
        let m = ctx.new_term("m".to_string(), None, Grounded);
        ctx.add_feat(
            m,
            Feature::Morphism {
                cat,
                src: v,
                dst: z,
            },
        );
        ctx.add_feat(
            m,
            Feature::ComposeMph {
                cat,
                src: v,
                mid: w,
                dst: z,
                m1,
                m2: m24,
            },
        );

        let gr = GraphImpl::<(), u64, (), (), ()> {
            nodes: vec![(v, cat, Default::default()), (z, cat, Default::default())],
            edges: vec![vec![(1, Default::default(), m, ())], vec![]],
            faces: vec![],
        };
        ctx.set_graph(gr);
        let mut vm = VM::<Mock, ()>::start(ctx);
        vm.split(0, 0);

        assert!(vm.graph.check(), "Graph is not valid after split");
        assert_eq!(vm.graph.nodes.len(), 5, "There should be 5 nodes now");
        assert_eq!(
            vm.graph.edges[0].len(),
            2,
            "There should be two outgoing edges from first node"
        );
        assert_eq!(vm.graph.faces.len(), 0, "No face should have been added");
        assert_ne!(
            vm.instructions.len(),
            0,
            "There should be at least one instruction"
        );
    }
}
