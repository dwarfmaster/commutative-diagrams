use crate::graph::GraphId;
use crate::normalizer::to_morphism;
use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::{Interactive, VM};
use core::ops::DerefMut;

type Ins = asm::Instruction;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    // Normalize morphism mph, and hide it if it changed
    pub async fn split(&self, src: usize, mph: usize) {
        if let Some(path) = self.split_norm(src, mph).await {
            self.hide(GraphId::Morphism(src, mph));

            let nfaces = self.graph.lock().await.graph.faces.len();
            // Replace mph by path in all equalities, the equality itself doesn't change
            for fce in 0..nfaces {
                let (left, old_left, right, old_right) = {
                    let graph = self.graph.lock().await;
                    let replace = |node: &mut usize,
                                   nxt: &usize|
                     -> Option<Box<dyn Iterator<Item = usize>>> {
                        let (dst, _, _, _) = &graph.graph.edges[*node][*nxt];
                        let prev = *node;
                        *node = *dst;
                        if prev == src && *nxt == mph {
                            Some(Box::new(path.iter().map(|(_, m)| *m)))
                        } else {
                            Some(Box::new(std::iter::once(*nxt)))
                        }
                    };

                    let left = graph.graph.faces[fce]
                        .left
                        .iter()
                        .scan(graph.graph.faces[fce].start, replace)
                        .flatten()
                        .collect::<Vec<_>>();
                    let right = graph.graph.faces[fce]
                        .right
                        .iter()
                        .scan(graph.graph.faces[fce].start, replace)
                        .flatten()
                        .collect::<Vec<_>>();

                    let old_left = if left.len() != graph.graph.faces[fce].left.len() {
                        Some(graph.graph.faces[fce].left.clone())
                    } else {
                        None
                    };
                    let old_right = if right.len() != graph.graph.faces[fce].right.len() {
                        Some(graph.graph.faces[fce].right.clone())
                    } else {
                        None
                    };
                    (left, old_left, right, old_right)
                };

                if let Some(oleft) = old_left {
                    let () = self
                        .register_instruction(Ins::RelocateFaceLeft(fce, oleft, left))
                        .await;
                }
                if let Some(oright) = old_right {
                    let () = self
                        .register_instruction(Ins::RelocateFaceRight(fce, oright, right))
                        .await;
                }
            }
        }
    }

    /// Normalize a morphism of the graph, then split it along composition and
    /// introduce the components as edges. Returns the new path as a sequence
    /// of edges in the graph if the edge wasn't already normal
    pub async fn split_norm(&self, src: usize, mph: usize) -> Option<Vec<(usize, usize)>> {
        let comps = {
            let graph = self.graph.lock().await;
            assert!(src < graph.graph.nodes.len(), "src out of bounds");
            assert!(mph < graph.graph.edges[src].len(), "mph out of bounds");

            let cat = graph.graph.nodes[src].1;
            let sobj = graph.graph.nodes[src].0;
            let dobj = graph.graph.nodes[graph.graph.edges[src][mph].0].0;
            let mobj = graph.graph.edges[src][mph].2;

            let ctx = &mut self.ctx.lock().await;
            let comps = to_morphism(ctx.deref_mut(), cat, sobj, dobj, mobj).comps;
            if comps.len() == 1 {
                return None;
            }
            comps
        };

        let mut snode = src;
        let mut res = Vec::new();
        res.reserve(comps.len());
        for (_, _, m) in &comps {
            let (m, dnode) = self.insert_mph_at(snode, *m).await;
            res.push((snode, m));
            snode = dnode;
        }
        assert_eq!(snode, self.graph.lock().await.graph.edges[src][mph].0);
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

        assert!(
            vm.graph.try_lock().unwrap().graph.check(),
            "Graph is not valid after split"
        );
        assert_eq!(
            vm.graph.try_lock().unwrap().graph.nodes.len(),
            5,
            "There should be 5 nodes now"
        );
        assert_eq!(
            vm.graph.try_lock().unwrap().graph.edges[0].len(),
            2,
            "There should be two outgoing edges from first node"
        );
        assert_eq!(
            vm.graph.try_lock().unwrap().graph.faces.len(),
            0,
            "No face should have been added"
        );
        assert_ne!(
            vm.ins.try_lock().unwrap().instructions.len(),
            0,
            "There should be at least one instruction"
        );
    }
}
