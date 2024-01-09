use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::{Interactive, VM};

type Ins = crate::vm::asm::Instruction;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    // Returns the face that is kept
    pub async fn merge_faces(&self, fce1: usize, fce2: usize) -> usize {
        let (fce1, fce2) = {
            let graph = self.graph.lock().await;
            if graph.graph.faces[fce1].label.name > graph.graph.faces[fce2].label.name {
                (fce2, fce1)
            } else {
                (fce1, fce2)
            }
        };
        let () = self.hide(GraphId::Face(fce2)).await;
        fce1
    }

    // Returns the edge that is kept
    pub async fn merge_edges(&self, src: usize, mph1: usize, mph2: usize) -> usize {
        let (mph1, mph2) = {
            let graph = self.graph.lock().await;
            assert_eq!(
                graph.graph.edges[src][mph1].0,
                graph.graph.edges[src][mph2].0
            );

            if graph.graph.edges[src][mph1].1.name > graph.graph.edges[src][mph2].1.name {
                (mph2, mph1)
            } else {
                (mph1, mph2)
            }
        };

        // Update equalities using the replaced morphism
        let nfaces = self.graph.lock().await.graph.faces.len();
        for fce in 0..nfaces {
            // Left side
            let nleft = {
                let graph = self.graph.lock().await;
                let mut nleft = graph.graph.faces[fce].left.clone();
                let mut nsrc = graph.graph.faces[fce].start;
                let mut changed = false;
                for nxt in 0..graph.graph.faces[fce].left.len() {
                    let mph = graph.graph.faces[fce].left[nxt];
                    if nsrc == src && mph == mph2 {
                        changed = true;
                        nleft[nxt] = mph1;
                    }
                    nsrc = graph.graph.edges[nsrc][mph].0;
                }
                if changed {
                    Some((nleft, graph.graph.faces[fce].left.clone()))
                } else {
                    None
                }
            };
            if let Some((nleft, oleft)) = nleft {
                self.register_instruction(Ins::RelocateFaceLeft(fce, oleft, nleft));
            }

            // Right side
            let nright = {
                let graph = self.graph.lock().await;
                let mut nright = graph.graph.faces[fce].right.clone();
                let mut nsrc = graph.graph.faces[fce].start;
                let mut changed = false;
                for nxt in 0..graph.graph.faces[fce].right.len() {
                    let mph = graph.graph.faces[fce].right[nxt];
                    if nsrc == src && mph == mph2 {
                        changed = true;
                        nright[nxt] = mph1;
                    }
                    nsrc = graph.graph.edges[nsrc][mph].0;
                }
                if changed {
                    Some((nright, graph.graph.faces[fce].right.clone()))
                } else {
                    None
                }
            };
            if let Some((nright, oright)) = nright {
                self.register_instruction(Ins::RelocateFaceRight(fce, oright, nright));
            }
        }

        let () = self.hide(GraphId::Morphism(src, mph2)).await;
        mph1
    }

    // Return the node that is kept
    pub async fn merge_nodes(&self, nd1: usize, nd2: usize) -> usize {
        let (nd1, nd2, nd2_mphs) = {
            let graph = self.graph.lock().await;
            if graph.graph.nodes[nd2].2.name > graph.graph.nodes[nd1].2.name {
                (nd2, nd1, graph.graph.edges[nd1].len())
            } else {
                (nd1, nd2, graph.graph.edges[nd2].len())
            }
        };
        // Update morphism starting at nd2
        for mph in (0..nd2_mphs).rev() {
            let () = self
                .register_instruction(Ins::RelocateMorphismSrc(nd2, nd1, mph))
                .await;
        }
        // Update morphisms ending at nd2
        let nnodes = self.graph.lock().await.graph.nodes.len();
        for src in 0..nnodes {
            let nmphs = self.graph.lock().await.graph.edges[src].len();
            for mph in (0..nmphs).rev() {
                if self.graph.lock().await.graph.edges[src][mph].0 == nd2 {
                    let () = self
                        .register_instruction(Ins::RelocateMorphismDst(src, mph, nd2, nd1))
                        .await;
                }
            }
        }
        // Update faces starting/ending at nd2
        let nfaces = self.graph.lock().await.graph.faces.len();
        for fce in 0..nfaces {
            if self.graph.lock().await.graph.faces[fce].start == nd2 {
                let () = self
                    .register_instruction(Ins::RelocateFaceSrc(fce, nd2, nd1))
                    .await;
            }
            if self.graph.lock().await.graph.faces[fce].end == nd2 {
                let () = self
                    .register_instruction(Ins::RelocateFaceDst(fce, nd2, nd1))
                    .await;
            }
        }
        let () = self.hide(GraphId::Node(nd2)).await;
        nd1
    }

    // Returns true if the two ids could be merged
    pub async fn merge_dwim(&self, id1: GraphId, id2: GraphId) -> bool {
        use GraphId::*;
        match (id1, id2) {
            (Node(n1), Node(n2)) => {
                let (v1, v2) = {
                    let graph = self.graph.lock().await;
                    let v1 = graph.graph.nodes[n1].0;
                    let v2 = graph.graph.nodes[n2].0;
                    (v1, v2)
                };
                if self
                    .ctx
                    .lock()
                    .await
                    .remote
                    .unify(std::iter::once((v1, v2)))
                    .unwrap()
                {
                    let () = self.change_state().await;
                    let _: usize = self.merge_nodes(n1, n2).await;
                    true
                } else {
                    false
                }
            }
            (Morphism(src1, mph1), Morphism(src2, mph2)) => {
                let (v1, v2) = {
                    let graph = self.graph.lock().await;
                    let v1 = graph.graph.edges[src1][mph1].2;
                    let v2 = graph.graph.edges[src2][mph2].2;
                    (v1, v2)
                };
                if self
                    .ctx
                    .lock()
                    .await
                    .remote
                    .unify(std::iter::once((v1, v2)))
                    .unwrap()
                {
                    let () = self.change_state().await;
                    let mut src = src1;
                    let mut mph1 = mph1;
                    let mut mph2 = mph2;
                    if src1 != src2 {
                        let nmphs = {
                            let graph = self.graph.lock().await;
                            graph.graph.edges[src1].len() + graph.graph.edges[src2].len()
                        };
                        let msrc = self.merge_nodes(src1, src2).await;
                        src = msrc;
                        if src == src1 {
                            mph2 = nmphs - mph2 - 1;
                        } else {
                            mph1 = nmphs - mph1 - 1;
                        }
                    }
                    let (dst1, dst2) = {
                        let graph = self.graph.lock().await;
                        let dst1 = graph.graph.edges[src][mph1].0;
                        let dst2 = graph.graph.edges[src][mph2].0;
                        (dst1, dst2)
                    };
                    if dst1 != dst2 {
                        let _: usize = self.merge_nodes(dst1, dst2).await;
                    }
                    let _: usize = self.merge_edges(src, mph1, mph2).await;
                    true
                } else {
                    false
                }
            }
            (Face(f1), Face(f2)) => {
                let eqs = {
                    let graph = self.graph.lock().await;
                    let fce1 = &graph.graph.faces[f1];
                    let fce2 = &graph.graph.faces[f2];
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
                        Some((fce1.eq.cat, eq1, eq2))
                    } else {
                        None
                    }
                };
                if let Some((cat, eq1, eq2)) = eqs {
                    if self.unify_eq(cat, &eq1, &eq2).await {
                        let _: usize = self.merge_faces(f1, f2).await;
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
    use futures::executor::block_on;

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
        assert_eq!(block_on(rmph), 0);
        assert_eq!(vm.graph.try_lock().unwrap().graph.faces[0].right[0], 0);
        assert_eq!(
            vm.graph.try_lock().unwrap().graph.edges[0][1].1.hidden,
            true
        );

        let rnode = vm.merge_nodes(2, 1);
        assert_eq!(block_on(rnode), 2);
        assert_eq!(vm.graph.try_lock().unwrap().graph.edges[0][0].0, 2);
        assert_eq!(vm.graph.try_lock().unwrap().graph.edges[0][1].0, 2);
        assert_eq!(vm.graph.try_lock().unwrap().graph.edges[2].len(), 1);
        assert_eq!(vm.graph.try_lock().unwrap().graph.edges[2][0].0, 2);
    }
}
