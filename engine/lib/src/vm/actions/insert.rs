use crate::graph::eq::Eq;
use crate::graph::Face;
use crate::normalizer;
use crate::normalizer::to_morphism;
use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::{FaceLabel, FaceStatus, Interactive, VM};
use core::ops::DerefMut;

type Ins = asm::Instruction;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    /// Look over graph nodes, if obj is already present returns its index,
    /// otherwise insert it and return the index of the nely inserted node.
    pub async fn insert_node(&self, obj: u64, cat: u64) -> usize {
        let len = {
            let graph = self.graph.lock().await;
            for n in 0..graph.graph.nodes.len() {
                if graph.graph.nodes[n].0 == obj && graph.graph.nodes[n].1 == cat {
                    return n;
                }
            }
            graph.graph.nodes.len()
        };
        let () = self.register_instruction(Ins::InsertNode(obj, cat)).await;
        len
    }

    /// Try to find the morphism in the output edges of node, and return its index.
    /// Otherwise add it and return its new index. Also return the index of the
    /// codomain of the morphism.
    pub async fn insert_mph_at(&self, node: usize, mph: u64) -> (usize, usize) {
        let (cat, src, dst, len) = {
            let graph = self.graph.lock().await;
            assert!(
                node < graph.graph.nodes.len(),
                "Trying to insert at unexisting node"
            );
            for m in 0..graph.graph.edges[node].len() {
                if graph.graph.edges[node][m].2 == mph {
                    return (m, graph.graph.edges[node][m].0);
                }
            }
            let cat = graph.graph.nodes[node].1;
            let (src, dst) = self.ctx.lock().await.is_mph(mph, cat).unwrap();
            (cat, src, dst, graph.graph.edges[node].len())
        };
        let ndst = self.insert_node(dst, cat).await;
        let morph = {
            let ctx = &mut self.ctx.lock().await;
            to_morphism(ctx.deref_mut(), cat, src, dst, mph)
        };
        self.register_instruction(Ins::InsertMorphism(node, ndst, mph, morph))
            .await;
        (len, ndst)
    }

    /// Same as insert_mph_at, but finds or insert automatically the source of the
    /// morhism. Returns the index of the source node, that of the morphism and that
    /// of the destination.
    pub async fn insert_mph(&self, mph: u64, cat: u64) -> (usize, usize, usize) {
        let (src, _) = self.ctx.lock().await.is_mph(mph, cat).unwrap();
        let nsrc = self.insert_node(src, cat).await;
        let (nmph, ndst) = self.insert_mph_at(nsrc, mph).await;
        (nsrc, nmph, ndst)
    }

    /// Insert a new equality as a new face. Starts by normalizing its sides and
    /// adding all parts.
    pub async fn insert_eq(&self, eq: u64, cat: u64) -> usize {
        let (src, dst, left, right) = self.ctx.lock().await.is_eq(eq, cat).unwrap();
        let nsrc = self.insert_node(src, cat).await;
        let ndst = self.insert_node(dst, cat).await;
        let (left_mph, right_mph) = {
            let ctx = &mut self.ctx.lock().await;
            let lmph = normalizer::to_morphism(ctx.deref_mut(), cat, src, dst, left);
            let rmph = normalizer::to_morphism(ctx.deref_mut(), cat, src, dst, right);
            (lmph, rmph)
        };

        let left_path = {
            let mut path = Vec::new();
            let mut src = nsrc;
            for (_, _, cmph) in left_mph.comps.iter() {
                let (nmph, nsrc) = self.insert_mph_at(src, *cmph).await;
                path.push(nmph);
                src = nsrc;
            }
            path
        };
        let right_path = {
            let mut path = Vec::new();
            let mut src = nsrc;
            for (_, _, cmph) in right_mph.comps.iter() {
                let (nmph, nsrc) = self.insert_mph_at(src, *cmph).await;
                path.push(nmph);
                src = nsrc;
            }
            path
        };

        let face = Face {
            start: nsrc,
            end: ndst,
            left: left_path,
            right: right_path,
            eq: Eq::atomic(cat, left_mph, right_mph, eq),
            label: FaceLabel {
                folded: false,
                hidden: false,
                parent: None,
                children: Vec::new(),
                label: self.ctx.lock().await.get_stored_label(eq),
                name: "".to_string(),
                status: FaceStatus::Refined,
            },
        };
        let id = self.graph.lock().await.graph.faces.len();
        self.register_instruction(Ins::InsertFace(face)).await;
        id
    }
}

#[cfg(test)]
mod tests {
    use crate::data::{EvarStatus, Feature};
    use crate::remote::Mock;
    use crate::vm::VM;
    use futures::executor::block_on;

    #[test]
    fn basic() {
        use EvarStatus::Grounded;

        let mut ctx = Mock::new();
        let cat = ctx.new_term("C".to_string(), None, Grounded);
        ctx.add_feat(cat, Feature::Category);
        let x = ctx.new_term("x".to_string(), None, Grounded);
        ctx.add_feat(x, Feature::Object { cat });
        let y = ctx.new_term("y".to_string(), None, Grounded);
        ctx.add_feat(y, Feature::Object { cat });
        let m1 = ctx.new_term("m1".to_string(), None, Grounded);
        ctx.add_feat(
            m1,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        let m2 = ctx.new_term("m2".to_string(), None, Grounded);
        ctx.add_feat(
            m2,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );

        let mut vm = VM::<Mock, ()>::start(ctx);

        let (m1_src, _, _) = block_on(vm.insert_mph(m1, cat));
        assert_eq!(
            vm.graph.try_lock().unwrap().graph.nodes.len(),
            2,
            "There should be two nodes after the insertion"
        );
        assert_eq!(
            vm.graph.try_lock().unwrap().graph.edges.len(),
            2,
            "The len of edges should be the same as the number of nodes"
        );

        let (m2_id, _) = block_on(vm.insert_mph_at(m1_src, m2.clone()));
        assert!(
            vm.graph.try_lock().unwrap().graph.check(),
            "The graph should still be valid"
        );
        assert_eq!(
            vm.graph.try_lock().unwrap().graph.nodes.len(),
            2,
            "There should still be 2 nodes"
        );
        assert_eq!(
            vm.graph.try_lock().unwrap().graph.edges[m1_src].len(),
            2,
            "There should be 2 outgoing edges"
        );

        let (m2_src, m2_id2, _) = block_on(vm.insert_mph(m2, cat));
        assert_eq!(m2_src, m1_src, "m1 and m2 have the same source");
        assert_eq!(m2_id, m2_id2, "m2 was already present in the graph");
    }
}
