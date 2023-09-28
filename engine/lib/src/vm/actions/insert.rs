use crate::graph::eq::Eq;
use crate::graph::Face;
use crate::normalizer;
use crate::normalizer::to_morphism;
use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::{FaceLabel, FaceStatus, Interactive, VM};

type Ins = asm::Instruction;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    /// Look over graph nodes, if obj is already present returns its index,
    /// otherwise insert it and return the index of the nely inserted node.
    pub fn insert_node(&mut self, obj: u64, cat: u64) -> usize {
        for n in 0..self.graph.nodes.len() {
            if self.graph.nodes[n].0 == obj && self.graph.nodes[n].1 == cat {
                return n;
            }
        }
        self.register_instruction(Ins::InsertNode(obj, cat));
        self.graph.nodes.len() - 1
    }

    /// Try to find the morphism in the output edges of node, and return its index.
    /// Otherwise add it and return its new index. Also return the index of the
    /// codomain of the morphism.
    pub fn insert_mph_at(&mut self, node: usize, mph: u64) -> (usize, usize) {
        assert!(
            node < self.graph.nodes.len(),
            "Trying to insert at unexisting node"
        );
        for m in 0..self.graph.edges[node].len() {
            if self.graph.edges[node][m].2 == mph {
                return (m, self.graph.edges[node][m].0);
            }
        }
        let cat = self.graph.nodes[node].1;
        let (src, dst) = self.ctx.is_mph(mph, cat).unwrap();
        let ndst = self.insert_node(dst, cat);
        let morph = to_morphism(&mut self.ctx, cat, src, dst, mph);
        self.register_instruction(Ins::InsertMorphism(node, ndst, mph, morph));
        (self.graph.edges[node].len() - 1, ndst)
    }

    /// Same as insert_mph_at, but finds or insert automatically the source of the
    /// morhism. Returns the index of the source node, that of the morphism and that
    /// of the destination.
    pub fn insert_mph(&mut self, mph: u64, cat: u64) -> (usize, usize, usize) {
        let (src, _) = self.ctx.is_mph(mph, cat).unwrap();
        let nsrc = self.insert_node(src, cat);
        let (nmph, ndst) = self.insert_mph_at(nsrc, mph);
        (nsrc, nmph, ndst)
    }

    /// Insert a new equality as a new face. Starts by normalizing its sides and
    /// adding all parts.
    pub fn insert_eq(&mut self, eq: u64, cat: u64) -> usize {
        let (src, dst, left, right) = self.ctx.is_eq(eq, cat).unwrap();
        let nsrc = self.insert_node(src, cat);
        let ndst = self.insert_node(dst, cat);
        let left_mph = normalizer::to_morphism(&mut self.ctx, cat, src, dst, left);
        let right_mph = normalizer::to_morphism(&mut self.ctx, cat, src, dst, right);

        let left_path = {
            let mut path = Vec::new();
            let mut src = nsrc;
            for (_, _, cmph) in left_mph.comps.iter() {
                let (nmph, nsrc) = self.insert_mph_at(src, *cmph);
                path.push(nmph);
                src = nsrc;
            }
            path
        };
        let right_path = {
            let mut path = Vec::new();
            let mut src = nsrc;
            for (_, _, cmph) in right_mph.comps.iter() {
                let (nmph, nsrc) = self.insert_mph_at(src, *cmph);
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
                label: self.ctx.get_stored_label(eq),
                name: "".to_string(),
                status: FaceStatus::Refined,
            },
        };
        let id = self.graph.faces.len();
        self.register_instruction(Ins::InsertFace(face));
        id
    }
}

#[cfg(test)]
mod tests {
    use crate::data::{EvarStatus, Feature};
    use crate::remote::Mock;
    use crate::vm::VM;

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

        let (m1_src, _, _) = vm.insert_mph(m1, cat);
        assert_eq!(
            vm.graph.nodes.len(),
            2,
            "There should be two nodes after the insertion"
        );
        assert_eq!(
            vm.graph.edges.len(),
            2,
            "The len of edges should be the same as the number of nodes"
        );

        let (m2_id, _) = vm.insert_mph_at(m1_src, m2.clone());
        assert!(vm.graph.check(), "The graph should still be valid");
        assert_eq!(vm.graph.nodes.len(), 2, "There should still be 2 nodes");
        assert_eq!(
            vm.graph.edges[m1_src].len(),
            2,
            "There should be 2 outgoing edges"
        );

        let (m2_src, m2_id2, _) = vm.insert_mph(m2, cat);
        assert_eq!(m2_src, m1_src, "m1 and m2 have the same source");
        assert_eq!(m2_id, m2_id2, "m2 was already present in the graph");
    }
}
