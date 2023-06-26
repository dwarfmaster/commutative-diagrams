use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::{Interactive, VM};

type Ins = asm::Instruction;

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
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
        assert_eq!(src, self.graph.nodes[node].0);
        let ndst = self.insert_node(dst, cat);
        self.register_instruction(Ins::InsertMorphism(node, ndst, mph));
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
