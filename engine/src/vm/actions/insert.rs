use crate::data::{Morphism, Object};
use crate::vm::asm;
use crate::vm::VM;

type Ins = asm::Instruction;

impl VM {
    /// Look over graph nodes, if obj is already present returns its index,
    /// otherwise insert it and return the index of the nely inserted node.
    pub fn insert_node(&mut self, obj: Object) -> usize {
        for n in 0..self.graph.nodes.len() {
            if self.graph.nodes[n].0 == obj {
                return n;
            }
        }
        self.register_instruction(Ins::InsertNode(obj));
        self.graph.nodes.len() - 1
    }

    /// Try to find the morphism in the output edges of node, and return its index.
    /// Otherwise add it and return its new index. Also return the index of the
    /// codomain of the morphism.
    pub fn insert_mph_at(&mut self, node: usize, mph: Morphism) -> (usize, usize) {
        assert!(
            node < self.graph.nodes.len(),
            "Trying to insert at unexisting node"
        );
        for m in 0..self.graph.edges[node].len() {
            if self.graph.edges[node][m].2 == mph {
                return (m, self.graph.edges[node][m].0);
            }
        }
        let dst = mph.dst(&self.ctx);
        let ndst = self.insert_node(dst);
        self.register_instruction(Ins::InsertMorphism(node, ndst, mph));
        (self.graph.edges[node].len() - 1, ndst)
    }

    /// Same as insert_mph_at, but finds or insert automatically the source of the
    /// morhism. Returns the index of the source node, that of the morphism and that
    /// of the destination.
    pub fn insert_mph(&mut self, mph: Morphism) -> (usize, usize, usize) {
        let src = mph.src(&self.ctx);
        let nsrc = self.insert_node(src);
        let (nmph, ndst) = self.insert_mph_at(nsrc, mph);
        (nsrc, nmph, ndst)
    }
}

#[cfg(test)]
mod tests {
    use crate::data::Context;
    use crate::dsl::{cat, mph, obj};
    use crate::vm::{Graph, VM};

    #[test]
    fn basic() {
        let ctx = Context::new();
        let cat = cat!(ctx, :0);
        let x = obj!(ctx, (:1) in cat);
        let y = obj!(ctx, (:2) in cat);
        let m1 = mph!(ctx, (:3) : x -> y);
        let m2 = mph!(ctx, (:4) : x -> y);

        let gr = Graph {
            nodes: Vec::new(),
            edges: Vec::new(),
            faces: Vec::new(),
        };
        let mut vm = VM::new(ctx, gr, Vec::new(), Vec::new());

        let (m1_src, _, _) = vm.insert_mph(m1);
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
        assert!(vm.graph.check(&vm.ctx), "The graph should still be valid");
        assert_eq!(vm.graph.nodes.len(), 2, "There should still be 2 nodes");
        assert_eq!(
            vm.graph.edges[m1_src].len(),
            2,
            "There should be 2 outgoing edges"
        );

        let (m2_src, m2_id2, _) = vm.insert_mph(m2);
        assert_eq!(m2_src, m1_src, "m1 and m2 have the same source");
        assert_eq!(m2_id, m2_id2, "m2 was already present in the graph");
    }
}
