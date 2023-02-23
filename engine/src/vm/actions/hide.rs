use crate::data::ActualMorphism;
use crate::vm;
use crate::vm::graph::GraphId;
use crate::vm::VM;
use std::iter;
use std::ops::Deref;

impl VM {
    fn hide_node(&mut self, id: usize) {
        if self.graph.nodes[id].1.hidden {
            return;
        }

        self.graph.nodes[id].1.hidden = true;
        for src in 0..self.graph.nodes.len() {
            for mph in 0..self.graph.edges[src].len() {
                if src == id || self.graph.edges[src][mph].0 == id {
                    self.graph.edges[src][mph].1.hidden = true;
                }
            }
        }
    }

    // When hiding a node, hide all adjacent edges
    pub fn hide(&mut self, id: GraphId) {
        use GraphId::*;
        match id {
            Node(n) => self.hide_node(n),
            Morphism(src, mph) => self.graph.edges[src][mph].1.hidden = true,
            Face(f) => self.graph.faces[f].label.hidden = true,
        }
        self.layout()
    }

    // When revealing an edge, reveal its source and target nodes
    pub fn reveal(&mut self, id: GraphId) {
        use GraphId::*;
        match id {
            Node(n) => self.graph.nodes[n].1.hidden = false,
            Morphism(src, mph) => {
                let dst = self.graph.edges[src][mph].0;
                self.graph.edges[src][mph].1.hidden = false;
                self.graph.nodes[src].1.hidden = false;
                self.graph.nodes[dst].1.hidden = false;
            }
            Face(f) => self.graph.faces[f].label.hidden = false,
        }
        self.layout()
    }

    // Hide a morphism, and create an alias for all faces that go through that
    // morphism
    pub fn hide_and_replace_morphism(&mut self, src: usize, mph: usize, rep_fce: usize) {
        assert_eq!(src, self.graph.faces[rep_fce].start);
        assert_eq!(self.graph.edges[src][mph].0, self.graph.faces[rep_fce].end);

        // Replacement
        let rep = self.get_right_side(rep_fce).clone();
        let replace = |node: &mut usize, nxt: &usize| -> Option<Box<dyn Iterator<Item = usize>>> {
            let (dst, _, _) = &self.graph.edges[*node][*nxt];
            let prev = *node;
            *node = *dst;
            if prev == src && *nxt == mph {
                Some(Box::new(rep.iter().map(|i| *i)))
            } else {
                Some(Box::new(iter::once(*nxt)))
            }
        };
        for fce in 0..self.graph.faces.len() {
            let start = self.graph.faces[fce].start;
            self.graph.faces[fce].label.left = Some(
                vm::get_left_side(&self.graph.faces, fce)
                    .iter()
                    .scan(start, replace)
                    .flatten()
                    .collect(),
            );
            self.graph.faces[fce].label.right = Some(
                vm::get_right_side(&self.graph.faces, fce)
                    .iter()
                    .scan(start, replace)
                    .flatten()
                    .collect(),
            );
        }

        // Hiding
        self.hide(GraphId::Morphism(src, mph))
    }

    // Hide all identities in the graph
    pub fn hide_identities(&mut self) {
        for src in 0..self.graph.edges.len() {
            for mph in 0..self.graph.edges[src].len() {
                if let ActualMorphism::Identity(_) = self.graph.edges[src][mph].2.deref() {
                    self.graph.edges[src][mph].1.hidden = true;
                }
            }
        }
    }
}
