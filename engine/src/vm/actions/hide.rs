
use crate::vm::graph::GraphId;
use crate::vm::VM;

impl VM {
    fn hide_node(&mut self, id: usize) {
        if self.graph.nodes[id].1.hidden {
            return
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
}
