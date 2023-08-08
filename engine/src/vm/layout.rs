use crate::vm::Graph;

mod edges;
mod engine;
mod nodes;
pub use engine::LayoutEngine;

impl LayoutEngine {
    pub fn particles_for_graph(&mut self, graph: &mut Graph) {
        self.particles_for_nodes(graph);
        self.particles_for_edges(graph);
    }

    pub fn apply_forces(&mut self, graph: &Graph) {
        self.apply_nodes_forces(graph);
        self.apply_edge_forces(graph);
    }
}
