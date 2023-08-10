use crate::graph::GraphId;
use crate::vm::Graph;

mod edges;
mod engine;
mod nodes;
mod precompute;
pub use engine::LayoutEngine;

impl LayoutEngine {
    pub fn particles_for_graph(&mut self, graph: &mut Graph) {
        self.compute_structure(graph);
        self.particles_for_nodes(graph);
        self.particles_for_edges(graph);
    }

    pub fn apply_forces<F>(&mut self, graph: &Graph, fixed: &F)
    where
        F: Fn(GraphId) -> bool,
    {
        self.apply_nodes_forces(graph, fixed);
        self.apply_edge_forces(graph, fixed);
    }
}
