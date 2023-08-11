use super::config::Config;
use crate::graph::GraphId;
use crate::vm::Graph;

mod ccs;
mod edges;
mod engine;
mod nodes;
mod precompute;
pub use engine::LayoutEngine;

impl LayoutEngine {
    pub fn particles_for_graph(&mut self, cfg: &Config, graph: &mut Graph) {
        self.compute_structure(cfg, graph);
        self.particles_for_nodes(cfg, graph);
        self.particles_for_edges(cfg, graph);
        self.reset_components(cfg);
    }

    pub fn apply_forces<F>(&mut self, cfg: &Config, graph: &Graph, fixed: &F)
    where
        F: Fn(GraphId) -> bool,
    {
        self.apply_nodes_forces(cfg, graph, fixed);
        self.apply_edge_forces(cfg, graph, fixed);
        self.apply_cc_forces(cfg);
    }
}
