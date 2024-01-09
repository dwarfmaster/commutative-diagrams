use super::config::Config;
use crate::graph::GraphId;
use crate::vm::vm::GraphState;

mod ccs;
mod edges;
mod engine;
mod nodes;
mod precompute;
pub use engine::LayoutEngine;

impl GraphState {
    pub fn particles_for_graph(&mut self, cfg: &Config) {
        self.layout.compute_structure(cfg, &mut self.graph);
        self.layout.particles_for_nodes(cfg, &mut self.graph);
        self.layout.particles_for_edges(cfg, &mut self.graph);
        self.layout.reset_components(cfg);
    }

    pub fn apply_forces<F>(&mut self, cfg: &Config, fixed: &F)
    where
        F: Fn(GraphId) -> bool,
    {
        self.layout.apply_nodes_forces(cfg, &mut self.graph, fixed);
        self.layout.apply_edge_forces(cfg, &mut self.graph, fixed);
        self.layout.apply_cc_forces(cfg, &mut self.graph, fixed);
    }
}
