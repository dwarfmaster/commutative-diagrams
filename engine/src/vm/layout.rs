use crate::vm::Graph;

mod engine;
pub use engine::LayoutEngine;

impl LayoutEngine {
    pub fn particles_for_graph(&mut self, graph: &mut Graph) {
        // TODO nodes
        // TODO edges
    }

    pub fn apply_forces(&mut self, graph: &Graph) {}
}
