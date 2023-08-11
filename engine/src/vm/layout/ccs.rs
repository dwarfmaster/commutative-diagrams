use super::super::config::Config;
use super::engine::ConnectedComponent;
use super::LayoutEngine;
use crate::graph::GraphId;
use crate::vm::Graph;
use egui::Vec2;

impl LayoutEngine {
    pub fn reset_components(&mut self, _cfg: &Config) {
        self.components
            .resize(self.structure.ccs.len(), ConnectedComponent::new());
    }

    pub fn apply_cc_forces<F>(&mut self, _cfg: &Config, graph: &Graph, fixed: &F)
    where
        F: Fn(GraphId) -> bool,
    {
        self.reset_cc_forces();
        self.apply_cc_forces_repulse();
        self.apply_cc_forces_to_graph(graph, fixed);
    }

    fn reset_cc_forces(&mut self) {
        for cc in self.components.iter_mut() {
            cc.force = Vec2::ZERO;
        }
    }

    fn apply_cc_forces_repulse(&mut self) {
        let c = 1f32;
        let dist = 20f32;
        for cc1 in 0..self.components.len() {
            let r1 = self.components[cc1].rect.expand(dist);
            for cc2 in (cc1 + 1)..self.components.len() {
                let r2 = self.components[cc2].rect.expand(dist);

                let int = r1.intersect(r2);
                let w = int.width().clamp(0f32, f32::INFINITY);
                let h = int.height().clamp(0f32, f32::INFINITY);

                let dir = r2.center() - r1.center();
                let dir = if dir.length_sq() < 1f32 {
                    Vec2::X
                } else {
                    dir.normalized()
                };
                let f = c * (1f32 + 5f32 * w.min(h)).ln() * dir;
                self.components[cc1].force += -f;
                self.components[cc2].force += f;
            }
        }
    }

    fn apply_cc_forces_to_graph<F>(&mut self, graph: &Graph, fixed: &F)
    where
        F: Fn(GraphId) -> bool,
    {
        for src in 0..graph.nodes.len() {
            if graph.nodes[src].2.hidden {
                continue;
            }

            let cc = self.structure.nodes_component[src];
            if !fixed(GraphId::Node(src)) {
                let src_id = graph.nodes[src].2.pos.unwrap();
                self.add_force(src_id, self.components[cc].force);
            }

            for mph in 0..graph.edges[src].len() {
                if graph.edges[src][mph].1.hidden {
                    continue;
                }
                if !fixed(GraphId::Morphism(src, mph)) {
                    let control_id = graph.edges[src][mph].1.control.unwrap();
                    self.add_force(control_id, self.components[cc].force);
                }
            }
        }
    }
}
