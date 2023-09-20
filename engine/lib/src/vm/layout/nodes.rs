use super::super::config::Config;
use super::LayoutEngine;
use crate::graph::GraphId;
use crate::vm::Graph;
use egui::{Pos2, Vec2};

// We precomputed the graph-theoretic distance between any two pair of nodes, so
// now we can use it to set spring force between connected nodes, with the ideal
// length of the pring proportional to the graph distance.

impl LayoutEngine {
    pub fn particles_for_nodes(&mut self, _cfg: &Config, graph: &mut Graph) {
        for node in 0..graph.nodes.len() {
            if graph.nodes[node].2.hidden {
                continue;
            }
            if let Some(id) = graph.nodes[node].2.pos {
                self.particles[id].cc = Some(self.structure.nodes_component[node]);
            } else {
                graph.nodes[node].2.pos =
                    Some(self.new_particle(Pos2::ZERO, Some(self.structure.nodes_component[node])));
            }
        }
    }

    pub fn apply_nodes_forces<F>(&mut self, cfg: &Config, graph: &Graph, fixed: &F)
    where
        F: Fn(GraphId) -> bool,
    {
        self.apply_nodes_graph_distance(cfg, graph, fixed);
    }

    // Apply spring forces proportional to graph theoretic distance
    fn apply_nodes_graph_distance<F>(&mut self, cfg: &Config, graph: &Graph, fixed: &F)
    where
        F: Fn(GraphId) -> bool,
    {
        let c = 1f32;
        for i in 0..graph.nodes.len() {
            for j in (i + 1)..graph.nodes.len() {
                if let Some(dist) = self.structure.distances[i][j] {
                    let dij = dist as f32;
                    let i_id = graph.nodes[i].2.pos.unwrap();
                    let j_id = graph.nodes[j].2.pos.unwrap();
                    let i_pos = self.particles[i_id].pos;
                    let j_pos = self.particles[j_id].pos;
                    let k = c / (dij * dij);
                    let l = cfg.layout.ideal_distance * dij;
                    let actual_dist = (i_pos - j_pos).length();
                    let f = k * (actual_dist - l);
                    let dir = if actual_dist < 1e-6f32 {
                        // If they are too close, just push them appart in any
                        // direction
                        let seed = (((i + j) * 17) % 31) as f32;
                        Vec2::angled((seed / 31f32) * std::f32::consts::TAU)
                    } else {
                        (j_pos - i_pos).normalized()
                    };
                    if !fixed(GraphId::Node(i)) {
                        self.add_force(i_id, f * dir);
                    }
                    if !fixed(GraphId::Node(j)) {
                        self.add_force(j_id, -f * dir);
                    }
                }
            }
        }
    }
}
