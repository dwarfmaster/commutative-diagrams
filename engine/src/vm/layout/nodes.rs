use super::LayoutEngine;
use crate::vm::Graph;
use egui::{Vec2,Pos2};

// We precomputed the graph-theoretic distance between any two pair of nodes, so
// now we can use it to set spring force between connected nodes, with the ideal
// length of the pring proportional to the graph distance.

impl LayoutEngine {
    pub fn particles_for_nodes(&mut self, graph: &mut Graph) {
        for node in graph.nodes.iter_mut() {
            if node.2.hidden {
                continue;
            }
            if node.2.pos.is_none() {
                node.2.pos = Some(self.new_particle(Pos2::ZERO));
            }
        }
    }

    pub fn apply_nodes_forces(&mut self, graph: &Graph) {
        self.apply_nodes_graph_distance(graph);
    }

    // Apply spring forces proportional to graph theoretic distance
    fn apply_nodes_graph_distance(&mut self, graph: &Graph) {
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
                    let l = self.ideal_distance * dij;
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
                    self.add_force(i_id, f * dir);
                    self.add_force(j_id, -f * dir);
                }
            }
        }
    }
}
