use super::LayoutEngine;
use crate::vm::Graph;
use egui::Vec2;
use rand::distributions::Uniform;
use rand::{thread_rng, Rng};
use std::collections::HashSet;

// We add two kind of forces for nodes:
// - Add attractive force along edges
// - A repulsive force between any pairs
// The algorithm is very simple and is taken from:
//   Peter Eades. A heuristic for graph drawing, 1984

impl LayoutEngine {
    pub fn particles_for_nodes(&mut self, graph: &mut Graph) {
        let mut rng = thread_rng();
        let dist = Uniform::new(0.0f32, std::f32::consts::TAU);
        for node in graph.nodes.iter_mut() {
            if node.2.hidden {
                continue;
            }
            if node.2.pos.is_none() {
                let pos = (100.0f32 * Vec2::angled(rng.sample(dist))).to_pos2();
                node.2.pos = Some(self.new_particle(pos));
            }
        }
    }

    pub fn apply_nodes_forces(&mut self, graph: &Graph) {
        self.apply_nodes_attractive(graph);
        self.apply_nodes_repulsive(graph);
    }

    // Spread nodes
    fn apply_nodes_repulsive(&mut self, graph: &Graph) {
        let c = self.ideal_distance * self.ideal_distance;
        for i in 0..graph.nodes.len() {
            if graph.nodes[i].2.hidden {
                continue;
            }
            for j in (i + 1)..graph.nodes.len() {
                if graph.nodes[j].2.hidden {
                    continue;
                }
                let pi_id = graph.nodes[i].2.pos.unwrap();
                let pi = self.particles[pi_id].pos;
                let pj_id = graph.nodes[j].2.pos.unwrap();
                let pj = self.particles[pj_id].pos;
                let dist = pi.distance_sq(pj);
                if dist > 1e-6f32 {
                    let dir = (pj - pi).normalized();
                    let f = (c / dist) * dir;
                    self.add_force(pi_id, -f);
                    self.add_force(pj_id, f);
                }
            }
        }
    }

    // Bring nodes closer along edges.
    fn apply_nodes_attractive(&mut self, graph: &Graph) {
        // We make sure to add the force only once between any pair of edge that is connected
        // multiple times
        let mut seen: HashSet<(usize, usize)> = HashSet::new();
        let order_pair = |p: (usize, usize)| -> (usize, usize) {
            if p.0 > p.1 {
                (p.1, p.0)
            } else {
                p
            }
        };

        let c = 2f32 * self.ideal_distance;
        let d = self.ideal_distance;
        for src in 0..graph.nodes.len() {
            if graph.nodes[src].2.hidden {
                continue;
            }
            for mph in 0..graph.edges[src].len() {
                if graph.edges[src][mph].1.hidden {
                    continue;
                }
                let dst = graph.edges[src][mph].0;
                let pair = order_pair((src, dst));
                if seen.contains(&pair) {
                    continue;
                }
                seen.insert(pair);

                let src_id = graph.nodes[src].2.pos.unwrap();
                let dst_id = graph.nodes[dst].2.pos.unwrap();
                let src = self.particles[src_id].pos;
                let dst = self.particles[dst_id].pos;
                let dir = (dst - src).normalized();
                let f = c * (src.distance(dst) / d).ln() * dir;
                self.add_force(src_id, f);
                self.add_force(dst_id, -f);
            }
        }
    }
}
