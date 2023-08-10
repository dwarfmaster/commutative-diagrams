use super::LayoutEngine;
use crate::graph::GraphId;
use crate::vm::Graph;

// We want two forces on edges:
// - one that try to keep them aligned by attracting the control point to the
//   mean of its extremities
// - one that push them away from other control points and nodes

impl LayoutEngine {
    pub fn particles_for_edges(&mut self, graph: &mut Graph) {
        for src in 0..graph.nodes.len() {
            for mph in 0..graph.edges[src].len() {
                if graph.edges[src][mph].1.hidden {
                    continue;
                }
                let cc = self.structure.nodes_component[src];
                if let Some(id) = graph.edges[src][mph].1.control {
                    self.particles[id].cc = Some(cc);
                } else {
                    let dst = graph.edges[src][mph].0;
                    let src_pos = self.get_pos(graph.nodes[src].2.pos.unwrap());
                    let dst_pos = self.get_pos(graph.nodes[dst].2.pos.unwrap());
                    let pos = src_pos + 0.5f32 * (dst_pos - src_pos);
                    graph.edges[src][mph].1.control = Some(self.new_particle(pos, Some(cc)));
                }
            }
        }
    }

    pub fn apply_edge_forces<F>(&mut self, graph: &Graph, fixed: &F)
    where
        F: Fn(GraphId) -> bool,
    {
        self.apply_edge_attract(graph, fixed);
        self.apply_edge_repulse_edge(graph, fixed);
    }

    fn apply_edge_attract<F>(&mut self, graph: &Graph, fixed: &F)
    where
        F: Fn(GraphId) -> bool,
    {
        let c = 2.0f32;
        for src in 0..graph.nodes.len() {
            for mph in 0..graph.edges[src].len() {
                let control_id = graph.edges[src][mph].1.control.unwrap();
                let dst = graph.edges[src][mph].0;
                let src_pos = self.get_pos(graph.nodes[src].2.pos.unwrap());
                let dst_pos = self.get_pos(graph.nodes[dst].2.pos.unwrap());
                let center = src_pos + 0.5f32 * (dst_pos - src_pos);
                let pos = self.particles[control_id].pos;
                let dist = pos.distance(center);
                let f = if dist > 500f32 {
                    dist
                } else {
                    (1f32 + dist).ln()
                };
                let f = c * f * (center - pos).normalized();
                if !fixed(GraphId::Morphism(src, mph)) {
                    self.add_force(control_id, f);
                }
            }
        }
    }

    fn apply_edge_repulse_edge<F>(&mut self, graph: &Graph, fixed: &F)
    where
        F: Fn(GraphId) -> bool,
    {
        let c = 5e4f32;
        for src1 in 0..graph.nodes.len() {
            for mph1 in 0..graph.edges[src1].len() {
                let dst1 = graph.edges[src1][mph1].0;
                let c1_id = graph.edges[src1][mph1].1.control.unwrap();
                let c1 = self.particles[c1_id].pos;
                for src2 in 0..graph.nodes.len() {
                    for mph2 in 0..graph.edges[src2].len() {
                        if src1 == src2 && mph1 == mph2 {
                            continue;
                        }
                        let dst2 = graph.edges[src2][mph2].0;
                        let c2_id = graph.edges[src2][mph2].1.control.unwrap();
                        let c2 = self.particles[c2_id].pos;
                        if src1 == src2 && dst1 == dst2 {
                            // If any of the two morphism is fixed, we dont push
                            // the other away
                            if fixed(GraphId::Morphism(src1, mph1))
                                || fixed(GraphId::Morphism(src2, mph2))
                            {
                                continue;
                            }
                            // If the morphisms are parallel, only spread them
                            // along the direction of the mediatrice of the
                            // endpoints
                            let psrc = self.particles[graph.nodes[src1].2.pos.unwrap()].pos;
                            let pdst = self.particles[graph.nodes[dst1].2.pos.unwrap()].pos;
                            let center = psrc + 0.5f32 * (pdst - psrc);
                            let norm = (pdst - psrc).rot90().normalized();
                            let coeff1 = norm.dot(c1 - center);
                            let coeff2 = norm.dot(c2 - center);
                            let dist = (coeff2 - coeff1).abs();
                            if dist > 10f32 {
                                let dir = ((coeff2 - coeff1) * norm) / dist;
                                let f = (c / (dist * dist)) * dir;
                                self.add_force(c1_id, -f);
                                self.add_force(c2_id, f);
                            } else {
                                // If they are too close, we just push them appart
                                let c = 15.0f32;
                                let f = if mph1 < mph2 { -c } else { c };
                                self.add_force(c1_id, f * norm);
                                self.add_force(c2_id, -f * norm);
                            }
                        } else {
                            let dist = c1.distance_sq(c2);
                            if dist > 100f32 {
                                let dir = (c2 - c1).normalized();
                                let f = (c / dist) * dir;
                                if !fixed(GraphId::Morphism(src1, mph1)) {
                                    self.add_force(c1_id, -f);
                                }
                                if !fixed(GraphId::Morphism(src2, mph2)) {
                                    self.add_force(c2_id, f);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
