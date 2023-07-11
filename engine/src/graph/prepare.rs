use crate::graph::eq::{Eq, Morphism};
use crate::graph::{Face, FaceParsed, Graph, GraphParsed};
use crate::normalizer::morphism;
use crate::remote::TermEngine;

impl<NL, EL, FL> GraphParsed<NL, EL, FL> {
    pub fn prepare<R: TermEngine>(self, rm: &mut R) -> Graph<NL, EL, FL> {
        let nodes = self.nodes;

        // Normalize edges
        let edges = self.edges;
        let mut edge_comps: Vec<Vec<(u64, Morphism)>> = vec![Vec::new(); nodes.len()];
        let mut edge_map: Vec<Vec<Option<usize>>> = vec![Vec::new(); nodes.len()];
        for src in 0..nodes.len() {
            for mph in 0..edges[src].len() {
                let dst = edges[src][mph].0;
                let (mph, _, comps) = morphism(
                    rm,
                    nodes[src].1,
                    nodes[src].0,
                    nodes[dst].0,
                    edges[src][mph].2,
                );
                let norm = Morphism {
                    src: nodes[src].0,
                    dst: nodes[dst].0,
                    comps,
                };
                edge_comps[src].push((mph, norm));
            }
            edge_map[src] = edge_comps[src]
                .iter()
                .scan(0, |index, cmps| {
                    if cmps.1.comps.is_empty() {
                        // Identity case
                        Some(None)
                    } else {
                        *index += 1;
                        Some(Some(*index - 1))
                    }
                })
                .collect();
        }

        let faces = self
            .faces
            .into_iter()
            .map(|fce| fce.prepare(rm, &nodes, &edges, &edge_map, &edge_comps))
            .collect();

        // Remove identites from edges and build the morphism
        let edges = edges
            .into_iter()
            .enumerate()
            .map(|(src, edges)| {
                edges
                    .into_iter()
                    .enumerate()
                    .filter(|(i, _)| edge_map[src][*i].is_some())
                    .map(|(i, mph)| {
                        let (m, norm) = edge_comps[src][i].clone();
                        (mph.0, mph.1, m, norm)
                    })
                    .collect()
            })
            .collect();
        Graph {
            nodes,
            edges,
            faces,
        }
    }
}

impl<FL> FaceParsed<FL> {
    fn prepare<R: TermEngine, NL, EL>(
        self,
        _rm: &mut R,
        nodes: &[(u64, u64, NL)],
        edges: &[Vec<(usize, EL, u64, ())>],
        edge_map: &[Vec<Option<usize>>],
        edge_comps: &[Vec<(u64, Morphism)>],
    ) -> Face<FL> {
        let nxt_mph = |src: &mut usize, mph: usize| -> Option<(usize, usize)> {
            let prev = *src;
            *src = edges[*src][mph].0;
            Some((prev, mph))
        };

        let left_path: Vec<usize> = self
            .left
            .into_iter()
            .scan(self.start, nxt_mph)
            .filter_map(|(src, mph)| edge_map[src][mph])
            .collect();
        let right_path: Vec<usize> = self
            .right
            .into_iter()
            .scan(self.start, nxt_mph)
            .filter_map(|(src, mph)| edge_map[src][mph])
            .collect();

        let left_mph = Morphism {
            src: nodes[self.start].0,
            dst: nodes[self.end].0,
            comps: left_path
                .iter()
                .copied()
                .scan(self.start, nxt_mph)
                .map(|(src, mph)| edge_comps[src][mph].1.comps.clone())
                .flatten()
                .collect(),
        };
        let right_mph = Morphism {
            src: nodes[self.start].0,
            dst: nodes[self.end].0,
            comps: right_path
                .iter()
                .copied()
                .scan(self.start, nxt_mph)
                .map(|(src, mph)| edge_comps[src][mph].1.comps.clone())
                .flatten()
                .collect(),
        };
        let eq = Eq::atomic(nodes[self.start].1, left_mph, right_mph, self.eq);

        Face {
            start: self.start,
            end: self.end,
            left: left_path,
            right: right_path,
            eq,
            label: self.label,
        }
    }
}
