use crate::vm::Graph;
use std::collections::HashSet;

#[derive(Clone, Debug)]
pub struct GraphStructure {
    pub ccs: Vec<HashSet<usize>>,        // List of connected components
    pub nodes_component: Vec<usize>,     // For each node, its connected component
    pub neighbours: Vec<HashSet<usize>>, // For each node, its immediate neighbours
    pub distances: Vec<Vec<Option<u64>>>, // A matrix of nodes, distances[i][j] gives the lentgth
                                         // of the shortest path from i to j
}

// min and plus with None being +infinity
fn omin(o1: Option<u64>, o2: Option<u64>) -> Option<u64> {
    match (o1, o2) {
        (Some(n1), Some(n2)) => Some(n1.min(n2)),
        (None, _) => o2,
        _ => o1,
    }
}

fn oplus(o1: Option<u64>, o2: Option<u64>) -> Option<u64> {
    match (o1, o2) {
        (Some(n1), Some(n2)) => Some(n1.saturating_add(n2)),
        _ => None,
    }
}

impl GraphStructure {
    pub fn new() -> Self {
        Self {
            ccs: Vec::new(),
            nodes_component: Vec::new(),
            neighbours: Vec::new(),
            distances: Vec::new(),
        }
    }

    pub fn from_graph(graph: &Graph) -> Self {
        // Floyd warshall algorithm
        let mut distances: Vec<Vec<Option<u64>>> = graph
            .nodes
            .iter()
            .map(|_| graph.nodes.iter().map(|_| None).collect())
            .collect();
        let mut neighbours: Vec<HashSet<usize>> =
            graph.nodes.iter().map(|_| HashSet::new()).collect();
        // Init the distances. Use this loop to also fill the neighbours information
        for src in 0..graph.nodes.len() {
            if graph.nodes[src].2.hidden {
                continue;
            }
            for mph in 0..graph.edges[src].len() {
                if graph.edges[src][mph].1.hidden {
                    continue;
                }
                let dst = graph.edges[src][mph].0;
                distances[src][dst] = Some(1);
                distances[dst][src] = Some(1);
                neighbours[src].insert(dst);
                neighbours[dst].insert(src);
            }
        }
        // Iterate
        {
            let mut tmp = distances.clone();
            let current = &mut distances;
            let previous = &mut tmp;
            for k in 0..graph.nodes.len() {
                std::mem::swap(current, previous);
                for i in 0..graph.nodes.len() {
                    // Not necessary, but avoid unnecessary loop
                    if graph.nodes[i].2.hidden {
                        continue;
                    }
                    for j in 0..graph.nodes.len() {
                        current[i][j] = omin(previous[i][j], oplus(previous[i][k], previous[k][j]));
                    }
                }
            }
            if graph.nodes.len() % 2 == 1 {
                // previous actually points to distances
                for i in 0..graph.nodes.len() {
                    for j in 0..graph.nodes.len() {
                        previous[i][j] = current[i][j];
                    }
                }
            }
        }

        // Union find on nodes
        let mut nodes_cc: Vec<(usize, usize)> = (0..graph.nodes.len()).map(|i| (i, 0)).collect();
        fn parent(ccs: &mut Vec<(usize, usize)>, cc: usize) -> usize {
            if ccs[cc].0 != cc {
                ccs[cc].0 = parent(ccs, ccs[cc].0)
            }
            ccs[cc].0
        }
        fn connect(ccs: &mut Vec<(usize, usize)>, cc1: usize, cc2: usize) {
            let p1 = parent(ccs, cc1);
            let p2 = parent(ccs, cc2);
            if p1 == p2 {
                return;
            }
            if ccs[p1].1 < ccs[p2].1 {
                ccs[p1].0 = p2;
            } else {
                ccs[p2].0 = p1;
                if ccs[p1].1 == ccs[p2].1 {
                    ccs[p1].1 += 1;
                }
            }
        }

        // Compute the connected components
        for i in 0..graph.nodes.len() {
            for j in (i + 1)..graph.nodes.len() {
                if distances[i][j].is_some() {
                    connect(&mut nodes_cc, i, j);
                }
            }
        }

        // Rework structures to make them more useful to the layouting algorithm
        let mut final_ccs: Vec<HashSet<usize>> = Vec::new();
        let mut final_nodes_cc: Vec<usize> = graph.nodes.iter().map(|_| 0).collect();
        let mut ccs_mapping: Vec<Option<usize>> = graph.nodes.iter().map(|_| None).collect();
        for nd in 0..graph.nodes.len() {
            if graph.nodes[nd].2.hidden {
                continue;
            }

            let cc = parent(&mut nodes_cc, nd);
            let final_cc = if let Some(final_cc) = ccs_mapping[cc] {
                final_cc
            } else {
                let final_cc = final_ccs.len();
                final_ccs.push(HashSet::new());
                ccs_mapping[cc] = Some(final_cc);
                final_cc
            };

            final_nodes_cc[nd] = final_cc;
            final_ccs[final_cc].insert(nd);
        }

        Self {
            ccs: final_ccs,
            nodes_component: final_nodes_cc,
            neighbours,
            distances,
        }
    }
}
