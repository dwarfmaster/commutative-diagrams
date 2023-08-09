use crate::vm::Graph;
use std::collections::HashSet;

#[derive(Clone, Debug)]
pub struct GraphStructure {
    pub ccs: Vec<HashSet<usize>>,        // List of connected components
    pub nodes_component: Vec<usize>,     // For each node, its connected component
    pub neighbours: Vec<HashSet<usize>>, // For each node, its immediate neighbours
}

impl GraphStructure {
    pub fn new() -> Self {
        Self {
            ccs: Vec::new(),
            nodes_component: Vec::new(),
            neighbours: Vec::new(),
        }
    }

    pub fn from_graph(graph: &Graph) -> Self {
        // Union find on connected components
        let mut ccs: Vec<(usize, usize)> = Vec::new();
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

        // DFS to find the connected component. Since the graph is directed, we
        // may encounter already defined components, in which case we merge them
        // together. Use this exploration to populate neighboors.
        let mut nodes_cc: Vec<Option<usize>> = graph.nodes.iter().map(|_| None).collect();
        let mut neighbours: Vec<HashSet<usize>> =
            graph.nodes.iter().map(|_| HashSet::new()).collect();
        fn explore(
            graph: &Graph,
            nodes_cc: &mut Vec<Option<usize>>,
            ccs: &mut Vec<(usize, usize)>,
            neighbours: &mut Vec<HashSet<usize>>,
            nd: usize,
            cc: usize,
        ) {
            if let Some(ncc) = nodes_cc[nd] {
                connect(ccs, cc, ncc);
            } else {
                nodes_cc[nd] = Some(cc);
                for mph in 0..graph.edges[nd].len() {
                    if graph.edges[nd][mph].1.hidden {
                        continue;
                    }
                    let dst = graph.edges[nd][mph].0;
                    if dst != nd {
                        neighbours[nd].insert(dst);
                        neighbours[dst].insert(nd);
                    }
                    explore(graph, nodes_cc, ccs, neighbours, dst, cc);
                }
            }
        }
        for nd in 0..graph.nodes.len() {
            if nodes_cc[nd].is_some() || graph.nodes[nd].2.hidden {
                continue;
            }
            let cc = ccs.len();
            ccs.push((cc, 0));
            explore(graph, &mut nodes_cc, &mut ccs, &mut neighbours, nd, cc);
        }

        // Rework structures to make them more useful to the layouting algorithm
        let mut final_ccs: Vec<HashSet<usize>> = Vec::new();
        let mut final_nodes_cc: Vec<usize> = graph.nodes.iter().map(|_| 0).collect();
        let mut ccs_mapping: Vec<Option<usize>> = ccs.iter().map(|_| None).collect();
        for nd in 0..graph.nodes.len() {
            if graph.nodes[nd].2.hidden {
                continue;
            }

            let cc = parent(&mut ccs, nodes_cc[nd].unwrap());
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
        }
    }
}
