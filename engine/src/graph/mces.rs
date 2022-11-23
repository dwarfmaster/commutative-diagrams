use crate::graph::Graph;
use crate::substitution::Substitution;
use std::vec::Vec;

//  ____
// / ___| _ __   __ _ _ __
// \___ \| '_ \ / _` | '_ \
//  ___) | |_) | (_| | | | |
// |____/| .__/ \__,_|_| |_|
//       |_|
// Span

/// A structure that identifies a common subgraph of two graphs
#[derive(Clone, Debug)]
pub struct Span {
    nodes: Vec<(usize, usize)>,
    edges: Vec<Vec<(usize, usize)>>,
    // TODO faces
}

struct SpanState {
    node_size: usize,
    edge_size: Vec<usize>,
}

impl Span {
    fn new() -> Self {
        Span {
            nodes: Vec::new(),
            edges: Vec::new(),
        }
    }

    fn save(&self) -> SpanState {
        SpanState {
            node_size: self.nodes.len(),
            edge_size: self.edges.iter().map(|v| v.len()).collect(),
        }
    }

    fn restore(&mut self, state: &SpanState) {
        assert!(
            state.node_size <= self.nodes.len(),
            "Restore point is invalid: too many nodes"
        );
        self.nodes.truncate(state.node_size);
        self.edges.truncate(state.node_size);
        self.edges
            .iter_mut()
            .zip(state.edge_size.iter())
            .for_each(|(v, i)| v.truncate(*i));
    }

    fn connect(&mut self, left: usize, right: usize) -> usize {
        let ret = self.nodes.len();
        self.nodes.push((left, right));
        self.edges.push(Vec::new());
        ret
    }
}

//  __  __  ____ _____ ____
// |  \/  |/ ___| ____/ ___|
// | |\/| | |   |  _| \___ \
// | |  | | |___| |___ ___) |
// |_|  |_|\____|_____|____/
//
// MCES

pub struct MCES {
    // Immutable data during the algorithm
    left: Graph,
    llen: usize,
    right: Graph,
    rlen: usize,
    // Exploration data
    current_span: Span,
    lnode_inv: Vec<Option<usize>>,
    rnode_inv: Vec<Option<usize>>,
    ledge_inv: Vec<Vec<Option<usize>>>,
    redge_inv: Vec<Vec<Option<usize>>>,
    // Exploration stack
    stack: Vec<(MCESState, SpanState)>,
}

enum MCESState {
    Node {
        lnode: usize,
        rnode: usize,
    },
    Edge {
        queue: Vec<usize>,
        from: usize,
        ledge: usize,
        redge: usize,
    },
}

impl MCES {
    pub fn new(left: Graph, right: Graph) -> Self {
        let first_state = MCESState::Node { lnode: 0, rnode: 0 };
        let empty_span_state = SpanState {
            node_size: 0,
            edge_size: Vec::new(),
        };
        let llen = left.nodes.len();
        let rlen = right.nodes.len();
        let ledge_inv = left
            .edges
            .iter()
            .map(|edges| vec![None; edges.len()])
            .collect();
        let redge_inv = right
            .edges
            .iter()
            .map(|edges| vec![None; edges.len()])
            .collect();
        MCES {
            left,
            right,
            llen,
            rlen,
            current_span: Span::new(),
            lnode_inv: vec![None; llen],
            rnode_inv: vec![None; rlen],
            ledge_inv,
            redge_inv,
            stack: vec![(first_state, empty_span_state)],
        }
    }

    fn save(&self) -> SpanState {
        self.current_span.save()
    }

    fn restore(&mut self, state: &SpanState) {
        self.current_span
            .nodes
            .iter()
            .skip(state.node_size)
            .for_each(|(l, r)| {
                self.lnode_inv[*l] = None;
                self.rnode_inv[*r] = None;
            });
        self.current_span
            .edges
            .iter()
            .enumerate()
            .for_each(|(i, v)| {
                let skp = if i < state.node_size {
                    state.edge_size[i]
                } else {
                    0
                };
                v.iter().skip(skp).for_each(|(l, r)| {
                    self.ledge_inv[i][*l] = None;
                    self.redge_inv[i][*r] = None;
                })
            });
        self.current_span.restore(state)
    }

    fn connect(&mut self, lnode: usize, rnode: usize) -> usize {
        let nnode = self.current_span.connect(lnode, rnode);
        self.lnode_inv[lnode] = Some(nnode);
        self.rnode_inv[rnode] = Some(nnode);
        nnode
    }

    fn push_node(&mut self, mut lnode: usize, mut rnode: usize) {
        let state = self.save();
        rnode += 1;
        if rnode >= self.rlen {
            lnode += 1;
            rnode = 0;
        }
        if lnode < self.llen {
            self.stack.push((MCESState::Node { lnode, rnode }, state))
        }
    }

    #[inline]
    fn try_connect_nodes(&mut self, lnode: usize, rnode: usize) -> bool {
        // The same test for lnode is unnecessary
        if self.rnode_inv[rnode].is_some() {
            return false;
        }

        // TODO test if unifiable
        // assume yes at that point

        self.push_node(lnode, rnode);
        let nnode = self.connect(lnode, rnode);
        self.restart_edges(Vec::new(), nnode, 0, 0);
        true
    }

    fn restart_node(&mut self, lstart: usize, rstart: usize) {
        if lstart < self.lnode_inv.len() && self.lnode_inv[lstart].is_none() {
            for rnode in rstart..self.rlen {
                if self.try_connect_nodes(lstart, rnode) {
                    break;
                }
            }
        }

        for lnode in (lstart + 1)..self.llen {
            if self.lnode_inv[lnode].is_some() {
                continue;
            }
            for rnode in 0..self.rlen {
                if self.try_connect_nodes(lnode, rnode) {
                    break;
                }
            }
        }
    }

    fn push_edge(
        &mut self,
        mut queue: Vec<usize>,
        mut from: usize,
        mut ledge: usize,
        mut redge: usize,
    ) {
        let state = self.save();
        let (lnode, rnode) = self.current_span.nodes[from];
        redge += 1;
        if redge >= self.right.edges[rnode].len() {
            ledge += 1;
            redge = 0;
        }
        if ledge >= self.left.edges[lnode].len() {
            ledge = 0;
            if let Some(nfrom) = queue.pop() {
                from = nfrom
            } else {
                return;
            }
        }
        self.stack.push((
            MCESState::Edge {
                queue,
                from,
                ledge,
                redge,
            },
            state,
        ))
    }

    #[inline]
    fn try_connect_edges(
        &mut self,
        queue: &mut Vec<usize>,
        from: usize,
        lnode: usize,
        ledge: usize,
        rnode: usize,
        redge: usize,
    ) -> bool {
        // No need to check the same condition of ledge
        if self.redge_inv[rnode][redge].is_some() {
            return false;
        }

        // TODO test if unifiable
        // assume yes at that point

        let ldst = self.left.edges[lnode][ledge].0;
        let rdst = self.right.edges[rnode][redge].0;
        match (self.ledge_inv[lnode][ledge], self.redge_inv[rnode][redge]) {
            // The targets are unbound, we need to create a new node, bind them and add
            // it to the queue
            (None, None) => {
                // We can safely assume the destinations are unifiable since the
                // morphisms themselves are
                self.push_edge(queue.clone(), from, lnode, rnode);
                let nnode = self.connect(ldst, rdst);
                queue.push(nnode)
            }
            // The targets are already bound. If it to the same object, we can
            // continue, otherwise this edge binding is impossible
            (Some(nl), Some(nr)) => {
                if nl != nr {
                    return false;
                }
            }
            // If only one is bound, we cannot bind the other to the same since we ask
            // the binding to be bijective, so this edge binding is impossible
            _ => return false,
        }

        let nedge = self.current_span.edges[from].len();
        self.current_span.edges[from].push((ledge, redge));
        self.ledge_inv[lnode][ledge] = Some(nedge);
        self.redge_inv[rnode][redge] = Some(nedge);
        true
    }

    fn restart_edges(&mut self, mut queue: Vec<usize>, from: usize, lstart: usize, rstart: usize) {
        {
            let (lnode, rnode) = self.current_span.nodes[from];
            let ledges = self.left.edges[lnode].len();
            let redges = self.right.edges[rnode].len();
            if lstart < self.ledge_inv[lnode].len() && self.ledge_inv[lnode][lstart].is_none() {
                for redge in rstart..redges {
                    if self.try_connect_edges(&mut queue, from, lnode, lstart, rnode, redge) {
                        break;
                    }
                }
            }
            for ledge in (lstart + 1)..ledges {
                if self.ledge_inv[lnode][ledge].is_some() {
                    continue;
                }
                for redge in 0..redges {
                    if self.try_connect_edges(&mut queue, from, lnode, lstart, rnode, redge) {
                        break;
                    }
                }
            }
        }

        while let Some(node_from) = queue.pop() {
            let (lnode, rnode) = self.current_span.nodes[node_from];
            let ledges = self.left.edges[lnode].len();
            let redges = self.right.edges[rnode].len();

            for ledge in 0..ledges {
                if self.ledge_inv[lnode][ledge].is_some() {
                    continue;
                }
                for redge in 0..redges {
                    if self.try_connect_edges(&mut queue, node_from, lnode, ledge, rnode, redge) {
                        break;
                    }
                }
            }
        }
    }

    fn restart(&mut self) -> Option<(Span, Substitution)> {
        if let Some(state) = self.stack.pop() {
            use MCESState::*;
            self.restore(&state.1);
            match state.0 {
                Node { lnode, rnode } => self.restart_node(lnode, rnode),
                Edge {
                    queue,
                    from,
                    ledge,
                    redge,
                } => self.restart_edges(queue, from, ledge, redge),
            }
            // TODO compute substitution
            let sigma = Substitution::new();
            Some((self.current_span.clone(), sigma))
        } else {
            None
        }
    }
}

impl Iterator for MCES {
    type Item = (Span, Substitution);

    fn next(&mut self) -> Option<Self::Item> {
        self.restart()
    }
}

pub fn mces(g1: Graph, g2: Graph) -> Vec<(Span, Substitution)> {
    let mces = MCES::new(g1, g2);
    mces.collect()
}

#[cfg(test)]
mod tests {
    use crate::data::Context;
    use crate::data::ProofObject;
    use crate::data::{ActualCategory, ActualObject};
    use crate::data::{CategoryData, ObjectData};
    use crate::dsl::{cat, obj};
    use crate::graph::*;

    #[test]
    fn empty_graphs() {
        let g1 = Graph {
            nodes: Vec::new(),
            edges: Vec::new(),
            faces: Vec::new(),
        };
        let g2 = Graph {
            nodes: Vec::new(),
            edges: Vec::new(),
            faces: Vec::new(),
        };
        let nb = mces::mces(g1, g2).len();
        assert_eq!(
            nb, 1,
            "Only one matching should be found between empty graphs"
        );
    }

    #[test]
    fn nodes_only() {
        let ctx = Context::new();
        let cat = cat!(ctx, :0);
        let a = obj!(ctx, (:1) in cat);
        let b = obj!(ctx, (:2) in cat);
        let c = obj!(ctx, (:3) in cat);
        let d = obj!(ctx, (:4) in cat);
        let x = obj!(ctx, (?0) in cat);
        let y = obj!(ctx, (?1) in cat);

        let g1: Graph = Graph {
            nodes: vec![a, b, c, d],
            edges: vec![Vec::new(); 4],
            faces: Vec::new(),
        };
        let g2: Graph = Graph {
            nodes: vec![x, y],
            edges: vec![Vec::new(); 2],
            faces: Vec::new(),
        };
        let sols : Vec<Vec<(usize,usize)>> = mces::MCES::new(g1, g2).map(|(sp,_)| sp.nodes).collect();
        // println!("Solution: {:#?}", sols);
        let nb = sols.len();
        // let nb = mces::mces(g1, g2).len();
        assert_eq!(
            nb, 17,
            "17? possible matching between 2 node graph and 4 node graph"
        );
    }
}
