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
#[derive(Clone,Debug)]
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

    fn better(&self, other: &Span, _left: &Graph, _right: &Graph) -> bool {
        // TODO better heuristic
        return self.nodes.len() > other.nodes.len()
            && self.edges.iter().map(|v| v.len()).sum::<usize>()
                > other.edges.iter().map(|v| v.len()).sum::<usize>();
    }
}

//  __  __  ____ _____ ____
// |  \/  |/ ___| ____/ ___|
// | |\/| | |   |  _| \___ \
// | |  | | |___| |___ ___) |
// |_|  |_|\____|_____|____/
//
// MCES

struct MCES {
    left: Graph,
    llen: usize,
    right: Graph,
    rlen: usize,
    current_span: Span,
    best_span: Span,
    sigma: Substitution,
    lnode_inv: Vec<Option<usize>>,
    rnode_inv: Vec<Option<usize>>,
    ledge_inv: Vec<Vec<Option<usize>>>,
    redge_inv: Vec<Vec<Option<usize>>>,
}

impl MCES {
    fn new(left: Graph, right: Graph) -> Self {
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
            best_span: Span::new(),
            sigma: Substitution::new(),
            lnode_inv: vec![None; llen],
            rnode_inv: vec![None; rlen],
            ledge_inv,
            redge_inv,
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

    fn connect_nodes(&mut self) {
        for lnode in 0..self.llen {
            if self.lnode_inv[lnode].is_some() { continue; }
            for rnode in 0..self.rlen {
                if self.rnode_inv[rnode].is_some() { continue; }

                // TODO test if unifiable
                // assume yes at that point
                
                // let state = self.save();
                let nnode = self.connect(lnode, rnode);
                self.connect_edges_from(vec![ nnode ]);
                // self.restore(&state)
            }
        }
    }

    fn connect_edges_from(&mut self, mut queue : Vec<usize>) {
        while let Some(node_from) = queue.pop() {
            let (lnode,rnode) = self.current_span.nodes[node_from];
            let ledges = self.left.edges[lnode].len();
            let redges = self.right.edges[rnode].len();

            for ledge in 0..ledges {
                if self.ledge_inv[lnode][ledge].is_some() { continue; }
                for redge in 0..redges {
                    if self.redge_inv[rnode][redge].is_some() { continue; }

                    // TODO test if unifiable
                    // assume yes at that point
                    
                    let ldst = self.left.edges[lnode][ledge].0;
                    let rdst = self.right.edges[rnode][redge].0;
                    match (self.ledge_inv[lnode][ledge], self.redge_inv[rnode][redge]) {
                        // The targets are unbound, we need to create a new node, bind them and add
                        // it to the queue
                        (None,None) => {
                            // We can safely assume the destinations are unifiable since the
                            // morphisms themselves are
                            let nnode = self.connect(ldst, rdst);
                            queue.push(nnode)
                        }
                        // The targets are already bound. If it to the same object, we can
                        // continue, otherwise this edge binding is impossible
                        (Some(nl), Some(nr)) => if nl != nr { continue },
                        // If only one is bound, we cannot bind the other to the same since we ask
                        // the binding to be bijective, so this edge binding is impossible
                        _ => continue,
                    }

                    // let state = self.save();
                    let nedge = self.current_span.edges[node_from].len();
                    self.current_span.edges[node_from].push((ledge,redge));
                    self.ledge_inv[lnode][ledge] = Some(nedge);
                    self.redge_inv[rnode][redge] = Some(nedge);
                    break;
                    // self.restore(&state);
                }
            }
        }
    }

    fn next(&mut self) -> (Span,Substitution) {
        // TODO restart for next branch
        // {
        self.connect_nodes();
        if self.current_span.better(&self.best_span, &self.left, &self.right) {
            self.best_span = self.current_span.clone();
            // TODO compute substitution for this solution
        }
        // }

        (self.best_span.clone(), self.sigma.clone())
    }
}

pub fn mces(g1: Graph, g2: Graph) -> (Span,Substitution) {
    let mut mces = MCES::new(g1,g2);
    mces.next()
}
