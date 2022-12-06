use crate::anyterm::{AnyTerm, IsTerm};
use crate::data::Context;
use crate::graph::Graph;
use crate::substitution::Substitution;
use crate::unification::UnifState;
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
    pub nodes: Vec<(usize, usize)>,
    pub edges: Vec<Vec<(usize, usize)>>,
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

pub struct MCES<'a> {
    // Immutable data during the algorithm
    left: &'a Graph,
    llen: usize,
    right: &'a Graph,
    rlen: usize,
    // Exploration data
    current_span: Span,
    lnode_inv: Vec<Option<usize>>,
    rnode_inv: Vec<Option<usize>>,
    ledge_inv: Vec<Vec<Option<usize>>>,
    redge_inv: Vec<Vec<Option<usize>>>,
    // Exploration stack
    stack: Vec<(MCESState, SpanState)>,
    unif: UnifState,
    unif_goals: Vec<(AnyTerm, AnyTerm)>,
}

enum MCESState {
    Node {
        lnode: usize,
        rnode: usize,
        unif: usize,
    },
    Edge {
        queue: Vec<usize>,
        from: usize,
        ledge: usize,
        redge: usize,
        unif: usize,
    },
}

impl<'a> MCES<'a> {
    pub fn new(ctx: &mut Context, left: &'a Graph, right: &'a Graph) -> Self {
        let first_state = MCESState::Node {
            lnode: 0,
            rnode: 0,
            unif: 0,
        };
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
        let mut unif = UnifState::new();
        left.nodes
            .iter()
            .for_each(|n| unif.add(&ctx, n.clone().term()));
        right
            .nodes
            .iter()
            .for_each(|n| unif.add(&ctx, n.clone().term()));
        left.edges
            .iter()
            .flatten()
            .for_each(|(_, e)| unif.add(&ctx, e.clone().term()));
        right
            .edges
            .iter()
            .flatten()
            .for_each(|(_, e)| unif.add(&ctx, e.clone().term()));
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
            unif,
            unif_goals: Vec::new(),
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
        self.stack.push((
            MCESState::Node {
                lnode,
                rnode,
                unif: self.unif_goals.len(),
            },
            state,
        ))
    }

    fn try_connect_nodes(&mut self, lnode: usize, rnode: usize) -> bool {
        // The same test for lnode is unnecessary
        if self.rnode_inv[rnode].is_some() {
            return false;
        }

        let lobj = self.left.nodes[lnode].clone().term();
        let robj = self.right.nodes[rnode].clone().term();
        self.unif.add_goal(lobj.clone(), robj.clone());
        if self.unif.solve().is_none() {
            self.unif.rm_goal(lobj, robj);
            return false;
        }

        self.push_node(lnode, rnode);
        self.unif_goals.push((lobj, robj));
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
            if let Some(nfrom) = queue.pop() {
                from = nfrom;
                ledge = 0;
            }
        }
        self.stack.push((
            MCESState::Edge {
                queue,
                from,
                ledge,
                redge,
                unif: self.unif_goals.len(),
            },
            state,
        ))
    }

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

        let lobj = self.left.edges[lnode][ledge].1.clone().term();
        let robj = self.right.edges[rnode][redge].1.clone().term();
        self.unif.add_goal(lobj.clone(), robj.clone());
        if self.unif.solve().is_none() {
            self.unif.rm_goal(lobj.clone(), robj.clone());
            return false;
        }

        let ldst = self.left.edges[lnode][ledge].0;
        let rdst = self.right.edges[rnode][redge].0;
        match (self.lnode_inv[ldst], self.rnode_inv[rdst]) {
            // The targets are unbound, we need to create a new node, bind them and add
            // it to the queue
            (None, None) => {
                // We can safely assume the destinations are unifiable since the
                // morphisms themselves are
                // We still add them to the unification algorithm in case they can constraint more
                // things
                self.push_edge(queue.clone(), from, ledge, redge);
                self.unif_goals.push((lobj, robj));
                // TODO
                let nnode = self.connect(ldst, rdst);
                queue.push(nnode)
            }
            // The targets are already bound. If it to the same object, we can
            // continue, otherwise this edge binding is impossible
            (Some(nl), Some(nr)) => {
                if nl != nr {
                    self.unif.rm_goal(lobj.clone(), robj.clone());
                    return false;
                } else {
                    self.push_edge(queue.clone(), from, ledge, redge);
                    self.unif_goals.push((lobj, robj));
                }
            }
            // If only one is bound, we cannot bind the other to the same since we ask
            // the binding to be bijective, so this edge binding is impossible
            _ => {
                self.unif.rm_goal(lobj.clone(), robj.clone());
                return false;
            }
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
            if lstart < ledges && self.ledge_inv[lnode][lstart].is_none() {
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
                    if self.try_connect_edges(&mut queue, from, lnode, ledge, rnode, redge) {
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

    fn restore_unif(&mut self, unif: usize) {
        self.unif_goals[unif..]
            .iter()
            .for_each(|(t1, t2)| self.unif.rm_goal(t1.clone(), t2.clone()));
        self.unif_goals.truncate(unif)
    }

    fn restart(&mut self) -> Option<(Span, Substitution)> {
        if let Some(state) = self.stack.pop() {
            use MCESState::*;
            self.restore(&state.1);
            match state.0 {
                Node { lnode, rnode, unif } => {
                    self.restore_unif(unif);
                    self.restart_node(lnode, rnode)
                }
                Edge {
                    queue,
                    from,
                    ledge,
                    redge,
                    unif,
                } => {
                    self.restore_unif(unif);
                    self.restart_edges(queue, from, ledge, redge)
                }
            }
            let sigma = self
                .unif
                .solve()
                .ok_or("Unification should have succeeded at that point")
                .unwrap();
            Some((self.current_span.clone(), sigma))
        } else {
            None
        }
    }
}

impl<'a> Iterator for MCES<'a> {
    type Item = (Span, Substitution);

    fn next(&mut self) -> Option<Self::Item> {
        self.restart()
    }
}

pub fn mces(ctx: &mut Context, g1: &Graph, g2: &Graph) -> Vec<(Span, Substitution)> {
    let mces = MCES::new(ctx, g1, g2);
    mces.collect()
}

#[cfg(test)]
mod tests {
    use crate::data::Context;
    use crate::data::ProofObject;
    use crate::data::{ActualCategory, ActualMorphism, ActualObject};
    use crate::data::{CategoryData, MorphismData, ObjectData};
    use crate::dsl::{cat, mph, obj};
    use crate::graph::*;

    #[test]
    fn empty_graphs() {
        let mut ctx = Context::new();
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
        let nb = mces::mces(&mut ctx, &g1, &g2).len();
        assert_eq!(
            nb, 1,
            "Only one matching should be found between empty graphs"
        );
    }

    #[test]
    fn nodes_only() {
        let mut ctx = Context::new();
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
        let sols: Vec<Vec<(usize, usize)>> = mces::MCES::new(&mut ctx, &g1, &g2)
            .map(|(sp, _)| sp.nodes)
            .collect();
        let nb = sols.len();
        assert_eq!(
            nb, 21,
            "21 possible matching between 2 node graph and 4 node graph"
        );
    }

    #[test]
    fn test_unification() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let a = obj!(ctx, (:1) in cat);
        let b = obj!(ctx, (:2) in cat);
        let m = mph!(ctx, (?0) : a -> b);
        let x = obj!(ctx, (?1) in cat);
        let y = obj!(ctx, (?2) in cat);
        let f = mph!(ctx, (:3) : x -> y);

        let g1: Graph = Graph {
            nodes: vec![a, b],
            edges: vec![vec![(1, m)], Vec::new()],
            faces: Vec::new(),
        };
        let g2: Graph = Graph {
            nodes: vec![x, y],
            edges: vec![vec![(1, f)], Vec::new()],
            faces: Vec::new(),
        };
        let sols: Vec<_> = mces::MCES::new(&mut ctx, &g1, &g2).collect();
        assert_eq!(sols.len(), 7, "Unexpected matchings");
        assert_eq!(
            sols.iter().map(|(_, sigma)| sigma.len()).max(),
            Some(3),
            "Should have found a maximum solution"
        );
    }
}
