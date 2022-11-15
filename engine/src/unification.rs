use crate::anyterm::AnyTerm;
use crate::anyterm::IsTerm;
use crate::substitution::Substitution;
use std::collections::HashMap;
use std::vec::Vec;

#[derive(PartialEq, Eq)]
enum NodeStatus {
    Deleted,
    Variable(u64),
    Live,
}

struct Node {
    status: NodeStatus,
    ancestors: Vec<usize>,
    descendants: Vec<usize>,
    siblings: Vec<usize>,
    pointer: Option<usize>,
    value: AnyTerm,
}

impl NodeStatus {
    #[inline]
    fn is_live(&self) -> bool {
        *self == NodeStatus::Live
    }

    #[inline]
    fn is_var(&self) -> Option<u64> {
        match self {
            NodeStatus::Variable(e) => Some(*e),
            _ => None,
        }
    }

    #[inline]
    fn is_deleted(&self) -> bool {
        *self == NodeStatus::Deleted
    }
}

struct Graph {
    nodes: Vec<Node>,
    mapping: HashMap<AnyTerm, usize>,
}

impl Node {
    fn new(gr: &Graph, term: AnyTerm) -> Node {
        use NodeStatus::*;
        Node {
            status: term.as_var().map(|e| Variable(e)).unwrap_or(Live),
            ancestors: Vec::new(),
            descendants: term
                .clone()
                .subterms()
                .map(|sub| gr.get(sub, "Children must be added to the graph first (0)"))
                .collect(),
            siblings: Vec::new(),
            pointer: None,
            value: term,
        }
    }
}

impl Graph {
    fn new() -> Graph {
        Graph {
            nodes: Vec::new(),
            mapping: HashMap::new(),
        }
    }

    fn get(&self, term: AnyTerm, msg: &str) -> usize {
        *self.mapping.get(&term).expect(msg)
    }

    fn add_ancestor(&mut self, term: AnyTerm, ancestor: usize) {
        let id = self.get(term, "Children must be added to the graph first (1)");
        self.nodes[id].ancestors.push(ancestor)
    }

    /// Insert a new node and takes cares of updating the ancestors of all its descendents Assumes
    /// term subterms are already present in the graph
    fn insert_node(&mut self, term: AnyTerm) -> usize {
        let id = self.nodes.len();
        self.mapping.insert(term.clone(), id);
        let node = Node::new(self, term.clone());
        self.nodes.push(node);
        term.subterms().for_each(|sub| self.add_ancestor(sub, id));
        id
    }

    /// Insert a node only if it wasn't present on the graph
    #[inline]
    fn may_insert(&mut self, term: AnyTerm) -> usize {
        let pos = self.mapping.get(&term);
        match pos {
            Some(id) => *id,
            None => self.insert_node(term),
        }
    }

    /// Insert a term and all its subterms
    fn insert(&mut self, term: AnyTerm) -> usize {
        term.clone().subterms().for_each(|sub| {
            self.insert(sub);
        });
        self.may_insert(term)
    }

    /// Connect two terms with an undirected edge
    fn connect(&mut self, t1: usize, t2: usize) {
        self.nodes[t1].siblings.push(t2);
        self.nodes[t2].siblings.push(t1);
    }
}

fn finish(mut gr: &mut Graph, mut sigma: &mut Substitution, r: usize) -> bool {
    if gr.nodes[r].status.is_deleted() {
        true
    } else if gr.nodes[r].pointer.is_some() {
        false
    } else {
        gr.nodes[r].pointer = Some(r);
        let mut stack: Vec<usize> = Vec::new();
        stack.push(r);
        while let Some(s) = stack.pop() {
            // TODO check if r and s have compatible function symbols

            {
                // I don't think I can avoid this clone without a lot refactoring to separate
                // dynamic from static data in Graph
                let ancestors = gr.nodes[s].ancestors.clone();
                if !ancestors.iter().all(|a| finish(&mut gr, &mut sigma, *a)) {
                    return false;
                }
            }

            // Process siblings
            while let Some(t) = gr.nodes[s].siblings.pop() {
                let ptr = gr.nodes[t].pointer.get_or_insert(r);
                if *ptr != r {
                    return false;
                };
                stack.push(t)
            }

            // Propagate siblings or extend substitution
            if s != r {
                if let Some(s) = gr.nodes[s].status.is_var() {
                    sigma.push((s, gr.nodes[r].value.clone()))
                }
                if gr.nodes[s].status.is_live() {
                    let descendants = gr.nodes[s].descendants.len();
                    assert_eq!(
                        descendants,
                        gr.nodes[r].descendants.len(),
                        "Same symbols should have same number of descendants"
                    );
                    for id in 0..descendants {
                        gr.connect(gr.nodes[s].descendants[id], gr.nodes[r].descendants[id])
                    }
                }
                gr.nodes[s].status = NodeStatus::Deleted;
            }
        }
        gr.nodes[r].status = NodeStatus::Deleted;
        true
    }
}

/// Find the MGU of two terms in linear time using the algorithm C described in the paper:
///     Paterson, M.S., and M.N. Wegman. “Linear Unification.”
///     Journal of Computer and System Sciences 16, no. 2 (April 1978): 158–67.
///     https://doi.org/10.1016/0022-0000(78)90043-0.
pub fn unify(t1: AnyTerm, t2: AnyTerm) -> Option<Substitution> {
    // Build the DAG of subterms of the two terms to unify
    let mut gr = Graph::new();
    let id1 = gr.insert(t1);
    let id2 = gr.insert(t2);
    gr.connect(id1, id2);
    let size = gr.nodes.len();

    // Prepare the substitution
    let mut sigma = Substitution::new();

    // Iterate over all nodes
    for id in 0..size {
        if !gr.nodes[id].status.is_live() {
            continue;
        }
        if !finish(&mut gr, &mut sigma, id) {
            return None;
        }
    }

    // Iterate over all variables
    for id in 0..size {
        if !gr.nodes[id].status.is_var().is_some() {
            continue;
        }
        if !finish(&mut gr, &mut sigma, id) {
            return None;
        }
    }

    // If we reached this point we've succeeded
    Some(sigma)
}

#[cfg(test)]
mod tests {
    use crate::anyterm::AnyTerm;
    use crate::data::{ActualCategory, ActualMorphism, ActualObject};
    use crate::data::{CategoryData, MorphismData, ObjectData};
    use crate::data::{Context, ProofObject};
    use crate::dsl::{cat, mph, obj};
    use crate::substitution::Substitutable;
    use crate::unification::unify;

    #[test]
    pub fn simple() {
        // This is the same example as the paper, encoded into our theory with
        //     x1, x2, x3 : cat(o,o)
        //     G(x) : 1_o o x
        //     F(x,y) : y o x
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let o = obj!(ctx, (:1) in cat);
        let x1 = mph!(ctx, (?0) : o -> o);
        let x2 = mph!(ctx, (?1) : o -> o);
        let x3 = mph!(ctx, (?2) : o -> o);
        let m1 = mph!(ctx, (x2 >> (id o)) >> (x3 >> (id o)));
        let m2 = mph!(ctx, x1 >> x2);
        let sigma = unify(AnyTerm::Mph(m1.clone()), AnyTerm::Mph(m2.clone()))
            .ok_or("Couldn't unify m1 and m2")
            .unwrap();
        let m1 = m1.subst(&mut ctx, &sigma);
        let m2 = m2.subst(&mut ctx, &sigma);
        assert_eq!(m1, m2, "Unifier is wrong");
    }
}
