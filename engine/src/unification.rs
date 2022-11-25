use crate::anyterm::AnyTerm;
use crate::anyterm::IsTerm;
use crate::data::Context;
use crate::substitution::Substitution;
use std::collections::HashMap;
use std::vec::Vec;

#[derive(Clone, Copy, PartialEq, Eq)]
enum NodeStatus {
    Deleted,
    Variable(u64),
    Live,
}

struct Node {
    init_status: NodeStatus,
    status: NodeStatus,
    ancestors: Vec<usize>,
    descendants: Vec<usize>,
    siblings: Vec<usize>,
    initial_siblings: usize,
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
    fn new(gr: &Graph, ctx: &Context, term: AnyTerm) -> Node {
        use NodeStatus::*;
        let status = term.as_var().map(|e| Variable(e)).unwrap_or(Live);
        Node {
            init_status: status,
            status,
            ancestors: Vec::new(),
            descendants: term
                .clone()
                .subterms(ctx.clone())
                .map(|sub| gr.get(sub, "Children must be added to the graph first (0)"))
                .collect(),
            siblings: Vec::new(),
            initial_siblings: 0,
            pointer: None,
            value: term,
        }
    }

    fn reset(&mut self) {
        self.status = self.init_status;
        self.pointer = None;
        self.siblings.truncate(self.initial_siblings);
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
    fn insert_node(&mut self, ctx: &Context, term: AnyTerm) -> usize {
        let id = self.nodes.len();
        self.mapping.insert(term.clone(), id);
        let node = Node::new(self, ctx, term.clone());
        self.nodes.push(node);
        term.subterms(ctx.clone())
            .for_each(|sub| self.add_ancestor(sub, id));
        id
    }

    /// Insert a node only if it wasn't present on the graph
    #[inline]
    fn may_insert(&mut self, ctx: &Context, term: AnyTerm) -> usize {
        let pos = self.mapping.get(&term);
        match pos {
            Some(id) => *id,
            None => self.insert_node(ctx, term),
        }
    }

    /// Insert a term and all its subterms
    fn insert(&mut self, ctx: &Context, term: AnyTerm) -> usize {
        term.clone().subterms(ctx.clone()).for_each(|sub| {
            self.insert(ctx, sub);
        });
        self.may_insert(ctx, term)
    }

    /// Connect two terms with an undirected edge
    /// The graph must be in initial state
    fn connect(&mut self, t1: usize, t2: usize) {
        self.nodes[t1].siblings.push(t2);
        self.nodes[t2].siblings.push(t1);
    }

    /// Mark two term as an unification goal
    fn add_goal(&mut self, t1: usize, t2: usize) {
        self.connect(t1, t2);
        self.nodes[t1].initial_siblings += 1;
        self.nodes[t2].initial_siblings += 1;
    }

    /// Remove an unification goal between two terms
    /// The goal MUST have been added before, and the graph must be in initial state
    fn remove_goal(&mut self, t1: usize, t2: usize) {
        let id1 = self.nodes[t1]
            .siblings
            .iter()
            .enumerate()
            .find(|(_, v)| **v == t2)
            .unwrap()
            .0;
        self.nodes[t1].siblings.swap_remove(id1);
        self.nodes[t1].initial_siblings -= 1;
        let id2 = self.nodes[t2]
            .siblings
            .iter()
            .enumerate()
            .find(|(_, v)| **v == t1)
            .unwrap()
            .0;
        self.nodes[t2].siblings.swap_remove(id2);
        self.nodes[t2].initial_siblings -= 1;
    }

    /// Reset the graph into an initial state
    fn reset(&mut self) {
        self.nodes.iter_mut().for_each(|n| n.reset())
    }
}

pub struct UnifState {
    gr: Graph,
}

impl UnifState {
    /// Initialize an empty state
    pub fn new() -> Self {
        Self { gr: Graph::new() }
    }

    /// Add a new term to the state
    pub fn add(&mut self, ctx: &Context, term: AnyTerm) {
        self.gr.insert(ctx, term.to_typed());
    }

    /// Add a goal of unification between two subterms
    /// The terms must have been added before
    pub fn add_goal(&mut self, t1: AnyTerm, t2: AnyTerm) {
        let id1 = self.gr.mapping.get(&t1.to_typed()).unwrap().clone();
        let id2 = self.gr.mapping.get(&t2.to_typed()).unwrap().clone();
        self.gr.add_goal(id1, id2)
    }

    /// Remove a previously added goal
    pub fn rm_goal(&mut self, t1: AnyTerm, t2: AnyTerm) {
        let id1 = self.gr.mapping.get(&t1.to_typed()).unwrap().clone();
        let id2 = self.gr.mapping.get(&t2.to_typed()).unwrap().clone();
        self.gr.remove_goal(id1, id2)
    }

    /// Helper function taken from Paterson paper
    fn finish(&mut self, mut sigma: &mut Substitution, r: usize) -> bool {
        if self.gr.nodes[r].status.is_deleted() {
            true
        } else if self.gr.nodes[r].pointer.is_some() {
            false
        } else {
            self.gr.nodes[r].pointer = Some(r);
            let mut stack: Vec<usize> = Vec::new();
            stack.push(r);
            while let Some(s) = stack.pop() {
                if !self.gr.nodes[r].status.is_var().is_some()
                    && !self.gr.nodes[s].status.is_var().is_some()
                    && !self.gr.nodes[r].value.same_head(&self.gr.nodes[s].value)
                {
                    return false;
                }

                {
                    // I don't think I can avoid this clone without a lot refactoring to separate
                    // dynamic from static data in Graph
                    let ancestors = self.gr.nodes[s].ancestors.clone();
                    if !ancestors.iter().all(|a| self.finish(&mut sigma, *a)) {
                        return false;
                    }
                }

                // Process siblings
                while let Some(t) = self.gr.nodes[s].siblings.pop() {
                    let ptr = self.gr.nodes[t].pointer.get_or_insert(r);
                    if *ptr != r {
                        return false;
                    };
                    stack.push(t)
                }

                // Propagate siblings or extend substitution
                if s != r {
                    if let Some(s) = self.gr.nodes[s].status.is_var() {
                        sigma.push((s, self.gr.nodes[r].value.clone()))
                    }
                    if self.gr.nodes[s].status.is_live() {
                        let descendants = self.gr.nodes[s].descendants.len();
                        assert_eq!(
                            descendants,
                            self.gr.nodes[r].descendants.len(),
                            "Same symbols should have same number of descendants"
                        );
                        for id in 0..descendants {
                            self.gr.connect(
                                self.gr.nodes[s].descendants[id],
                                self.gr.nodes[r].descendants[id],
                            )
                        }
                    }
                    self.gr.nodes[s].status = NodeStatus::Deleted;
                }
            }
            self.gr.nodes[r].status = NodeStatus::Deleted;
            true
        }
    }

    /// Try to solve for all the goals in the unification state
    /// Find the MGU of pairs of terms in linear time using the algorithm C described in the paper:
    ///     Paterson, M.S., and M.N. Wegman. “Linear Unification.”
    ///     Journal of Computer and System Sciences 16, no. 2 (April 1978): 158–67.
    ///     https://doi.org/10.1016/0022-0000(78)90043-0.
    pub fn solve(&mut self) -> Option<Substitution> {
        let size = self.gr.nodes.len();

        // Prepare the substitution
        let mut sigma = Substitution::new();

        // Iterate over all nodes
        for id in 0..size {
            if !self.gr.nodes[id].status.is_live() {
                continue;
            }
            if !self.finish(&mut sigma, id) {
                self.gr.reset();
                return None;
            }
        }

        // Iterate over all variables
        for id in 0..size {
            if !self.gr.nodes[id].status.is_var().is_some() {
                continue;
            }
            if !self.finish(&mut sigma, id) {
                self.gr.reset();
                return None;
            }
        }

        // If we reached this point we've succeeded
        self.gr.reset();
        Some(sigma)
    }
}

/// Wrapper over UnifState when we only want to unify two terms
pub fn unify(ctx: &Context, t1: AnyTerm, t2: AnyTerm) -> Option<Substitution> {
    let mut unif = UnifState::new();
    unif.add(ctx, t1.clone());
    unif.add(ctx, t2.clone());
    unif.add_goal(t1, t2);
    unif.solve()
}

#[cfg(test)]
mod tests {
    use crate::anyterm::AnyTerm;
    use crate::data::{ActualCategory, ActualMorphism, ActualObject};
    use crate::data::{CategoryData, MorphismData, ObjectData};
    use crate::data::{Context, ProofObject};
    use crate::dsl::{cat, mph, obj};
    use crate::substitution::Substitutable;
    use crate::unification::{unify, UnifState};

    #[test]
    pub fn simple() {
        // This is the same example as the paper, encoded into our theory with
        //     x1, x2, x3 : cat(o,o)
        //     G(x) : 1_o o x
        //     F(x,y) : y o x
        let ctx = Context::new();
        let cat = cat!(ctx, :0);
        let o = obj!(ctx, (:1) in cat);
        let x1 = mph!(ctx, (?0) : o -> o);
        let x2 = mph!(ctx, (?1) : o -> o);
        let x3 = mph!(ctx, (?2) : o -> o);
        let m1 = mph!(ctx, (x2 >> (id o)) >> (x3 >> (id o)));
        let m2 = mph!(ctx, x1 >> x2);
        let sigma = unify(&ctx, AnyTerm::Mph(m1.clone()), AnyTerm::Mph(m2.clone()))
            .ok_or("Couldn't unify m1 and m2")
            .unwrap();
        let m1 = m1.subst(&ctx, &sigma);
        let m2 = m2.subst(&ctx, &sigma);
        assert_eq!(m1, m2, "Unifier is wrong");
    }

    #[test]
    pub fn in_type() {
        let ctx = Context::new();
        let cat = cat!(ctx, :0);
        let o = obj!(ctx, (:1) in cat);
        let o_ex = obj!(ctx, (?42) in cat);
        let m1_ex = mph!(ctx, (:2) : o -> o_ex);
        let m2_ex = mph!(ctx, (:3) : o_ex -> o);
        let m1 = mph!(ctx, (:2) : o -> o);
        let m2 = mph!(ctx, (:3) : o -> o);
        let m_ex = mph!(ctx, m1_ex >> m2_ex);
        let m = mph!(ctx, m1 >> m2);
        assert!(m_ex.check(&ctx), "m_ex is invalid");
        assert!(m.check(&ctx), "m is invalid");
        let sigma = unify(&ctx, AnyTerm::Mph(m_ex.clone()), AnyTerm::Mph(m.clone()))
            .ok_or("Couldn't unify m and m_ex")
            .unwrap();
        let m_ex = m_ex.subst(&ctx, &sigma);
        let m = m.subst(&ctx, &sigma);
        assert_eq!(m_ex, m, "Unifier is wrong");
    }

    #[test]
    pub fn ununifyable() {
        let ctx = Context::new();
        let cat = cat!(ctx, :0);
        let a = obj!(ctx, (:1) in cat);
        let b = obj!(ctx, (:2) in cat);
        let mab = mph!(ctx, (?0) : a -> b);
        let mba = mph!(ctx, (?1) : b -> a);
        let maa1 = mph!(ctx, (:3) : a -> a);
        let maa2 = mph!(ctx, (:4) : a -> a);
        let m_ex = mph!(ctx, mab >> mba);
        let m = mph!(ctx, maa1 >> maa2);
        assert!(m_ex.check(&ctx), "m_ex is invalid");
        assert!(m.check(&ctx), "m is invalid");
        let sigma = unify(&ctx, AnyTerm::Mph(m_ex.clone()), AnyTerm::Mph(m.clone()));
        match sigma {
            Some(_) => panic!("Unification succeeded when it should have failed"),
            None => (),
        }
    }

    #[test]
    pub fn multiple() {
        let ctx = Context::new();
        let mut unif = UnifState::new();
        let cat = cat!(ctx, :0);
        let a = obj!(ctx, (:1) in cat);
        let b = obj!(ctx, (:2) in cat);
        let m1 = mph!(ctx, (:4) : a -> ((?1) in cat));
        let m2 = mph!(ctx, (:4) : ((?0) in cat) -> b);
        let m = mph!(ctx, (:4) : ((?0) in cat) -> ((?1) in cat));
        unif.add(&ctx, AnyTerm::Mph(m1.clone()));
        unif.add(&ctx, AnyTerm::Mph(m2.clone()));
        unif.add(&ctx, AnyTerm::Mph(m.clone()));
        unif.add_goal(AnyTerm::Mph(m1.clone()), AnyTerm::Mph(m.clone()));
        unif.add_goal(AnyTerm::Mph(m2.clone()), AnyTerm::Mph(m.clone()));
        let sigma = unif
            .solve()
            .ok_or("Couldn't unify m with m1 and m2 silmutaneously")
            .unwrap();
        assert_eq!(
            m1.subst(&ctx, &sigma),
            m.clone().subst(&ctx, &sigma),
            "m1 and m weren't successfully unified"
        );
        assert_eq!(
            m2.subst(&ctx, &sigma),
            m.clone().subst(&ctx, &sigma),
            "m2 and m weren't successfully unified"
        );
    }
}
