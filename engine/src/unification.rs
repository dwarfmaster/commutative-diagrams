use crate::anyterm::AnyTerm;
use crate::anyterm::IsTerm;
use crate::data::Context;
use crate::substitution::Substitution;
use std::collections::HashMap;
use std::ops::Deref;
use std::vec::Vec;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum NodeStatus {
    Deleted,
    Variable(u64),
    Live,
}

#[derive(Clone, Debug)]
enum NodeValue {
    RootLeft,
    RootRight,
    Term(AnyTerm),
}

impl NodeValue {
    fn same_head(&self, other: &Self) -> bool {
        use NodeValue::*;
        match (self, other) {
            // The two roots must be unifiable, so they must have the same head
            (RootLeft, RootLeft) => true,
            (RootRight, RootRight) => true,
            (RootLeft, RootRight) => true,
            (RootRight, RootLeft) => true,
            (Term(t1), Term(t2)) => t1.same_head(t2),
            _ => false,
        }
    }
}

struct Node {
    init_status: NodeStatus,
    status: NodeStatus,
    ancestors: Vec<usize>,
    descendants: Vec<usize>,
    siblings: Vec<usize>,
    pointer: Option<usize>,
    value: NodeValue,
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

/// Their are two root nodes, which are the first two
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
            pointer: None,
            value: NodeValue::Term(term),
        }
    }

    fn root(left: bool) -> Node {
        Node {
            init_status: NodeStatus::Live,
            status: NodeStatus::Live,
            ancestors: Vec::new(),
            descendants: Vec::new(),
            siblings: vec![(if left { 1 } else { 0 })],
            pointer: None,
            value: if left {
                NodeValue::RootLeft
            } else {
                NodeValue::RootRight
            },
        }
    }

    fn reset(&mut self) {
        self.status = self.init_status;
        self.pointer = None;
        self.siblings.clear();
        match self.value {
            NodeValue::RootLeft => self.siblings.push(1),
            NodeValue::RootRight => self.siblings.push(0),
            _ => (),
        }
    }

    fn assert_reset(&self) {
        match self.value {
            NodeValue::RootLeft => assert_eq!(self.siblings.len(), 1, "Root must have one sibling"),
            NodeValue::RootRight => {
                assert_eq!(self.siblings.len(), 1, "Root must have one sibling")
            }
            _ => assert!(
                self.siblings.is_empty(),
                "Non-root node must node have siblings"
            ),
        }
        assert!(self.pointer.is_none(), "Self pointer hasn't been cleared");
        assert_eq!(
            self.status, self.init_status,
            "Node status hasn't been resetted"
        );
    }
}

fn swap_remove_eq<T>(v: &mut Vec<T>, t: T)
where
    T: PartialEq,
{
    let id = v.iter().enumerate().find(|(_, v)| **v == t).unwrap().0;
    v.swap_remove(id);
}

impl Graph {
    fn new() -> Graph {
        Graph {
            nodes: vec![Node::root(true), Node::root(false)],
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
        assert_eq!(
            self.nodes[0].descendants.len(),
            self.nodes[1].descendants.len(),
            "Root nodes must have same arity"
        );
        self.nodes[0].descendants.push(t1);
        self.nodes[t1].ancestors.push(0);
        self.nodes[1].descendants.push(t2);
        self.nodes[t2].ancestors.push(1);
    }

    /// Remove an unification goal between two terms
    /// The goal MUST have been added before, and the graph must be in initial state
    fn remove_goal(&mut self, t1: usize, t2: usize) {
        swap_remove_eq(&mut self.nodes[0].descendants, t1);
        swap_remove_eq(&mut self.nodes[t1].ancestors, 0);
        swap_remove_eq(&mut self.nodes[1].descendants, t2);
        swap_remove_eq(&mut self.nodes[t2].ancestors, 1);
    }

    /// Reset the graph into an initial state
    fn reset(&mut self) {
        self.nodes.iter_mut().for_each(|n| n.reset())
    }

    fn assert_reset(&self) {
        self.nodes.iter().for_each(|n| n.assert_reset())
    }

    /// Print as a graphviz graph for debug purposes
    #[allow(dead_code)]
    fn debug_graphviz<O: std::io::Write>(&self, fmt: &mut O) -> Result<(), std::io::Error> {
        writeln!(fmt, "digraph {{")?;

        // Nodes
        for (i, nd) in self.nodes.iter().enumerate() {
            use AnyTerm::*;
            use NodeValue::*;
            let label = match &nd.value {
                RootLeft => "ROOT_LEFT".to_string(),
                RootRight => "ROOT_RIGHT".to_string(),
                Term(TypedCat(_)) => "CAT".to_string(),
                Term(TypedFunct(_)) => "FUNCT".to_string(),
                Term(TypedObj(_)) => "OBJ".to_string(),
                Term(TypedMph(_)) => "MPH".to_string(),
                Term(TypedEq(_)) => "EQ".to_string(),
                Term(Pobj(obj)) => {
                    use crate::data::ActualProofObject::*;
                    match obj.deref() {
                        Term(t) => format!(":{}", t),
                        Existential(e) => format!("?{}", e),
                        Cat(..) => "Cat".to_string(),
                        Funct(..) => "Funct".to_string(),
                        Obj(..) => "Obj".to_string(),
                        Mph(..) => "Mph".to_string(),
                        Eq(..) => "Eq".to_string(),
                        Composed(id, name, _) => format!("{}-{}", id, name),
                    }
                }
                Term(Cat(_)) => "Atomic".to_string(),
                Term(Funct(_)) => "Atomic".to_string(),
                Term(Obj(o)) => {
                    use crate::data::ActualObject::*;
                    match o.deref() {
                        Atomic(..) => "Atomic".to_string(),
                        Funct(..) => "Funct".to_string(),
                    }
                }
                Term(Mph(m)) => {
                    use crate::data::ActualMorphism::*;
                    match m.deref() {
                        Atomic(..) => "Atomic".to_string(),
                        Identity(..) => "Id".to_string(),
                        Comp(..) => "Comp".to_string(),
                        Funct(..) => "Funct".to_string(),
                    }
                }
                Term(Eq(e)) => {
                    use crate::data::ActualEquality::*;
                    match e.deref() {
                        Atomic(..) => "Atomic".to_string(),
                        Refl(..) => "Refl".to_string(),
                        Concat(..) => "Concat".to_string(),
                        Inv(..) => "Inv".to_string(),
                        Compose(..) => "Compose".to_string(),
                        Assoc(..) => "Assoc".to_string(),
                        LeftId(..) => "LeftId".to_string(),
                        RightId(..) => "RightId".to_string(),
                        RAp(..) => "RAp".to_string(),
                        LAp(..) => "LAp".to_string(),
                        FunctId(..) => "FId".to_string(),
                        FunctComp(..) => "FComp".to_string(),
                        FunctCtx(..) => "FCtx".to_string(),
                    }
                }
            };
            let color = match &nd.status {
                NodeStatus::Live => "black",
                NodeStatus::Deleted => "red",
                NodeStatus::Variable(_) => "green",
            };
            writeln!(fmt, "  n{} [label=\"{}\", color=\"{}\"];", i, label, color)?;
        }

        for (i, nd) in self.nodes.iter().enumerate() {
            // Subterms
            for (k, j) in nd.descendants.iter().enumerate() {
                writeln!(fmt, "  n{} -> n{} [label=\"{}\"];", i, j, k)?;
            }
            // Pointer
            if let Some(j) = &nd.pointer {
                writeln!(fmt, "  n{} -> n{} [color=\"red\"];", i, j)?;
            }
            // Siblings
            for j in &nd.siblings {
                if *j < i {
                    writeln!(fmt, "  n{} -> n{} [color=\"green\",dir=none];", i, j)?;
                }
            }
        }

        writeln!(fmt, "}}")
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
                        match &self.gr.nodes[r].value {
                            NodeValue::Term(t) => sigma.push((s, t.clone())),
                            // This means a variable should be unified with a root node, which
                            // never should append
                            _ => unreachable!("Variable never unifiable with root node"),
                        }
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
    fn solve_no_reset(&mut self) -> Option<Substitution> {
        let size = self.gr.nodes.len();

        // Prepare the substitution
        let mut sigma = Substitution::new();

        // Iterate over all nodes
        for id in 0..size {
            if !self.gr.nodes[id].status.is_live() {
                continue;
            }
            if !self.finish(&mut sigma, id) {
                return None;
            }
        }

        // Iterate over all variables
        for id in 0..size {
            if !self.gr.nodes[id].status.is_var().is_some() {
                continue;
            }
            if !self.finish(&mut sigma, id) {
                return None;
            }
        }

        // If we reached this point we've succeeded
        Some(sigma)
    }

    // Solve the goals, then reset the state to make the unifstate reusable.
    pub fn solve(&mut self) -> Option<Substitution> {
        let result = self.solve_no_reset();
        self.gr.reset();
        result
    }

    /// For debugging purposes, checks that the unification status is ready (should always be after
    /// solve). Fails with an assertion otherwise
    pub fn assert_ready(&self) {
        self.gr.assert_reset()
    }

    // For debugging purposes
    pub fn debug_graphviz<O: std::io::Write>(&self, fmt: &mut O) -> Result<(), std::io::Error> {
        self.gr.debug_graphviz(fmt)
    }
}

/// Wrapper over UnifState when we only want to unify two terms
pub fn unify(ctx: &Context, t1: AnyTerm, t2: AnyTerm) -> Option<Substitution> {
    let mut unif = UnifState::new();
    unif.add(ctx, t1.clone());
    unif.add(ctx, t2.clone());
    unif.add_goal(t1, t2);
    // For performance reasons we skip the reset part
    unif.solve_no_reset()
}

/// Same as unify, but write the state to a graphviz file after the unification
/// for debug purposes.
pub fn unify_debug(ctx: &Context, t1: AnyTerm, t2: AnyTerm, path: String) -> Option<Substitution> {
    let mut unif = UnifState::new();
    unif.add(ctx, t1.clone());
    unif.add(ctx, t2.clone());
    unif.add_goal(t1, t2);
    let result = unif.solve_no_reset();
    let mut file = std::fs::File::create(path).unwrap();
    unif.debug_graphviz(&mut file).unwrap();
    result
}

#[cfg(test)]
mod tests {
    use crate::anyterm::AnyTerm;
    use crate::data::Context;
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
    pub fn bidirectional() {
        let ctx = Context::new();
        let c = cat!(ctx, :0);
        let a = obj!(ctx, (:1) in c);
        let b = obj!(ctx, (:2) in c);
        let x = obj!(ctx, (?0) in c);
        let y = obj!(ctx, (?1) in c);
        let m = mph!(ctx, (:3) : x -> y);
        let f = mph!(ctx, (?3) : a -> b);
        assert!(m.check(&ctx), "m is invalid");
        assert!(f.check(&ctx), "f is invalid");
        let sigma = unify(&ctx, AnyTerm::Mph(m.clone()), AnyTerm::Mph(f.clone()))
            .ok_or("Couldn't unify f and m")
            .unwrap();
        assert_eq!(
            m.subst(&ctx, &sigma),
            f.subst(&ctx, &sigma),
            "Invalid unifier for m and f"
        );
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
        unif.assert_ready();
        assert_eq!(
            m1.clone().subst(&ctx, &sigma),
            m.clone().subst(&ctx, &sigma),
            "m1 and m weren't successfully unified"
        );
        assert_eq!(
            m2.clone().subst(&ctx, &sigma),
            m.clone().subst(&ctx, &sigma),
            "m2 and m weren't successfully unified"
        );

        unif.rm_goal(AnyTerm::Mph(m2.clone()), AnyTerm::Mph(m.clone()));
        let sigma = unif.solve().ok_or("Couldn't unify m with m1").unwrap();
        unif.assert_ready();
        assert_eq!(
            m1.subst(&ctx, &sigma),
            m.clone().subst(&ctx, &sigma),
            "m1 and m weren't successfuly unified"
        );
        assert_ne!(
            m2.subst(&ctx, &sigma),
            m.clone().subst(&ctx, &sigma),
            "m2 and m shouldn't be unified"
        );
    }
}
