use crate::anyterm::AnyTerm;
use crate::data::{Context, Equality, Morphism, Object};
use crate::substitution::{Substitutable, SubstitutableInPlace};
use std::vec::Vec;

/// A pair of two paths in a graph with the same start and end
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FaceImpl<EqType, FaceLabel> {
    pub start: usize,
    pub end: usize,
    pub left: Vec<usize>,
    pub right: Vec<usize>,
    pub eq: EqType,
    pub label: FaceLabel,
}

pub type FaceParsed<FaceLabel> = FaceImpl<Equality, FaceLabel>;
pub type Face<FaceLabel> = FaceImpl<Equality, FaceLabel>;

/// Adjacency list 2-graph of morphisms
#[derive(Clone, Debug)]
pub struct GraphImpl<EqType, NodeLabel, EdgeLabel, FaceLabel> {
    pub nodes: Vec<(Object, NodeLabel)>,
    pub edges: Vec<Vec<(usize, EdgeLabel, Morphism)>>,
    pub faces: Vec<FaceImpl<EqType, FaceLabel>>,
}

pub type GraphParsed<NL, EL, FL> = GraphImpl<Equality, NL, EL, FL>;
pub type Graph<NL, EL, FL> = GraphImpl<Equality, NL, EL, FL>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GraphId {
    Node(usize),
    Morphism(usize, usize),
    Face(usize),
}

impl<FL> Face<FL> {
    fn check_path<NL, EL>(
        path: &[usize],
        gr: &Graph<NL, EL, FL>,
        node: usize,
        next: usize,
    ) -> Option<usize> {
        if path.len() <= next {
            Some(node)
        } else if gr.edges.len() <= node {
            None
        } else if gr.edges[node].len() <= path[next] {
            None
        } else {
            Face::check_path(path, gr, gr.edges[node][path[next]].0, next + 1)
        }
    }

    pub fn check<NL, EL>(&self, ctx: &Context, gr: &Graph<NL, EL, FL>) -> bool {
        self.start < gr.nodes.len()
            && self.end < gr.nodes.len()
            && Face::check_path(&self.left, gr, self.start, 0)
                .map(|node| node == self.end)
                .unwrap_or(false)
            && Face::check_path(&self.right, gr, self.start, 0)
                .map(|node| node == self.end)
                .unwrap_or(false)
            && self.eq.check(ctx)
    }
}

impl<FL> Substitutable for Face<FL> {
    fn subst_slice(self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Self {
        Face {
            start: self.start,
            end: self.end,
            left: self.left,
            right: self.right,
            eq: self.eq.subst_slice(ctx, sigma),
            label: self.label,
        }
    }
}

impl<FL> SubstitutableInPlace for Face<FL> {
    fn subst_slice_in_place(&mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]) {
        self.eq.subst_slice_in_place(ctx, sigma);
    }
}

impl<NL, EL, FL> Graph<NL, EL, FL> {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            edges: Vec::new(),
            faces: Vec::new(),
        }
    }

    pub fn check(&self, ctx: &Context) -> bool {
        self.nodes.len() == self.edges.len()
            && self.nodes.iter().all(|o| o.0.check(ctx))
            && self.edges.iter().enumerate().all(|(start, out)| {
                out.iter().all(|(next, _, mph)| {
                    mph.src(ctx) == self.nodes[start].0 && mph.dst(ctx) == self.nodes[*next].0
                })
            })
            && self.faces.iter().all(|fce| fce.check(ctx, self))
    }

    pub fn edge_by_id(&self, id: usize) -> Option<(usize, usize)> {
        let mut acc: usize = 0;
        for i in 0..self.edges.len() {
            let prev = acc;
            acc += self.edges[i].len();
            if id < acc {
                return Some((i, id - prev));
            }
        }
        None
    }
}

impl<NL, EL, FL> Substitutable for Graph<NL, EL, FL> {
    fn subst_slice(mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Self {
        self.nodes = self
            .nodes
            .into_iter()
            .map(|(node, label)| (node.subst_slice(ctx, sigma), label))
            .collect();
        self.edges = self
            .edges
            .into_iter()
            .map(|edges| {
                edges
                    .into_iter()
                    .map(|(dst, label, edge)| (dst, label, edge.subst_slice(ctx, sigma)))
                    .collect()
            })
            .collect();
        self.faces = self
            .faces
            .into_iter()
            .map(|face| face.subst_slice(ctx, sigma))
            .collect();
        self
    }
}

impl<NL, EL, FL> SubstitutableInPlace for Graph<NL, EL, FL> {
    fn subst_slice_in_place(&mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]) {
        for (n, _) in self.nodes.iter_mut() {
            n.subst_slice_in_place(ctx, sigma)
        }
        for src in 0..self.nodes.len() {
            for (_, _, e) in self.edges[src].iter_mut() {
                e.subst_slice_in_place(ctx, sigma)
            }
        }
        for fce in self.faces.iter_mut() {
            fce.eq.subst_slice_in_place(ctx, sigma)
        }
    }
}
