use crate::anyterm::AnyTerm;
use crate::data::{Context, Equality, Morphism, Object};
use crate::substitution::Substitutable;
use std::vec::Vec;

/// A pair of two paths in a graph with the same start and end
#[derive(Clone)]
pub struct Face {
    pub start: usize,
    pub end: usize,
    pub left: Vec<usize>,
    pub right: Vec<usize>,
    pub eq: Equality,
}

/// Adjacency list 2-graph of morphisms
#[derive(Clone)]
pub struct Graph {
    pub nodes: Vec<Object>,
    pub edges: Vec<Vec<(usize, Morphism)>>,
    pub faces: Vec<Face>,
}

impl Face {
    fn check_path(path: &[usize], gr: &Graph, node: usize, next: usize) -> Option<usize> {
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

    pub fn check(&self, ctx: &Context, gr: &Graph) -> bool {
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

impl Substitutable for Face {
    fn subst_slice(self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Self {
        Face {
            start: self.start,
            end: self.end,
            left: self.left,
            right: self.right,
            eq: self.eq.subst_slice(ctx, sigma),
        }
    }
}

impl Graph {
    pub fn check(&self, ctx: &Context) -> bool {
        self.nodes.len() == self.edges.len()
            && self.nodes.iter().all(|o| o.check(ctx))
            && self.edges.iter().enumerate().all(|(start, out)| {
                out.iter().all(|(next, mph)| {
                    mph.src(ctx) == self.nodes[start] && mph.dst(ctx) == self.nodes[*next]
                })
            })
            && self.faces.iter().all(|fce| fce.check(ctx, self))
    }
}

impl Substitutable for Graph {
    fn subst_slice(mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Self {
        self.nodes = self
            .nodes
            .into_iter()
            .map(|node| node.subst_slice(ctx, sigma))
            .collect();
        self.edges = self
            .edges
            .into_iter()
            .map(|edges| {
                edges
                    .into_iter()
                    .map(|(dst, edge)| (dst, edge.subst_slice(ctx, sigma)))
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
