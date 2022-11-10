use crate::data::{Context, Equality, Morphism, Object};
use std::vec::Vec;

/// A pair of two paths in a graph with the same start and end
pub struct Face {
    pub start: usize,
    pub end: usize,
    pub left: Vec<usize>,
    pub right: Vec<usize>,
    pub eq: Equality,
}

/// Adjacency list 2-graph of morphisms
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

    pub fn check(&self, ctx: &mut Context, gr: &Graph) -> bool {
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

impl Graph {
    pub fn check(&self, ctx: &mut Context) -> bool {
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
