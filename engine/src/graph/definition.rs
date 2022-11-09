use crate::data::Equality;
use crate::data::Morphism;
use crate::data::Object;
use std::rc::Rc;
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
    pub nodes: Vec<Rc<Object>>,
    pub edges: Vec<Vec<(usize, Rc<Morphism>)>>,
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

    pub fn check(&self, gr: &Graph) -> bool {
        self.start < gr.nodes.len()
            && self.end < gr.nodes.len()
            && Face::check_path(&self.left, gr, self.start, 0)
                .map(|node| node == self.end)
                .unwrap_or(false)
            && Face::check_path(&self.right, gr, self.start, 0)
                .map(|node| node == self.end)
                .unwrap_or(false)
            && self.eq.check()
    }
}

impl Graph {
    pub fn check(&self) -> bool {
        self.nodes.len() == self.edges.len()
            && self.nodes.iter().all(|o| o.check())
            && self.edges.iter().enumerate().all(|(start, out)| {
                out.iter().all(|(next, mph)| {
                    mph.src() == self.nodes[start] && mph.dst() == self.nodes[*next]
                })
            })
            && self.faces.iter().all(|fce| fce.check(self))
    }
}
