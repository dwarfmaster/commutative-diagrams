use super::eq::Eq;
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

pub type FaceParsed<FaceLabel> = FaceImpl<u64, FaceLabel>;
pub type Face<FaceLabel> = FaceImpl<Eq, FaceLabel>;

/// Adjacency list 2-graph of morphisms
#[derive(Clone, Debug)]
pub struct GraphImpl<EqType, NodeLabel, EdgeLabel, FaceLabel> {
    pub nodes: Vec<(/*node*/ u64, /*cat*/ u64, NodeLabel)>,
    pub edges: Vec<Vec<(usize, EdgeLabel, u64)>>,
    pub faces: Vec<FaceImpl<EqType, FaceLabel>>,
}

pub type GraphParsed<NL, EL, FL> = GraphImpl<u64, NL, EL, FL>;
pub type Graph<NL, EL, FL> = GraphImpl<Eq, NL, EL, FL>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GraphId {
    Node(usize),
    Morphism(usize, usize),
    Face(usize),
}

impl<EQ, FL> FaceImpl<EQ, FL> {
    fn check_path<NL, EL>(
        path: &[usize],
        gr: &GraphImpl<EQ, NL, EL, FL>,
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
            FaceImpl::check_path(path, gr, gr.edges[node][path[next]].0, next + 1)
        }
    }

    pub fn check<NL, EL>(&self, gr: &GraphImpl<EQ, NL, EL, FL>) -> bool {
        self.start < gr.nodes.len()
            && self.end < gr.nodes.len()
            && FaceImpl::check_path(&self.left, gr, self.start, 0)
                .map(|node| node == self.end)
                .unwrap_or(false)
            && FaceImpl::check_path(&self.right, gr, self.start, 0)
                .map(|node| node == self.end)
                .unwrap_or(false)
    }
}

impl<EQ, NL, EL, FL> GraphImpl<EQ, NL, EL, FL> {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            edges: Vec::new(),
            faces: Vec::new(),
        }
    }

    pub fn check(&self) -> bool {
        self.nodes.len() == self.edges.len() && self.faces.iter().all(|fce| fce.check(self))
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
