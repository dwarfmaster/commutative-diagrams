use crate::graph;
use crate::graph::GraphId;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NodeLabel {
    // Refers to position in the layout engine
    pub pos: Option<usize>,
    // Wether the position is fixed or updated by the layout engine
    pub pinned: bool,
    pub name: String,
    pub label: String,
    // Invariant: when a node is hidden, all in/out-going edges must be hidden too
    pub hidden: bool,
}

impl NodeLabel {
    pub fn new() -> Self {
        Self {
            pos: None,
            pinned: false,
            name: "".to_string(),
            label: String::new(),
            hidden: false,
        }
    }
}

// Default is all false
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct EdgeStyle {
    pub left: bool,
    pub right: bool,
    pub highlight: bool,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct EdgeLabel {
    // Start and end are given by source and destination, so we only need the
    // control point, which is handled by the layouting engine
    pub control: Option<usize>,
    // Wether the control point is pinned
    pub pinned: bool,
    pub name: String,
    pub label: String,
    pub style: EdgeStyle,
    pub hidden: bool,
}

impl EdgeLabel {
    pub fn new() -> Self {
        Self {
            control: None,
            pinned: false,
            name: "".to_string(),
            label: String::new(),
            style: EdgeStyle::default(),
            hidden: false,
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum FaceStatus {
    Goal,
    Refined,
    #[default]
    Hypothesis,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FaceLabel {
    pub label: String,
    pub name: String,
    pub hidden: bool,
    pub folded: bool,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
    pub status: FaceStatus,
}

impl FaceLabel {
    pub fn new() -> Self {
        Self {
            name: "".to_string(),
            label: String::new(),
            hidden: false,
            folded: false,
            parent: None,
            children: Vec::new(),
            status: FaceStatus::Hypothesis,
        }
    }
}

pub type GraphParsed = graph::GraphParsed<NodeLabel, EdgeLabel, FaceLabel>;
pub type Graph = graph::Graph<NodeLabel, EdgeLabel, FaceLabel>;

impl Graph {
    pub fn pinned(&self, id: GraphId) -> bool {
        use GraphId::*;
        match id {
            Node(nd) => self.nodes[nd].2.pinned,
            Morphism(src, mph) => self.edges[src][mph].1.pinned,
            _ => false,
        }
    }
}
