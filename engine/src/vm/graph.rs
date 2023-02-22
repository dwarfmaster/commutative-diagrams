use crate::graph;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GraphId {
    Node(usize),
    Morphism(usize, usize),
    Face(usize),
}

#[derive(Debug, Default)]
pub struct NodeLabel {
    pub pos: egui::Pos2,
    pub name: Option<String>,
    pub label: String,
    // Invariant: when a node is hidden, all in/out-going edges must be hidden too
    pub hidden: bool,
}

impl NodeLabel {
    pub fn new(label: String) -> Self {
        Self {
            pos: egui::Pos2::ZERO,
            name: None,
            label,
            hidden: false,
        }
    }
}

// Default is all false
#[derive(Debug, Default)]
pub struct EdgeStyle {
    pub left: bool,
    pub right: bool,
    pub highlight: bool,
}

#[derive(Debug, Default)]
pub struct EdgeLabel {
    pub shape: Vec<[egui::Pos2; 4]>,
    pub name: Option<String>,
    pub id: usize,
    pub label: String,
    pub label_pos: egui::Pos2,
    pub style: EdgeStyle,
    pub hidden: bool,
}

impl EdgeLabel {
    pub fn new(label: String) -> Self {
        Self {
            shape: Vec::new(),
            name: None,
            label,
            label_pos: egui::Pos2::ZERO,
            id: 0, // Invalid number, but will be set during VM initialization
            style: EdgeStyle::default(),
            hidden: false,
        }
    }
}

#[derive(Debug, Default)]
pub struct FaceLabel {
    pub label: String,
    pub name: Option<String>,
    pub hidden: bool,
}

impl FaceLabel {
    pub fn new(label: String) -> Self {
        Self {
            name: None,
            label,
            hidden: false,
        }
    }
}

pub type Graph = graph::Graph<NodeLabel, EdgeLabel, FaceLabel>;
