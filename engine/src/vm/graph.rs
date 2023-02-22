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
}

impl NodeLabel {
    pub fn new(label: String) -> Self {
        Self {
            pos: egui::Pos2::ZERO,
            name: None,
            label,
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
    pub style: EdgeStyle,
}

impl EdgeLabel {
    pub fn new(label: String) -> Self {
        Self {
            shape: Vec::new(),
            name: None,
            label,
            id: 0, // Invalid number, but will be set during VM initialization
            style: EdgeStyle::default(),
        }
    }
}

#[derive(Debug, Default)]
pub struct FaceLabel {
    pub label: String,
    pub name: Option<String>,
}

impl FaceLabel {
    pub fn new(label: String) -> Self {
        Self { name: None, label }
    }
}

pub type Graph = graph::Graph<NodeLabel, EdgeLabel, FaceLabel>;
