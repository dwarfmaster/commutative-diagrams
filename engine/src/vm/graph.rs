use crate::graph;

#[derive(Debug, Default)]
pub struct NodeLabel {
    pub pos: egui::Pos2,
    pub name: String,
    pub label: String,
}

impl NodeLabel {
    pub fn new(name: String) -> Self {
        Self {
            pos: egui::Pos2::ZERO,
            name: name.clone(),
            label: name,
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
    pub name: String,
    pub label: String,
    pub style: EdgeStyle,
}

impl EdgeLabel {
    pub fn new(name: String) -> Self {
        Self {
            shape: Vec::new(),
            name: name.clone(),
            label: name,
            style: EdgeStyle::default(),
        }
    }
}

#[derive(Debug, Default)]
pub struct FaceLabel {
    pub label: String,
    pub name: String,
}

impl FaceLabel {
    pub fn new(name: String) -> Self {
        Self {
            name: name.clone(),
            label: name,
        }
    }
}

pub type Graph = graph::Graph<NodeLabel, EdgeLabel, FaceLabel>;
