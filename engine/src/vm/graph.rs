use crate::graph;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GraphId {
    Node(usize),
    Morphism(usize, usize),
    Face(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LabelSource {
    Manual,
    Render(u64),
    #[default]
    None,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NodeLabel {
    pub pos: egui::Pos2,
    pub name: Option<String>,
    pub label: String,
    pub label_source: LabelSource,
    // Invariant: when a node is hidden, all in/out-going edges must be hidden too
    pub hidden: bool,
}

impl NodeLabel {
    pub fn new() -> Self {
        Self {
            pos: egui::Pos2::ZERO,
            name: None,
            label: String::new(),
            label_source: LabelSource::None,
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
    pub shape: Vec<[egui::Pos2; 4]>,
    pub name: Option<String>,
    pub id: usize,
    pub label: String,
    pub label_pos: egui::Pos2,
    pub label_source: LabelSource,
    pub style: EdgeStyle,
    pub hidden: bool,
}

impl EdgeLabel {
    pub fn new() -> Self {
        Self {
            shape: Vec::new(),
            name: None,
            label: String::new(),
            label_pos: egui::Pos2::ZERO,
            label_source: LabelSource::None,
            id: 0, // Invalid number, but will be set during VM initialization
            style: EdgeStyle::default(),
            hidden: false,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FaceLabel {
    pub label: String,
    pub label_source: LabelSource,
    pub name: Option<String>,
    pub hidden: bool,
}

impl FaceLabel {
    pub fn new() -> Self {
        Self {
            name: None,
            label: String::new(),
            label_source: LabelSource::None,
            hidden: false,
        }
    }
}

pub type Graph = graph::Graph<NodeLabel, EdgeLabel, FaceLabel>;
