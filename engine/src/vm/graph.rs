use crate::graph;
use lens_rs::Lens;

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

#[derive(Debug, Default, Clone, PartialEq, Eq, Lens)]
pub struct NodeLabel {
    #[optic]
    pub pos: egui::Pos2,
    #[optic]
    pub name: Option<String>,
    #[optic]
    pub label: String,
    #[optic]
    pub label_source: LabelSource,
    // Invariant: when a node is hidden, all in/out-going edges must be hidden too
    #[optic]
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
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Lens)]
pub struct EdgeStyle {
    #[optic]
    pub left: bool,
    #[optic]
    pub right: bool,
    #[optic]
    pub highlight: bool,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Lens)]
pub struct EdgeLabel {
    #[optic]
    pub shape: Vec<[egui::Pos2; 4]>,
    #[optic]
    pub name: Option<String>,
    #[optic]
    pub id: usize,
    #[optic]
    pub label: String,
    #[optic]
    pub label_pos: egui::Pos2,
    #[optic]
    pub label_source: LabelSource,
    #[optic]
    pub style: EdgeStyle,
    #[optic]
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

#[derive(Debug, Default, Clone, PartialEq, Eq, Lens)]
pub struct FaceLabel {
    #[optic]
    pub label: String,
    #[optic]
    pub label_source: LabelSource,
    #[optic]
    pub name: Option<String>,
    #[optic]
    pub hidden: bool,
    // The paths to show for this equality
    #[optic]
    pub left: Option<Vec<usize>>,
    #[optic]
    pub right: Option<Vec<usize>>,
}

impl FaceLabel {
    pub fn new() -> Self {
        Self {
            name: None,
            label: String::new(),
            label_source: LabelSource::None,
            hidden: false,
            left: None,
            right: None,
        }
    }
}

pub type Graph = graph::Graph<NodeLabel, EdgeLabel, FaceLabel>;
