use crate::graph;
use bevy::ecs::system::Resource;
use egui::Vec2;

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

#[derive(Debug, Default)]
pub struct EdgeLabel {
    pub shape: Vec<[egui::Pos2; 4]>,
    pub name: String,
    pub label: String,
}

impl EdgeLabel {
    pub fn new(name: String) -> Self {
        Self {
            shape: Vec::new(),
            name: name.clone(),
            label: name,
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

#[derive(Resource)]
pub struct GraphDisplay {
    pub graph: Graph,
    pub offset: Vec2,
    pub zoom: f32,
}

impl GraphDisplay {
    pub fn new(graph: Graph) -> Self {
        GraphDisplay {
            graph,
            offset: Vec2::ZERO,
            zoom: 1.0,
        }
    }
}
