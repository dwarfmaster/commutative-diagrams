use super::graph::{EdgeLabel, FaceLabel, NodeLabel};
use crate::graph::GraphId;
use crate::lemmas;
use egui::Vec2;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub struct LemmaState {
    pub zoom: f32,
    pub offset: Vec2,
    pub focused: Option<GraphId>,
    pub selected: bool,
    pub names: HashMap<String, GraphId>,
}

impl Default for LemmaState {
    fn default() -> Self {
        Self {
            zoom: 1.0,
            offset: Vec2::ZERO,
            focused: None,
            selected: false,
            names: HashMap::new(),
        }
    }
}

pub type Lemma = lemmas::Lemma<LemmaState, NodeLabel, EdgeLabel, FaceLabel>;
