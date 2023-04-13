use super::graph::{EdgeLabel, FaceLabel, NodeLabel};
use crate::data::Context;
use crate::graph::GraphId;
use crate::lemmas;
use crate::vm::VM;
use egui::Vec2;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub struct LemmaState {
    pub zoom: f32,
    pub offset: Vec2,
    pub focused: Option<GraphId>,
    pub hovered: Option<GraphId>,
    pub selected: bool,
    pub names: HashMap<String, GraphId>,
}

impl Default for LemmaState {
    fn default() -> Self {
        Self {
            zoom: 1.0,
            offset: Vec2::ZERO,
            focused: None,
            hovered: None,
            selected: false,
            names: HashMap::new(),
        }
    }
}

pub type Lemma = lemmas::Lemma<LemmaState, NodeLabel, EdgeLabel, FaceLabel>;

impl Lemma {
    pub fn relabel(&mut self, ctx: &Context) {
        for nd in 0..self.pattern.nodes.len() {
            self.pattern.nodes[nd].1.label = self.pattern.nodes[nd].0.render(ctx, 100);
        }
        for src in 0..self.pattern.nodes.len() {
            for mph in 0..self.pattern.edges[src].len() {
                self.pattern.edges[src][mph].1.label =
                    self.pattern.edges[src][mph].2.render(ctx, 100);
            }
        }
        for fce in 0..self.pattern.faces.len() {
            self.pattern.faces[fce].label.label = self.pattern.faces[fce].eq.render(ctx, 100);
        }
    }

    pub fn name(&mut self, ctx: &mut Context) {
        for nd in 0..self.pattern.nodes.len() {
            self.pattern.nodes[nd].1.name =
                VM::name_compute_node(ctx, &self.pattern, &self.graphical_state.names, nd);
        }
        for src in 0..self.pattern.nodes.len() {
            for mph in 0..self.pattern.edges[src].len() {
                self.pattern.edges[src][mph].1.name = VM::name_compute_morphism(
                    ctx,
                    &self.pattern,
                    &self.graphical_state.names,
                    src,
                    mph,
                );
            }
        }
        for fce in 0..self.pattern.faces.len() {
            self.pattern.faces[fce].label.name =
                VM::name_compute_face(ctx, &self.pattern, &self.graphical_state.names, "Lem", fce);
        }
    }
}
