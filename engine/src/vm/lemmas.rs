use super::graph::{EdgeLabel, FaceLabel, NodeLabel};
use crate::graph::GraphId;
use crate::lemmas;
use crate::remote::Mock;
use crate::remote::Remote;
use crate::vm::store::Context;
use crate::vm::VM;
use egui::Vec2;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub struct LemmaState {
    pub zoom: f32,
    pub offset: Vec2,
    pub focused: Option<GraphId>,
    pub hovered: Option<GraphId>,
    pub selected_face: Option<usize>,
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
            selected_face: None,
            selected: false,
            names: HashMap::new(),
        }
    }
}

pub type Lemma = lemmas::Lemma<LemmaState, NodeLabel, EdgeLabel, FaceLabel>;

impl Lemma {
    pub fn relabel<Rm: Remote>(&mut self, ctx: &mut Context<Rm>) {
        if let Some(pattern) = &mut self.pattern {
            for nd in 0..pattern.nodes.len() {
                pattern.nodes[nd].2.label = ctx.get_stored_label(pattern.nodes[nd].0);
            }
            for src in 0..pattern.nodes.len() {
                for mph in 0..pattern.edges[src].len() {
                    pattern.edges[src][mph].1.label =
                        ctx.get_stored_label(pattern.edges[src][mph].2);
                }
            }
            for fce in 0..pattern.faces.len() {
                pattern.faces[fce].label.label = "{{todo!}}".to_string();
            }
        }
    }

    pub fn name<Rm: Remote + Sync + Send>(&mut self, ctx: &mut Context<Rm>) {
        if let Some(pattern) = &mut self.pattern {
            for nd in 0..pattern.nodes.len() {
                pattern.nodes[nd].2.name =
                    VM::<Rm, ()>::name_compute_node(ctx, &pattern, &self.graphical_state.names, nd);
                self.graphical_state
                    .names
                    .insert(pattern.nodes[nd].2.name.clone(), GraphId::Node(nd));
            }
            for src in 0..pattern.nodes.len() {
                for mph in 0..pattern.edges[src].len() {
                    pattern.edges[src][mph].1.name = VM::<Rm, ()>::name_compute_morphism(
                        ctx,
                        &pattern,
                        &self.graphical_state.names,
                        src,
                        mph,
                    );
                    self.graphical_state.names.insert(
                        pattern.edges[src][mph].1.name.clone(),
                        GraphId::Morphism(src, mph),
                    );
                }
            }
            for fce in 0..pattern.faces.len() {
                pattern.faces[fce].label.name = VM::<Rm, ()>::name_compute_face(
                    ctx,
                    &pattern,
                    &self.graphical_state.names,
                    "Lem",
                    fce,
                );
                self.graphical_state
                    .names
                    .insert(pattern.faces[fce].label.name.clone(), GraphId::Face(fce));
            }
        }
    }

    pub fn show_face(&mut self, fce: usize) {
        if let Some(pattern) = &mut self.pattern {
            VM::<Mock, ()>::show_face_impl(pattern, fce);
        }
    }

    pub fn unshow_face(&mut self, fce: usize) {
        if let Some(pattern) = &mut self.pattern {
            VM::<Mock, ()>::unshow_face_impl(pattern, fce);
        }
    }
}
