use super::graph::{EdgeLabel, FaceLabel, NodeLabel};
use crate::graph::GraphId;
use crate::lemmas;
use crate::remote::Mock;
use crate::remote::Remote;
use crate::vm::layout::LayoutEngine;
use crate::vm::store::Context;
use crate::vm::{Graph, VM};
use egui::Vec2;
use std::collections::HashMap;

mod tree;
pub use tree::LemmaTree;

#[derive(Clone, Debug)]
pub struct LemmaState {
    pub zoom: f32,
    pub offset: Vec2,
    pub focused: Option<GraphId>,
    pub hovered: Option<GraphId>,
    pub selected_face: Option<usize>,
    pub selected: bool,
    pub names: HashMap<String, GraphId>,
    pub layout: LayoutEngine,
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
            layout: LayoutEngine::new(),
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

    pub fn recompute_face_statuses<Rm: Remote>(&mut self, ctx: &mut Context<Rm>) {
        let pattern = self.pattern.as_mut().unwrap();
        for fce in 0..pattern.faces.len() {
            pattern.faces[fce].label.status = ctx.compute_eq_status(&pattern.faces[fce].eq);
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

    pub fn get_pattern<Rm: Remote + Sync + Send>(&mut self, ctx: &mut Context<Rm>) {
        if self.pattern.is_some() {
            return;
        }

        let graph = ctx
            .remote
            .pattern::<NodeLabel, EdgeLabel, FaceLabel>(self.id)
            .unwrap();
        ctx.set_lem_context(self.id);
        let mut graph = graph.prepare(ctx);
        self.graphical_state.layout.particles_for_graph(&mut graph);
        self.graphical_state.layout.run();
        self.pattern = Some(graph);
        self.relabel(ctx);
        self.name(ctx);
        self.recompute_face_statuses(ctx);
        ctx.unset_lem_context();
    }

    pub fn instantiate<Rm: Remote + Sync + Send>(&mut self, ctx: &mut Context<Rm>) -> Graph {
        self.get_pattern(ctx);

        let graph = ctx
            .remote
            .instantiate::<NodeLabel, EdgeLabel, FaceLabel>(self.id)
            .unwrap();
        let mut graph = graph.prepare(ctx);

        // Copy labels
        let pattern = self.pattern.as_ref().unwrap();
        for nd in 0..graph.nodes.len() {
            graph.nodes[nd].2 = pattern.nodes[nd].2.clone();
        }
        for src in 0..graph.nodes.len() {
            for mph in 0..graph.edges[src].len() {
                graph.edges[src][mph].1 = pattern.edges[src][mph].1.clone();
            }
        }
        for fce in 0..graph.faces.len() {
            graph.faces[fce].label = pattern.faces[fce].label.clone();
        }
        // TODO unselect face

        graph
    }
}
