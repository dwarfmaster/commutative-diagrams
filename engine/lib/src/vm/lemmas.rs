use super::config::Config;
use super::graph::{EdgeLabel, FaceLabel, NodeLabel};
use crate::graph::GraphId;
use crate::remote::Mock;
use crate::remote::Remote;
use crate::vm::layout::LayoutEngine;
use crate::vm::store::Context;
use crate::vm::vm::{GraphState, GraphicalState};
use crate::vm::VM;
use egui::Vec2;
use futures::lock::Mutex;
use std::collections::HashMap;

mod tree;
pub use tree::LemmaTree;

pub struct Lemma {
    pub id: u64,
    pub index: usize,
    pub name: String,
    pub namespace: Vec<String>,
    pub complete_name: String,
    pub selected: bool,
    pub pattern: Mutex<Option<GraphState>>,
    pub graphical: GraphicalState,
}

impl Lemma {
    pub fn new(id: u64, index: usize, name: String, namespace: Vec<String>) -> Self {
        let complete = itertools::Itertools::intersperse(
            namespace
                .iter()
                .map(|s| s.as_str())
                .chain(std::iter::once(name.as_str())),
            ">",
        )
        .collect();
        Self {
            id,
            index,
            name,
            namespace,
            complete_name: complete,
            selected: false,
            pattern: Mutex::new(None),
            graphical: GraphicalState {
                offset: Vec2::ZERO,
                zoom: 1f32,
                focused: None,
                hovered: None,
                dragged: None,
            },
        }
    }

    pub async fn relabel<Rm: Remote>(&mut self, ctx: &mut Context<Rm>) {
        let pattern = &mut self.pattern.lock().await;
        if let Some(pattern) = pattern.as_mut() {
            for nd in 0..pattern.graph.nodes.len() {
                pattern.graph.nodes[nd].2.label = ctx.get_stored_label(pattern.graph.nodes[nd].0);
            }
            for src in 0..pattern.graph.nodes.len() {
                for mph in 0..pattern.graph.edges[src].len() {
                    pattern.graph.edges[src][mph].1.label =
                        ctx.get_stored_label(pattern.graph.edges[src][mph].2);
                }
            }
            for fce in 0..pattern.graph.faces.len() {
                pattern.graph.faces[fce].label.label = "{{todo!}}".to_string();
            }
        }
    }

    pub async fn recompute_face_statuses<Rm: Remote>(&mut self, ctx: &mut Context<Rm>) {
        let pattern = &mut self.pattern.lock().await;
        let pattern = pattern.as_mut().unwrap();
        for fce in 0..pattern.graph.faces.len() {
            pattern.graph.faces[fce].label.status =
                ctx.compute_eq_status(&pattern.graph.faces[fce].eq);
        }
    }

    pub async fn name<Rm: Remote>(&mut self, ctx: &mut Context<Rm>) {
        let pattern = &mut self.pattern.lock().await;
        if let Some(pattern) = pattern.as_mut() {
            for nd in 0..pattern.graph.nodes.len() {
                pattern.graph.nodes[nd].2.name = pattern.name_compute_node(ctx, nd).await;
                pattern
                    .names
                    .insert(pattern.graph.nodes[nd].2.name.clone(), GraphId::Node(nd));
            }
            for src in 0..pattern.graph.nodes.len() {
                for mph in 0..pattern.graph.edges[src].len() {
                    pattern.graph.edges[src][mph].1.name =
                        pattern.name_compute_morphism(ctx, src, mph).await;
                    pattern.names.insert(
                        pattern.graph.edges[src][mph].1.name.clone(),
                        GraphId::Morphism(src, mph),
                    );
                }
            }
            for fce in 0..pattern.graph.faces.len() {
                pattern.graph.faces[fce].label.name =
                    pattern.name_compute_face(ctx, "Lem", fce).await;
                pattern.names.insert(
                    pattern.graph.faces[fce].label.name.clone(),
                    GraphId::Face(fce),
                );
            }
        }
    }

    pub async fn show_face(&mut self, fce: usize) {
        let pattern = &mut self.pattern.lock().await;
        if let Some(pattern) = pattern.as_mut() {
            VM::<Mock, ()>::show_face_impl(&mut pattern.graph, fce);
        }
    }

    pub async fn unshow_face(&mut self, fce: usize) {
        let pattern = &mut self.pattern.lock().await;
        if let Some(pattern) = pattern.as_mut() {
            VM::<Mock, ()>::unshow_face_impl(&mut pattern.graph, fce);
        }
    }

    pub async fn get_pattern<Rm: Remote>(&mut self, ctx: &mut Context<Rm>, cfg: &Config) {
        if self.pattern.lock().await.is_some() {
            return;
        }

        let graph = ctx
            .remote
            .pattern::<NodeLabel, EdgeLabel, FaceLabel>(self.id)
            .unwrap();
        ctx.set_lem_context(self.id);
        let graph = graph.prepare(ctx);
        let mut gstate = GraphState {
            graph,
            names: HashMap::new(),
            layout: LayoutEngine::new(),
            face_goal_order: Vec::new(),
            face_hyps_order: Vec::new(),
            selected_face: None,
        };
        let () = gstate.particles_for_graph(cfg);
        let () = gstate.init_face_order();
        *self.pattern.lock().await = Some(gstate);
        let () = self.relabel(ctx).await;
        let () = self.name(ctx).await;
        let () = self.recompute_face_statuses(ctx).await;
        let () = ctx.unset_lem_context();
    }

    pub async fn instantiate<Rm: Remote>(
        &mut self,
        ctx: &mut Context<Rm>,
        cfg: &Config,
        unselect: bool,
    ) -> GraphState {
        let () = self.get_pattern(ctx, cfg).await;

        let graph = ctx
            .remote
            .instantiate::<NodeLabel, EdgeLabel, FaceLabel>(self.id)
            .unwrap();
        let mut graph = graph.prepare(ctx);

        // Copy labels
        let pattern = self.pattern.lock().await;
        let pattern = pattern.as_ref().unwrap();
        for nd in 0..graph.nodes.len() {
            graph.nodes[nd].2 = pattern.graph.nodes[nd].2.clone();
        }
        for src in 0..graph.nodes.len() {
            for mph in 0..graph.edges[src].len() {
                graph.edges[src][mph].1 = pattern.graph.edges[src][mph].1.clone();
            }
        }
        for fce in 0..graph.faces.len() {
            graph.faces[fce].label = pattern.graph.faces[fce].label.clone();
        }

        // Unselect face
        if unselect {
            if let Some(fce) = pattern.selected_face {
                VM::<Mock, ()>::unshow_face_impl(&mut graph, fce);
            }
        }

        GraphState {
            graph,
            layout: pattern.layout.clone(),
            names: pattern.names.clone(),
            face_hyps_order: pattern.face_hyps_order.clone(),
            face_goal_order: pattern.face_goal_order.clone(),
            selected_face: if unselect { None } else { pattern.selected_face.clone() },
        }
    }
}
