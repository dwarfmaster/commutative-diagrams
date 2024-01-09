use super::graph::{edge_label_pos, prepare_edge};
use super::graph::{Action, ArrowStyle, CurveStyle, Drawable, Modifier, UiGraph};
use super::graph::{FaceContent, FaceStyle, TextStyle};
use crate::graph::GraphId;
use crate::runtime::Runtime;
use crate::vm::{FaceStatus, Lemma};
use egui::{Rect, Stroke, Style, Ui, Vec2};
use std::sync::Arc;
use std::ops::DerefMut;

impl<RT: Runtime> UiGraph<RT> for Lemma {
    fn draw<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(Drawable<'a>, Stroke, Modifier, GraphId) -> Rect,
    {
        let mut stroke = style.noninteractive().fg_stroke;

        if let Some(pattern) = self.pattern.try_lock() {
            if let Some(pattern) = pattern.as_ref() {
                // Draw nodes
                let mut nodes_rect: Vec<Rect> = Vec::new();
                nodes_rect.reserve(pattern.graph.nodes.len());
                for nd in 0..pattern.graph.nodes.len() {
                    if let Some(pos_id) = pattern.graph.nodes[nd].2.pos {
                        // There will be no hidden nodes
                        let pos = pattern.layout.get_pos(pos_id);
                        let drawable = Drawable::Text(
                            pos,
                            &pattern.graph.nodes[nd].2.label,
                            TextStyle {
                                underline: pattern.graph.nodes[nd].2.pinned,
                                ..TextStyle::new()
                            },
                        );
                        let modifier = if self.graphical.hovered == Some(GraphId::Node(nd)) {
                            Modifier::Highlight
                        } else {
                            Modifier::None
                        };
                        let rect = f(drawable, stroke, modifier, GraphId::Node(nd));
                        nodes_rect.push(rect);
                    }
                }

                // Draw edges
                for src in 0..pattern.graph.nodes.len() {
                    for mph in 0..pattern.graph.edges[src].len() {
                        // There will be no hidden edges
                        let dst = pattern.graph.edges[src][mph].0;
                        let id = GraphId::Morphism(src, mph);
                        let modifier = if self.graphical.hovered == Some(id) {
                            Modifier::Highlight
                        } else {
                            Modifier::None
                        };

                        // Positions
                        let psrc = pattern
                            .layout
                            .get_pos(pattern.graph.nodes[src].2.pos.unwrap());
                        let pdst = pattern
                            .layout
                            .get_pos(pattern.graph.nodes[dst].2.pos.unwrap());
                        let control = pattern
                            .layout
                            .get_pos(pattern.graph.edges[src][mph].1.control.unwrap());

                        // Label
                        f(
                            Drawable::Text(
                                edge_label_pos(psrc, pdst, control),
                                &pattern.graph.edges[src][mph].1.label,
                                TextStyle {
                                    underline: pattern.graph.edges[src][mph].1.pinned,
                                    ..TextStyle::new()
                                },
                            ),
                            stroke,
                            modifier,
                            id,
                        );

                        // Curve
                        let arrow = ArrowStyle::Simple;
                        let curve = prepare_edge(psrc, nodes_rect[src], control, pdst, nodes_rect[dst]);
                        let drawable = Drawable::Curve(curve, CurveStyle::Simple, arrow);

                        let stl = pattern.graph.edges[src][mph].1.style;
                        stroke.color = if stl.left && stl.right {
                            egui::Color32::GOLD
                        } else if stl.left {
                            egui::Color32::RED
                        } else if stl.right {
                            egui::Color32::GREEN
                        } else {
                            style.noninteractive().fg_stroke.color
                        };

                        f(drawable, stroke, modifier, id);
                        stroke.color = style.noninteractive().fg_stroke.color
                    }
                }
            }
        }
    }

    fn faces<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(GraphId, FaceContent<'a>, bool, FaceStyle),
    {
        if let Some(pattern) = self.pattern.try_lock() {
            if let Some(pattern) = pattern.as_ref() {
                for fce in 0..pattern.graph.faces.len() {
                    // There won't be any hidden
                    let id = GraphId::Face(fce);
                    let content = FaceContent {
                        name: &pattern.graph.faces[fce].label.name,
                        content: &pattern.graph.faces[fce].label.label,
                    };
                    let folded = pattern.graph.faces[fce].label.folded;

                    let border_color = match pattern.graph.faces[fce].label.status {
                        FaceStatus::Goal => egui::Color32::GOLD,
                        FaceStatus::Refined => egui::Color32::GREEN,
                        FaceStatus::Hypothesis => {
                            if pattern.selected_face == Some(fce) {
                                style.noninteractive().fg_stroke.color
                            } else {
                                style.noninteractive().bg_stroke.color
                            }
                        }
                    };

                    let style = if pattern.selected_face == Some(fce) {
                        FaceStyle {
                            border: Stroke {
                                color: border_color,
                                ..style.noninteractive().fg_stroke
                            },
                            sep: style.noninteractive().fg_stroke,
                            fill: style.visuals.widgets.active.bg_fill,
                            text: style.visuals.widgets.active.fg_stroke.color,
                        }
                    } else {
                        FaceStyle {
                            border: Stroke {
                                color: border_color,
                                ..style.noninteractive().fg_stroke
                            },
                            sep: style.noninteractive().bg_stroke,
                            fill: style.noninteractive().bg_fill,
                            text: style.noninteractive().fg_stroke.color,
                        }
                    };

                    f(id, content, folded, style);
                }
            }
        }
    }

    fn zoom<'a>(&'a mut self) -> &'a mut f32 {
        &mut self.graphical.zoom
    }

    fn offset<'a>(&'a mut self) -> &'a mut Vec2 {
        &mut self.graphical.offset
    }

    fn focused<'a>(&'a mut self) -> &'a mut Option<GraphId> {
        &mut self.graphical.focused
    }

    fn dragged<'a>(&'a mut self) -> &'a mut Option<GraphId> {
        &mut self.graphical.dragged
    }

    fn action(&mut self, act: Action, _ui: &mut Ui, rt: &mut RT) {
        self.graphical.hovered = None;
        match act {
            Action::Hover(id) => self.graphical.hovered = Some(id),
            _ => (),
        }
        let running = rt.running();
        if running.is_none() {
            match act {
                Action::Click(GraphId::Face(fce)) => {
                    let index = self.index;
                    rt.run("Select face", move |vm| async {
                        let lemmas = vm.lemmas.lock().await;
                        let lem = &mut lemmas.lemmas[index];
                        if let Some(pattern) = lem.pattern.lock().await.deref_mut() {
                            if pattern.selected_face != Some(fce) {
                                if let Some(prev) = pattern.selected_face {
                                    lem.unshow_face(prev).await
                                }
                                pattern.selected_face = Some(fce);
                                lem.show_face(fce).await;
                            }
                        }
                    });
                }
                Action::Drag(GraphId::Node(nd), pos, _) => {
                    if let Some(pattern) = &self.pattern {
                        if let Some(part) = pattern.nodes[nd].2.pos {
                            self.graphical_state.layout.set_pos(part, pos);
                        }
                    }
                }
                Action::Drag(GraphId::Morphism(src, mph), _, vec) => {
                    if let Some(pattern) = &self.pattern {
                        if let Some(part) = pattern.edges[src][mph].1.control {
                            let pos = self.graphical_state.layout.get_pos(part) + vec;
                            self.graphical_state.layout.set_pos(part, pos);
                        }
                    }
                }
                _ => (),
            }
        }
    }

    // No menu
    fn context_menu(&mut self, on: GraphId, ui: &mut Ui, _rt: &mut RT) -> bool {
        if let Some(pattern) = &mut self.pattern {
            match on {
                GraphId::Node(n) => {
                    if ui
                        .button(if pattern.nodes[n].2.pinned {
                            "Unpin"
                        } else {
                            "Pin"
                        })
                        .clicked()
                    {
                        pattern.nodes[n].2.pinned = !pattern.nodes[n].2.pinned;
                        ui.close_menu();
                        return false;
                    }
                    true
                }
                GraphId::Morphism(src, mph) => {
                    if ui
                        .button(if pattern.edges[src][mph].1.pinned {
                            "Unpin"
                        } else {
                            "Pin"
                        })
                        .clicked()
                    {
                        pattern.edges[src][mph].1.pinned = !pattern.edges[src][mph].1.pinned;
                        ui.close_menu();
                        return false;
                    }
                    true
                }
                GraphId::Face(fce) => {
                    if pattern.faces[fce].label.folded {
                        if ui.button("Show term").clicked() {
                            pattern.faces[fce].label.folded = false;
                            ui.close_menu();
                            return false;
                        }
                    } else {
                        if ui.button("Hide term").clicked() {
                            pattern.faces[fce].label.folded = true;
                            ui.close_menu();
                            return false;
                        }
                    }
                    true
                }
            }
        } else {
            ui.close_menu();
            false
        }
    }

    fn face_folded<'a>(&'a mut self, fce: usize) -> &'a mut bool {
        &mut self.pattern.as_mut().unwrap().faces[fce].label.folded
    }
}
