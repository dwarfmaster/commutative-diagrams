use super::graph::{Action, ArrowStyle, CurveStyle, Drawable, Modifier, UiGraph};
use super::graph::{FaceContent, FaceStyle};
use crate::graph::GraphId;
use crate::vm::{FaceStatus, Interactive, VM};
use egui::{Stroke, Style, Ui, Vec2};
use std::sync::Arc;

impl<I: Interactive + Sync + Send> UiGraph for VM<I> {
    fn draw<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(Drawable<'a>, Stroke, Modifier, GraphId),
    {
        let mut stroke = style.noninteractive().fg_stroke;

        // Draw nodes
        for nd in 0..self.graph.nodes.len() {
            if self.graph.nodes[nd].1.hidden {
                continue;
            }

            let drawable =
                Drawable::Text(self.graph.nodes[nd].1.pos, &self.graph.nodes[nd].1.label);
            let modifier = if self.hovered_object == Some(GraphId::Node(nd)) {
                Modifier::Highlight
            } else {
                Modifier::None
            };
            let id = GraphId::Node(nd);
            f(drawable, stroke, modifier, id);
        }

        // Draw edges
        for src in 0..self.graph.nodes.len() {
            for mph in 0..self.graph.edges[src].len() {
                if self.graph.edges[src][mph].1.hidden {
                    continue;
                }

                let modifier = if self.hovered_object == Some(GraphId::Morphism(src, mph)) {
                    Modifier::Highlight
                } else {
                    Modifier::None
                };
                let id = GraphId::Morphism(src, mph);

                // Label
                f(
                    Drawable::Text(
                        self.graph.edges[src][mph].1.label_pos,
                        &self.graph.edges[src][mph].1.label,
                    ),
                    stroke,
                    modifier,
                    id,
                );

                // Curve
                for part in 0..self.graph.edges[src][mph].1.shape.len() {
                    let arrow = if part == self.graph.edges[src][mph].1.shape.len() - 1 {
                        ArrowStyle::Simple
                    } else {
                        ArrowStyle::None
                    };
                    let drawable = Drawable::Curve(
                        self.graph.edges[src][mph].1.shape[part],
                        CurveStyle::Simple,
                        arrow,
                    );

                    let stl = self.graph.edges[src][mph].1.style;
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
                }
                stroke.color = style.noninteractive().fg_stroke.color
            }
        }
    }

    fn faces<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(GraphId, FaceContent<'a>, bool, FaceStyle),
    {
        let len = self.face_goal_order.len() + self.face_hyps_order.len();
        for id in 0..len {
            let fce = if id >= self.face_goal_order.len() {
                self.face_hyps_order[id - self.face_goal_order.len()]
            } else {
                self.face_goal_order[id]
            };

            if self.graph.faces[fce].label.hidden {
                continue;
            }
            let id = GraphId::Face(fce);

            let content = FaceContent {
                name: &self.graph.faces[fce].label.name,
                content: &self.graph.faces[fce].label.label,
            };

            let folded = self.graph.faces[fce].label.folded;

            let stroke = style.noninteractive().bg_stroke;
            let border = match self.graph.faces[fce].label.status {
                FaceStatus::Goal => Stroke {
                    color: egui::Color32::GOLD,
                    ..stroke
                },
                FaceStatus::Refined => Stroke {
                    color: egui::Color32::GREEN,
                    ..stroke
                },
                FaceStatus::Hypothesis => {
                    if self.selected_face == Some(fce) {
                        style.noninteractive().fg_stroke
                    } else {
                        stroke
                    }
                }
            };
            let (fill, text, sep) = if self.selected_face == Some(fce) {
                (
                    style.visuals.widgets.active.bg_fill,
                    style.visuals.widgets.active.fg_stroke.color,
                    style.noninteractive().fg_stroke,
                )
            } else {
                (
                    style.visuals.noninteractive().bg_fill,
                    style.noninteractive().fg_stroke.color,
                    style.noninteractive().bg_stroke,
                )
            };
            let style = FaceStyle {
                border,
                fill,
                sep,
                text,
            };

            // Do the drawing
            f(id, content, folded, style);
        }
    }

    fn zoom<'a>(&'a mut self) -> &'a mut f32 {
        &mut self.zoom
    }

    fn offset<'a>(&'a mut self) -> &'a mut Vec2 {
        &mut self.offset
    }

    fn focused<'a>(&'a mut self) -> &'a mut Option<GraphId> {
        &mut self.focused_object
    }

    fn face_folded<'a>(&'a mut self, fce: usize) -> &'a mut bool {
        &mut self.graph.faces[fce].label.folded
    }

    fn action(&mut self, act: Action, ui: &mut Ui) {
        self.hovered_object = None;
        match act {
            Action::Hover(id) => {
                self.hovered_object = Some(id);
                // Show tooltip
                egui::show_tooltip_at_pointer(ui.ctx(), egui::Id::new("Graph tooltip"), |ui| {
                    let label = match id {
                        GraphId::Node(n) => {
                            let node = &self.graph.nodes[n].1;
                            format!("node {}: {}", node.name, node.label)
                        }
                        GraphId::Morphism(src, dst) => {
                            let edge = &self.graph.edges[src][dst].1;
                            format!("morphism {}: {}", edge.name, edge.label)
                        }
                        GraphId::Face(fce) => {
                            let face = &self.graph.faces[fce].label;
                            format!("face {}", face.name)
                        }
                    };
                    ui.label(label)
                });
            }
            Action::Click(GraphId::Face(fce)) => {
                if self.selected_face != Some(fce) {
                    if let Some(prev) = self.selected_face {
                        self.unshow_face(prev);
                    }
                    self.selected_face = Some(fce);
                    self.show_face(fce);
                }
            }
            Action::DoubleClick(GraphId::Face(fce)) => {
                self.insert_and_run(&format!("solve {}", self.graph.faces[fce].label.name))
            }
            _ => (),
        }
    }

    fn context_menu(&mut self, on: GraphId, ui: &mut Ui) -> bool {
        match on {
            GraphId::Morphism(src, dst) => {
                if ui.button("Split").clicked() {
                    self.insert_and_run(&format!("split {}", self.graph.edges[src][dst].1.name));
                    ui.close_menu();
                    return false;
                }
                true
            }
            GraphId::Face(fce) => {
                if self.graph.faces[fce].label.status == FaceStatus::Goal {
                    if ui.button("Solve").clicked() {
                        self.insert_and_run(&format!("solve {}", self.graph.faces[fce].label.name));
                        ui.close_menu();
                        return false;
                    }
                    if ui.button("Shrink").clicked() {
                        self.insert_and_run(&format!(
                            "shrink {}",
                            self.graph.faces[fce].label.name
                        ));
                        ui.close_menu();
                        return false;
                    }
                    if ui.button("Pull").clicked() {
                        self.insert_and_run(&format!(
                            "pull {}, *",
                            self.graph.faces[fce].label.name
                        ));
                        ui.close_menu();
                        return false;
                    }
                    if ui.button("Push").clicked() {
                        self.insert_and_run(&format!(
                            "push {}, *",
                            self.graph.faces[fce].label.name
                        ));
                        ui.close_menu();
                        return false;
                    }
                }
                if self.graph.faces[fce].label.folded {
                    if ui.button("Show term").clicked() {
                        self.graph.faces[fce].label.folded = false;
                        ui.close_menu();
                        return false;
                    }
                } else {
                    if ui.button("Hide term").clicked() {
                        self.graph.faces[fce].label.folded = true;
                        ui.close_menu();
                        return false;
                    }
                }
                true
            }
            _ => {
                ui.close_menu();
                false
            }
        }
    }
}
