use super::graph::{Action, ArrowStyle, CurveStyle, Drawable, Modifier, UiGraph};
use super::graph::{FaceContent, FaceStyle};
use crate::graph::GraphId;
use crate::vm::{FaceStatus, Lemma};
use egui::{Stroke, Style, Ui, Vec2};
use std::sync::Arc;

impl UiGraph for Lemma {
    fn draw<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(Drawable<'a>, Stroke, Modifier, GraphId),
    {
        let mut stroke = style.noninteractive().fg_stroke;

        if let Some(pattern) = &self.pattern {
            // Draw nodes
            for nd in 0..pattern.nodes.len() {
                if let Some(pos_id) = pattern.nodes[nd].2.pos {
                    // There will be no hidden nodes
                    let pos = self.graphical_state.layout.get_pos(pos_id);
                    let drawable = Drawable::Text(pos, &pattern.nodes[nd].2.label);
                    let modifier = if self.graphical_state.hovered == Some(GraphId::Node(nd)) {
                        Modifier::Highlight
                    } else {
                        Modifier::None
                    };
                    f(drawable, stroke, modifier, GraphId::Node(nd));
                }
            }

            // Draw edges
            for src in 0..pattern.nodes.len() {
                for mph in 0..pattern.edges[src].len() {
                    // There will be no hidden edges
                    let id = GraphId::Morphism(src, mph);
                    let modifier = if self.graphical_state.hovered == Some(id) {
                        Modifier::Highlight
                    } else {
                        Modifier::None
                    };

                    // Label
                    f(
                        Drawable::Text(
                            pattern.edges[src][mph].1.label_pos,
                            &pattern.edges[src][mph].1.label,
                        ),
                        stroke,
                        modifier,
                        id,
                    );

                    // Curve
                    for part in 0..pattern.edges[src][mph].1.shape.len() {
                        let arrow = if part == pattern.edges[src][mph].1.shape.len() - 1 {
                            ArrowStyle::Simple
                        } else {
                            ArrowStyle::None
                        };
                        let drawable = Drawable::Curve(
                            pattern.edges[src][mph].1.shape[part],
                            CurveStyle::Simple,
                            arrow,
                        );

                        let stl = pattern.edges[src][mph].1.style;
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
    }

    fn faces<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(GraphId, FaceContent<'a>, bool, FaceStyle),
    {
        if let Some(pattern) = &self.pattern {
            for fce in 0..pattern.faces.len() {
                // There won't be any hidden
                let id = GraphId::Face(fce);
                let content = FaceContent {
                    name: &pattern.faces[fce].label.name,
                    content: &pattern.faces[fce].label.label,
                };
                let folded = pattern.faces[fce].label.folded;

                let border_color = match pattern.faces[fce].label.status {
                    FaceStatus::Goal => egui::Color32::GOLD,
                    FaceStatus::Refined => egui::Color32::GREEN,
                    FaceStatus::Hypothesis => {
                        if self.graphical_state.selected_face == Some(fce) {
                            style.noninteractive().fg_stroke.color
                        } else {
                            style.noninteractive().bg_stroke.color
                        }
                    }
                };

                let style = if self.graphical_state.selected_face == Some(fce) {
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

    fn zoom<'a>(&'a mut self) -> &'a mut f32 {
        &mut self.graphical_state.zoom
    }

    fn offset<'a>(&'a mut self) -> &'a mut Vec2 {
        &mut self.graphical_state.offset
    }

    fn focused<'a>(&'a mut self) -> &'a mut Option<GraphId> {
        &mut self.graphical_state.focused
    }

    fn action(&mut self, act: Action, _ui: &mut Ui) {
        self.graphical_state.hovered = None;
        match act {
            Action::Hover(id) => self.graphical_state.hovered = Some(id),
            Action::Click(GraphId::Face(fce)) => {
                if self.graphical_state.selected_face != Some(fce) {
                    if let Some(prev) = self.graphical_state.selected_face {
                        self.unshow_face(prev)
                    }
                    self.graphical_state.selected_face = Some(fce);
                    self.show_face(fce);
                }
            }
            _ => (),
        }
    }

    // No menu
    fn context_menu(&mut self, on: GraphId, ui: &mut Ui) -> bool {
        if let Some(pattern) = &mut self.pattern {
            if let GraphId::Face(fce) = on {
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
            } else {
                ui.close_menu();
                false
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
