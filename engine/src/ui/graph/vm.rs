use super::graph::{edge_label_pos, prepare_edge};
use super::graph::{Action, ArrowStyle, CurveStyle, Drawable, Modifier, UiGraph};
use super::graph::{FaceContent, FaceStyle, TextStyle};
use crate::graph::GraphId;
use crate::remote::Remote;
use crate::ui::vm::{InteractiveAction, VM};
use crate::vm::FaceStatus;
use egui::{Rect, Stroke, Style, Ui, Vec2};
use std::sync::Arc;
type CMR = crate::ui::vm::ContextMenuResult;

impl<Rm: Remote + Sync + Send> UiGraph for VM<Rm> {
    fn draw<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(Drawable<'a>, Stroke, Modifier, GraphId) -> Rect,
    {
        let mut stroke = style.noninteractive().fg_stroke;

        // Draw nodes
        let mut nodes_rect: Vec<Rect> = Vec::new();
        nodes_rect.reserve(self.graph.nodes.len());
        for nd in 0..self.graph.nodes.len() {
            if self.graph.nodes[nd].2.hidden {
                nodes_rect.push(Rect::NOTHING);
                continue;
            }

            if let Some(pos_id) = self.graph.nodes[nd].2.pos {
                let pos = self.layout.get_pos(pos_id);
                let drawable = Drawable::Text(
                    pos,
                    &self.graph.nodes[nd].2.label,
                    TextStyle {
                        underline: self.graph.nodes[nd].2.pinned,
                        ..TextStyle::new()
                    },
                );
                let mut modifier = if self.hovered_object == Some(GraphId::Node(nd)) {
                    Modifier::Highlight
                } else {
                    Modifier::None
                };
                let id = GraphId::Node(nd);

                if let Some((_, interactive)) = &self.current_action {
                    let md = interactive.modifier(self, GraphId::Node(nd));
                    crate::ui::vm::apply_modifier(md, &mut stroke.color, &mut modifier);
                }

                let rect = f(drawable, stroke, modifier, id);
                nodes_rect.push(rect);
                stroke.color = style.noninteractive().fg_stroke.color;
            }
        }

        // Draw edges
        for src in 0..self.graph.nodes.len() {
            for mph in 0..self.graph.edges[src].len() {
                if self.graph.edges[src][mph].1.hidden {
                    continue;
                }
                let dst = self.graph.edges[src][mph].0;

                let mut modifier = if self.hovered_object == Some(GraphId::Morphism(src, mph)) {
                    Modifier::Highlight
                } else {
                    Modifier::None
                };
                let id = GraphId::Morphism(src, mph);

                if let Some((_, interactive)) = &self.current_action {
                    let md = interactive.modifier(self, id);
                    crate::ui::vm::apply_modifier(md, &mut stroke.color, &mut modifier);
                }

                // Positions
                let psrc = self.layout.get_pos(self.graph.nodes[src].2.pos.unwrap());
                let pdst = self.layout.get_pos(self.graph.nodes[dst].2.pos.unwrap());
                let control = self
                    .layout
                    .get_pos(self.graph.edges[src][mph].1.control.unwrap());

                // Label
                f(
                    Drawable::Text(
                        edge_label_pos(psrc, pdst, control),
                        &self.graph.edges[src][mph].1.label,
                        TextStyle {
                            underline: self.graph.edges[src][mph].1.pinned,
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

                let stl = self.graph.edges[src][mph].1.style;
                stroke.color = if stl.left && stl.right {
                    egui::Color32::GOLD
                } else if stl.left {
                    egui::Color32::RED
                } else if stl.right {
                    egui::Color32::GREEN
                } else {
                    stroke.color
                };

                f(drawable, stroke, modifier, id);
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

            let mut border_color = match self.graph.faces[fce].label.status {
                FaceStatus::Goal => egui::Color32::GOLD,
                FaceStatus::Refined => egui::Color32::GREEN,
                FaceStatus::Hypothesis => {
                    if self.selected_face == Some(fce) {
                        style.noninteractive().fg_stroke.color
                    } else {
                        style.noninteractive().bg_stroke.color
                    }
                }
            };
            let mut md = if self.selected_face == Some(fce) {
                Modifier::Highlight
            } else {
                Modifier::None
            };
            if let Some((_, interactive)) = &self.current_action {
                let modifier = interactive.modifier(&self, id);
                crate::ui::vm::apply_modifier(modifier, &mut border_color, &mut md);
            }

            let border = Stroke {
                color: border_color,
                width: if md == Modifier::Highlight {
                    style.noninteractive().fg_stroke.width
                } else {
                    style.noninteractive().bg_stroke.width
                },
            };
            let (fill, text, sep) = if md == Modifier::Highlight {
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

    fn dragged<'a>(&'a mut self) -> &'a mut Option<GraphId> {
        &mut self.dragged_object
    }

    fn face_folded<'a>(&'a mut self, fce: usize) -> &'a mut bool {
        &mut self.graph.faces[fce].label.folded
    }

    fn action(&mut self, act: Action, ui: &mut Ui) {
        self.hovered_object = None;

        if let Some((last, mut interactive)) = self.current_action.take() {
            let r = interactive.action(self, act, ui);
            self.current_action = Some((last, interactive));
            if !r {
                return;
            }
        }

        match act {
            Action::Hover(id) => {
                self.hovered_object = Some(id);
                // Show tooltip
                egui::show_tooltip_at_pointer(ui.ctx(), egui::Id::new("Graph tooltip"), |ui| {
                    let label = match id {
                        GraphId::Node(n) => {
                            let node = &self.graph.nodes[n].2;
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
            Action::Drag(GraphId::Node(nd), pos, _) => {
                if let Some(part) = self.graph.nodes[nd].2.pos {
                    self.layout.set_pos(part, pos);
                }
            }
            Action::Drag(GraphId::Morphism(src, mph), _, vec) => {
                if let Some(part) = self.graph.edges[src][mph].1.control {
                    let pos = self.layout.get_pos(part) + vec;
                    self.layout.set_pos(part, pos);
                }
            }
            _ => (),
        }
    }

    fn context_menu(&mut self, on: GraphId, ui: &mut Ui) -> bool {
        if let Some((last, mut interactive)) = self.current_action.take() {
            let r = interactive.context_menu(self, on, ui);
            self.current_action = Some((last, interactive));
            if r == CMR::Closed {
                return false;
            }
        }
        match on {
            GraphId::Node(n) => {
                if ui.button("Merge with").clicked() {
                    let merge = InteractiveAction::merge(GraphId::Node(n));
                    self.start_interactive(merge);
                    ui.close_menu();
                    return false;
                }
                if ui
                    .button(if self.graph.nodes[n].2.pinned {
                        "Unpin"
                    } else {
                        "Pin"
                    })
                    .clicked()
                {
                    self.graph.nodes[n].2.pinned = !self.graph.nodes[n].2.pinned;
                    ui.close_menu();
                    return false;
                }
                true
            }
            GraphId::Morphism(src, dst) => {
                if ui.button("Split").clicked() {
                    self.insert_and_run(&format!("split {}", self.graph.edges[src][dst].1.name));
                    ui.close_menu();
                    return false;
                }
                if ui.button("Merge with").clicked() {
                    let merge = InteractiveAction::merge(GraphId::Morphism(src, dst));
                    self.start_interactive(merge);
                    ui.close_menu();
                    return false;
                }
                if ui
                    .button(if self.graph.edges[src][dst].1.pinned {
                        "Unpin"
                    } else {
                        "Pin"
                    })
                    .clicked()
                {
                    self.graph.edges[src][dst].1.pinned = !self.graph.edges[src][dst].1.pinned;
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
                    if ui.button("Decompose").clicked() {
                        self.planar_split(fce);
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
                    if ui.button("Merge with").clicked() {
                        let merge = InteractiveAction::merge(GraphId::Face(fce));
                        self.start_interactive(merge);
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
        }
    }
}
