use crate::graph::GraphId;
use crate::ui::graph::graph::{Action, ArrowStyle, CurveStyle, Drawable, Modifier, UiGraph};
use crate::vm::VM;
use egui::{Stroke, Style, Ui, Vec2};
use std::sync::Arc;

impl UiGraph for VM {
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

    fn zoom<'a>(&'a mut self) -> &'a mut f32 {
        &mut self.zoom
    }

    fn offset<'a>(&'a mut self) -> &'a mut Vec2 {
        &mut self.offset
    }

    fn focused<'a>(&'a mut self) -> &'a mut Option<GraphId> {
        &mut self.focused_object
    }

    fn action(&mut self, act: Action, ui: &mut Ui) {
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
                        _ => panic!(),
                    };
                    ui.label(label)
                });
            }
            _ => self.hovered_object = None,
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
            _ => {
                ui.close_menu();
                false
            }
        }
    }
}
