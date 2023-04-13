use crate::graph::GraphId;
use crate::ui::graph::graph::{Action, ArrowStyle, CurveStyle, Drawable, Modifier, UiGraph};
use crate::vm::Lemma;
use egui::{Stroke, Style, Ui, Vec2};
use std::sync::Arc;

impl UiGraph for Lemma {
    fn draw<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(Drawable<'a>, Stroke, Modifier, GraphId),
    {
        let mut stroke = style.noninteractive().fg_stroke;

        // Draw nodes
        for nd in 0..self.pattern.nodes.len() {
            // There will be no hidden nodes
            let drawable = Drawable::Text(
                self.pattern.nodes[nd].1.pos,
                &self.pattern.nodes[nd].1.label,
            );
            let modifier = if self.graphical_state.hovered == Some(GraphId::Node(nd)) {
                Modifier::Highlight
            } else {
                Modifier::None
            };
            f(drawable, stroke, modifier, GraphId::Node(nd));
        }

        // Draw edges
        for src in 0..self.pattern.nodes.len() {
            for mph in 0..self.pattern.edges[src].len() {
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
                        self.pattern.edges[src][mph].1.label_pos,
                        &self.pattern.edges[src][mph].1.label,
                    ),
                    stroke,
                    modifier,
                    id,
                );

                // Curve
                for part in 0..self.pattern.edges[src][mph].1.shape.len() {
                    let arrow = if part == self.pattern.edges[src][mph].1.shape.len() - 1 {
                        ArrowStyle::Simple
                    } else {
                        ArrowStyle::None
                    };
                    let drawable = Drawable::Curve(
                        self.pattern.edges[src][mph].1.shape[part],
                        CurveStyle::Simple,
                        arrow,
                    );

                    let stl = self.pattern.edges[src][mph].1.style;
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
        &mut self.graphical_state.zoom
    }

    fn offset<'a>(&'a mut self) -> &'a mut Vec2 {
        &mut self.graphical_state.offset
    }

    fn focused<'a>(&'a mut self) -> &'a mut Option<GraphId> {
        &mut self.graphical_state.focused
    }

    fn action(&mut self, act: Action, _ui: &mut Ui) {
        match act {
            Action::Hover(id) => self.graphical_state.hovered = Some(id),
            _ => self.graphical_state.hovered = None,
        }
    }

    // No menu
    fn context_menu(&mut self, _on: GraphId, ui: &mut Ui) -> bool {
        ui.close_menu();
        false
    }
}
