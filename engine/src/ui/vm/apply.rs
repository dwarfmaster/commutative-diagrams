use crate::graph::{Graph, GraphId};
use crate::ui::graph::graph::{Action, Drawable, FaceContent, UiGraph};
use crate::ui::graph::graph::{ArrowStyle, CurveStyle, FaceStyle, Modifier};
use crate::ui::graph::widget;
use crate::ui::VM;
use crate::vm::{EdgeLabel, FaceLabel, NodeLabel};
use egui::{Context, Stroke, Style, Ui, Vec2};
use std::collections::HashMap;
use std::sync::Arc;

type Mod = crate::ui::vm::Modifier;

#[derive(Clone, Debug, PartialEq, Eq)]
enum AppId {
    Lemma(GraphId),
    Goal(GraphId),
}

#[derive(Clone, Debug)]
pub struct LemmaApplicationState {
    lemma: usize,
    graph: Graph<NodeLabel, EdgeLabel, FaceLabel>,
    // The mapping is considered to be lemma -> state graph
    direct_mapping: HashMap<GraphId, Vec<GraphId>>,
    reverse_mapping: HashMap<GraphId, Vec<GraphId>>,

    // Action state
    selected: Option<AppId>,

    // Graphical state
    zoom: f32,
    offset: Vec2,
    focused: Option<GraphId>,
    hovered: Option<GraphId>,
}

impl LemmaApplicationState {
    pub fn new(vm: &VM, lemma: usize) -> Self {
        Self {
            lemma,
            graph: vm.lemmas[lemma].pattern.clone(),
            direct_mapping: HashMap::new(),
            selected: None,
            reverse_mapping: HashMap::new(),
            zoom: 1.0,
            offset: Vec2::ZERO,
            focused: None,
            hovered: None,
        }
    }

    pub fn display(&mut self, vm: &mut VM, ui: &Context) -> bool {
        let mut open = true;
        let mut should_close = false;
        egui::Window::new(format!("Applying {}", vm.lemmas[self.lemma].name))
            .id(egui::Id::new("Apply lemma"))
            .open(&mut open)
            .show(ui, |ui| {
                ui.with_layout(egui::Layout::bottom_up(egui::Align::RIGHT), |ui| {
                    ui.allocate_ui_with_layout(
                        Vec2::new(100.0, 40.0),
                        egui::Layout::right_to_left(egui::Align::Center),
                        |ui| {
                            if ui.button("Apply").clicked() {
                                should_close = true;
                                // TODO commit
                            };
                            if ui.button("Cancel").clicked() {
                                should_close = true;
                            }
                        },
                    );
                    ui.add(widget::graph(self))
                })
            });

        open && !should_close
    }

    pub fn context_menu(&mut self, _vm: &mut VM, _on: GraphId, _ui: &mut Ui) -> bool {
        // No context menu
        true
    }

    pub fn action(&mut self, vm: &mut VM, act: Action, _ui: &mut Ui) -> bool {
        match act {
            Action::Click(id) => {
                if let Some(AppId::Lemma(GraphId::Face(prev))) = self.selected {
                    self.unshow_face(prev);
                }
                vm.deselect_face();
                self.selected = Some(AppId::Goal(id));
                if let GraphId::Face(fce) = id {
                    vm.select_face(fce);
                }
                false
            }
            _ => true,
        }
    }

    pub fn modifier(&self, _vm: &VM, on: GraphId) -> Mod {
        Mod {
            active: self.reverse_mapping.contains_key(&on),
            selected: self.selected == Some(AppId::Goal(on)),
            candidate: false,
        }
    }

    pub fn self_modifier(&self, on: GraphId) -> Mod {
        Mod {
            active: self.direct_mapping.contains_key(&on),
            selected: self.selected == Some(AppId::Lemma(on)),
            candidate: false,
        }
    }

    fn show_face(&mut self, fce: usize) {
        VM::show_face_impl(&mut self.graph, fce);
    }

    fn unshow_face(&mut self, fce: usize) {
        VM::unshow_face_impl(&mut self.graph, fce);
    }
}

impl UiGraph for LemmaApplicationState {
    fn draw<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(Drawable<'a>, Stroke, Modifier, GraphId),
    {
        let mut stroke = style.noninteractive().fg_stroke;

        // Draw nodes
        for nd in 0..self.graph.nodes.len() {
            let drawable =
                Drawable::Text(self.graph.nodes[nd].1.pos, &self.graph.nodes[nd].1.label);
            let mut modifier = if self.hovered == Some(GraphId::Node(nd)) {
                Modifier::Highlight
            } else {
                Modifier::None
            };
            let md = self.self_modifier(GraphId::Node(nd));
            super::apply_modifier(md, &mut stroke.color, &mut modifier);

            f(drawable, stroke, modifier, GraphId::Node(nd));
            stroke.color = style.noninteractive().fg_stroke.color;
        }

        // Draw edges
        for src in 0..self.graph.nodes.len() {
            for mph in 0..self.graph.edges[src].len() {
                let id = GraphId::Morphism(src, mph);
                let mut modifier = if self.hovered == Some(id) {
                    Modifier::Highlight
                } else {
                    Modifier::None
                };
                let md = self.self_modifier(id);
                super::apply_modifier(md, &mut stroke.color, &mut modifier);

                // label
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
                        stroke.color
                    };

                    f(drawable, stroke, modifier, id);
                }
                stroke.color = style.noninteractive().fg_stroke.color;
            }
        }
    }

    fn faces<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(GraphId, FaceContent<'a>, bool, FaceStyle),
    {
        for fce in 0..self.graph.faces.len() {
            let id = GraphId::Face(fce);
            let content = FaceContent {
                name: &self.graph.faces[fce].label.name,
                content: &self.graph.faces[fce].label.label,
            };
            let folded = self.graph.faces[fce].label.folded;

            let mut border = style.noninteractive().bg_stroke;
            let mut modifier = Modifier::None;
            let md = self.self_modifier(id);
            super::apply_modifier(md, &mut border.color, &mut modifier);

            let style = if modifier == Modifier::Highlight {
                FaceStyle {
                    border,
                    sep: style.noninteractive().fg_stroke,
                    fill: style.visuals.widgets.active.bg_fill,
                    text: style.visuals.widgets.active.fg_stroke.color,
                }
            } else {
                FaceStyle {
                    border,
                    sep: style.noninteractive().bg_stroke,
                    fill: style.noninteractive().bg_fill,
                    text: style.noninteractive().fg_stroke.color,
                }
            };

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
        &mut self.focused
    }

    fn face_folded<'a>(&'a mut self, fce: usize) -> &'a mut bool {
        &mut self.graph.faces[fce].label.folded
    }

    fn action(&mut self, act: Action, _ui: &mut Ui) {
        self.hovered = None;
        match act {
            Action::Hover(id) => self.hovered = Some(id),
            Action::Click(id) => {
                if self.selected != Some(AppId::Lemma(id)) {
                    if let Some(AppId::Lemma(GraphId::Face(prev))) = self.selected {
                        self.unshow_face(prev)
                    }
                    self.selected = Some(AppId::Lemma(id));
                    if let GraphId::Face(fce) = id {
                        self.show_face(fce);
                    }
                }
            }
            _ => (),
        }
    }

    fn context_menu(&mut self, on: GraphId, ui: &mut Ui) -> bool {
        if let GraphId::Face(fce) = on {
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
        } else {
            ui.close_menu();
            false
        }
    }
}
