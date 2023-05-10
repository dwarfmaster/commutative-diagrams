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
type CMR = super::ContextMenuResult;

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

fn same_nature(id1: GraphId, id2: GraphId) -> bool {
    use GraphId::*;
    match (id1, id2) {
        (Node(..), Node(..)) => true,
        (Morphism(..), Morphism(..)) => true,
        (Face(..), Face(..)) => true,
        _ => false,
    }
}

struct DisplayState<'a> {
    apply: &'a mut LemmaApplicationState,
    vm: &'a mut VM,
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
        let mut state = DisplayState { apply: self, vm };
        egui::Window::new(format!(
            "Applying {}",
            state.vm.lemmas[state.apply.lemma].name
        ))
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
                ui.add(widget::graph(&mut state))
            })
        });

        open && !should_close
    }

    pub fn context_menu(&mut self, vm: &mut VM, on: GraphId, ui: &mut Ui) -> CMR {
        if let Some(AppId::Lemma(id)) = &self.selected {
            if same_nature(*id, on) {
                if ui.button("Match").clicked() {
                    self.do_match(vm, *id, on);
                    ui.close_menu();
                    return CMR::Closed;
                } else {
                    return CMR::Added;
                }
            }
        }
        CMR::Nothing
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

    fn do_match(&mut self, vm: &mut VM, lem: GraphId, goal: GraphId) {
        // TODO
    }
}

impl<'vm> UiGraph for DisplayState<'vm> {
    fn draw<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(Drawable<'a>, Stroke, Modifier, GraphId),
    {
        let mut stroke = style.noninteractive().fg_stroke;

        // Draw nodes
        for nd in 0..self.apply.graph.nodes.len() {
            let drawable = Drawable::Text(
                self.apply.graph.nodes[nd].1.pos,
                &self.apply.graph.nodes[nd].1.label,
            );
            let mut modifier = if self.apply.hovered == Some(GraphId::Node(nd)) {
                Modifier::Highlight
            } else {
                Modifier::None
            };
            let md = self.apply.self_modifier(GraphId::Node(nd));
            super::apply_modifier(md, &mut stroke.color, &mut modifier);

            f(drawable, stroke, modifier, GraphId::Node(nd));
            stroke.color = style.noninteractive().fg_stroke.color;
        }

        // Draw edges
        for src in 0..self.apply.graph.nodes.len() {
            for mph in 0..self.apply.graph.edges[src].len() {
                let id = GraphId::Morphism(src, mph);
                let mut modifier = if self.apply.hovered == Some(id) {
                    Modifier::Highlight
                } else {
                    Modifier::None
                };
                let md = self.apply.self_modifier(id);
                super::apply_modifier(md, &mut stroke.color, &mut modifier);

                // label
                f(
                    Drawable::Text(
                        self.apply.graph.edges[src][mph].1.label_pos,
                        &self.apply.graph.edges[src][mph].1.label,
                    ),
                    stroke,
                    modifier,
                    id,
                );

                // Curve
                for part in 0..self.apply.graph.edges[src][mph].1.shape.len() {
                    let arrow = if part == self.apply.graph.edges[src][mph].1.shape.len() - 1 {
                        ArrowStyle::Simple
                    } else {
                        ArrowStyle::None
                    };
                    let drawable = Drawable::Curve(
                        self.apply.graph.edges[src][mph].1.shape[part],
                        CurveStyle::Simple,
                        arrow,
                    );

                    let stl = self.apply.graph.edges[src][mph].1.style;
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
        for fce in 0..self.apply.graph.faces.len() {
            let id = GraphId::Face(fce);
            let content = FaceContent {
                name: &self.apply.graph.faces[fce].label.name,
                content: &self.apply.graph.faces[fce].label.label,
            };
            let folded = self.apply.graph.faces[fce].label.folded;

            let mut border = style.noninteractive().bg_stroke;
            let mut modifier = Modifier::None;
            let md = self.apply.self_modifier(id);
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
        &mut self.apply.zoom
    }

    fn offset<'a>(&'a mut self) -> &'a mut Vec2 {
        &mut self.apply.offset
    }

    fn focused<'a>(&'a mut self) -> &'a mut Option<GraphId> {
        &mut self.apply.focused
    }

    fn face_folded<'a>(&'a mut self, fce: usize) -> &'a mut bool {
        &mut self.apply.graph.faces[fce].label.folded
    }

    fn action(&mut self, act: Action, _ui: &mut Ui) {
        self.apply.hovered = None;
        match act {
            Action::Hover(id) => self.apply.hovered = Some(id),
            Action::Click(id) => {
                if self.apply.selected != Some(AppId::Lemma(id)) {
                    if let Some(AppId::Lemma(GraphId::Face(prev))) = self.apply.selected {
                        self.apply.unshow_face(prev)
                    }
                    self.apply.selected = Some(AppId::Lemma(id));
                    if let GraphId::Face(fce) = id {
                        self.apply.show_face(fce);
                    }
                }
            }
            _ => (),
        }
    }

    fn context_menu(&mut self, on: GraphId, ui: &mut Ui) -> bool {
        let mut r = CMR::Nothing;
        if let Some(AppId::Goal(id)) = self.apply.selected {
            if same_nature(id, on) {
                if ui.button("Match").clicked() {
                    self.apply.do_match(self.vm, on, id);
                    ui.close_menu();
                    return false;
                } else {
                    r = CMR::Added;
                }
            }
        }

        if let GraphId::Face(fce) = on {
            if self.apply.graph.faces[fce].label.folded {
                if ui.button("Show term").clicked() {
                    self.apply.graph.faces[fce].label.folded = false;
                    ui.close_menu();
                    return false;
                }
            } else {
                if ui.button("Hide term").clicked() {
                    self.apply.graph.faces[fce].label.folded = true;
                    ui.close_menu();
                    return false;
                }
            }
            true
        } else if r == CMR::Nothing {
            ui.close_menu();
            false
        } else {
            true
        }
    }
}
