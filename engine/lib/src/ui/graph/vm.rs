use super::graph::{edge_label_pos, prepare_edge};
use super::graph::{Action, ArrowStyle, CurveStyle, Drawable, Modifier, UiGraph};
use super::graph::{FaceContent, FaceStyle, TextStyle};
use crate::graph::GraphId;
use crate::runtime::Runtime;
use crate::ui::vm::{InteractiveAction, VM};
use crate::vm::FaceStatus;
use egui::{Rect, Stroke, Style, Ui, Vec2};
use std::sync::Arc;
use std::ops::Deref;
type CMR = crate::ui::vm::ContextMenuResult;

impl<RT: Runtime> UiGraph<RT> for VM<RT::Rem> {
    fn draw<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(Drawable<'a>, Stroke, Modifier, GraphId) -> Rect,
    {
        if let Some(graph) = self.graph.try_lock() {
            let mut stroke = style.noninteractive().fg_stroke;

            // Draw nodes
            let mut nodes_rect: Vec<Rect> = Vec::new();
            nodes_rect.reserve(graph.graph.nodes.len());
            for nd in 0..graph.graph.nodes.len() {
                if graph.graph.nodes[nd].2.hidden {
                    nodes_rect.push(Rect::NOTHING);
                    continue;
                }

                if let Some(pos_id) = graph.graph.nodes[nd].2.pos {
                    let pos = graph.layout.get_pos(pos_id);
                    let drawable = Drawable::Text(
                        pos,
                        &graph.graph.nodes[nd].2.label,
                        TextStyle {
                            underline: graph.graph.nodes[nd].2.pinned,
                            ..TextStyle::new()
                        },
                    );
                    let mut modifier = if self.graphical.hovered == Some(GraphId::Node(nd)) {
                        Modifier::Highlight
                    } else {
                        Modifier::None
                    };
                    let id = GraphId::Node(nd);

                    if let Some(interactive) = self.current_action.try_lock() {
                        if let Some((_, interactive)) = interactive.deref() {
                            let md = interactive.modifier(self, GraphId::Node(nd));
                            crate::ui::vm::apply_modifier(md, &mut stroke.color, &mut modifier);
                        }
                    }

                    let rect = f(drawable, stroke, modifier, id);
                    nodes_rect.push(rect);
                    stroke.color = style.noninteractive().fg_stroke.color;
                }
            }

            // Draw edges
            for src in 0..graph.graph.nodes.len() {
                for mph in 0..graph.graph.edges[src].len() {
                    if graph.graph.edges[src][mph].1.hidden {
                        continue;
                    }
                    let dst = graph.graph.edges[src][mph].0;

                    let mut modifier =
                        if self.graphical.hovered == Some(GraphId::Morphism(src, mph)) {
                            Modifier::Highlight
                        } else {
                            Modifier::None
                        };
                    let id = GraphId::Morphism(src, mph);

                    if let Some(interactive) = self.current_action.try_lock() {
                        if let Some((_, interactive)) = interactive.deref() {
                            let md = interactive.modifier(self, id);
                            crate::ui::vm::apply_modifier(md, &mut stroke.color, &mut modifier);
                        }
                    }

                    // Positions
                    let psrc = 
                        graph
                        .layout
                        .get_pos(graph.graph.nodes[src].2.pos.unwrap());
                    let pdst = 
                        graph
                        .layout
                        .get_pos(graph.graph.nodes[dst].2.pos.unwrap());
                    let control = 
                        graph
                        .layout
                        .get_pos(graph.graph.edges[src][mph].1.control.unwrap());

                    // Label
                    f(
                        Drawable::Text(
                            edge_label_pos(psrc, pdst, control),
                            &graph.graph.edges[src][mph].1.label,
                            TextStyle {
                                underline: graph.graph.edges[src][mph].1.pinned,
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

                    let stl = graph.graph.edges[src][mph].1.style;
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
    }

    fn faces<'a, F>(&'a self, style: &Arc<Style>, mut f: F)
    where
        F: FnMut(GraphId, FaceContent<'a>, bool, FaceStyle),
    {
        if let Some(graph) = self.graph.try_lock() {
            let len = graph.face_goal_order.len() + graph.face_hyps_order.len();
            for id in 0..len {
                let fce = if id >= graph.face_goal_order.len() {
                    graph.face_hyps_order[id - graph.face_goal_order.len()]
                } else {
                    graph.face_goal_order[id]
                };

                if graph.graph.faces[fce].label.hidden {
                    continue;
                }
                let id = GraphId::Face(fce);

                let content = FaceContent {
                    name: &graph.graph.faces[fce].label.name,
                    content: &graph.graph.faces[fce].label.label,
                };

                let folded = graph.graph.faces[fce].label.folded;

                let mut border_color = match graph.graph.faces[fce].label.status {
                    FaceStatus::Goal => egui::Color32::GOLD,
                    FaceStatus::Refined => egui::Color32::GREEN,
                    FaceStatus::Hypothesis => {
                        if graph.selected_face == Some(fce) {
                            style.noninteractive().fg_stroke.color
                        } else {
                            style.noninteractive().bg_stroke.color
                        }
                    }
                };
                let mut md = if graph.selected_face == Some(fce) {
                    Modifier::Highlight
                } else {
                    Modifier::None
                };
                if let Some(interactive) = self.current_action.try_lock() {
                    if let Some((_, interactive)) = interactive.deref() {
                        let modifier = interactive.modifier(&self, id);
                        crate::ui::vm::apply_modifier(modifier, &mut border_color, &mut md);
                    }
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

    fn face_folded<'a>(&'a mut self, fce: usize) -> Option<&'a mut bool> {
        if let Some(graph) = self.graph.try_lock().as_mut() {
            Some(&mut graph.graph.faces[fce].label.folded)
        } else {
            None
        }
    }

    fn action(&mut self, act: Action, ui: &mut Ui, rt: &mut RT) {
        self.graphical.hovered = None;

        if let Some(current_action) = self.current_action.try_lock() {
            if let Some((last, mut interactive)) = current_action.take() {
                let r = interactive.action(rt, self, act, ui);
                *current_action = Some((last, interactive));
                if !r {
                    return;
                }
            }
        }

        if let Some(graph) = self.graph.try_lock().as_mut() {
            match act {
                Action::Hover(id) => {
                    self.graphical.hovered = Some(id);
                    // Show tooltip
                    egui::show_tooltip_at_pointer(ui.ctx(), egui::Id::new("Graph tooltip"), |ui| {
                        let label = match id {
                            GraphId::Node(n) => {
                                let node = &graph.graph.nodes[n].2;
                                format!("node {}: {}", node.name, node.label)
                            }
                            GraphId::Morphism(src, dst) => {
                                let edge = &graph.graph.edges[src][dst].1;
                                format!("morphism {}: {}", edge.name, edge.label)
                            }
                            GraphId::Face(fce) => {
                                let face = &graph.graph.faces[fce].label;
                                format!("face {}", face.name)
                            }
                        };
                        ui.label(label)
                    });
                }
                Action::Click(GraphId::Face(fce)) => {
                    let running = rt.running();
                    if running.is_none() {
                        rt.run("Selecting face", move |vm| async {
                            let sel = vm.graph.lock().await.selected_face.clone();
                            if sel != Some(fce) {
                                if let Some(prev) = sel {
                                    vm.unshow_face(prev).await;
                                }
                                graph.selected_face = Some(fce);
                                vm.show_face(fce).await;
                            }
                        });
                    }
                }
                Action::DoubleClick(GraphId::Face(fce)) => {
                    let running = rt.running();
                    if running.is_none() {
                        rt.run("Solving face", move |vm| async {
                            let graph = vm.graph.lock().await;
                            self.insert_and_run(&format!("solve {}", graph.graph.faces[fce].label.name)).await;
                        });
                    }
                }
                Action::Drag(GraphId::Node(nd), pos, _) => {
                    if let Some(part) = graph.graph.nodes[nd].2.pos {
                        graph.layout.set_pos(part, pos);
                    }
                }
                Action::Drag(GraphId::Morphism(src, mph), _, vec) => {
                    if let Some(part) = graph.graph.edges[src][mph].1.control {
                        let pos = graph.layout.get_pos(part) + vec;
                        graph.layout.set_pos(part, pos);
                    }
                }
                _ => (),
            }
        }
    }

    fn context_menu(&mut self, on: GraphId, ui: &mut Ui, rt: &mut RT) -> bool {
        if let Some(current_action) = self.current_action.try_lock() {
            if let Some((last, mut interactive)) = current_action.take() {
                let r = interactive.context_menu(rt, self, on, ui);
                *current_action = Some((last, interactive));
                if r == CMR::Closed {
                    return false;
                }
            }
        }
        let running = rt.running();
        ui.add_enabled_ui(running.is_none(), |ui| {
            match on {
                GraphId::Node(n) => {
                    if ui.button("Merge with").clicked() {
                        rt.run("Start merge", move |vm| async {
                            let merge = InteractiveAction::merge(GraphId::Node(n)).await;
                            vm.start_interactive(merge).await;
                        });
                        ui.close_menu();
                        return false;
                    }
                    if ui
                        .button("Toggle pin")
                        .clicked()
                    {
                        rt.run("Toggle pin", move |vm| async {
                            let graph = vm.graph.lock().await;
                            graph.graph.nodes[n].2.pinned = !graph.graph.nodes[n].2.pinned;
                        });
                        ui.close_menu();
                        return false;
                    }
                    true
                }
                GraphId::Morphism(src, dst) => {
                    if ui.button("Split").clicked() {
                        rt.run("Split", move |vm| async {
                            let graph = vm.graph.lock().await;
                            vm.insert_and_run(&format!(
                                "split {}",
                                graph.graph.edges[src][dst].1.name
                            )).await;
                        });
                        ui.close_menu();
                        return false;
                    }
                    if ui.button("Merge with").clicked() {
                        rt.run("Start merge", move |vm| async {
                            let merge = InteractiveAction::merge(GraphId::Morphism(src, dst)).await;
                            vm.start_interactive(merge).await;
                        });
                        ui.close_menu();
                        return false;
                    }
                    if ui
                        .button("Toggle pin")
                        .clicked()
                    {
                        rt.run("Toggle pin", move |vm| async {
                            let graph = vm.graph.lock().await;
                            graph.graph.edges[src][dst].1.pinned =
                                !graph.graph.edges[src][dst].1.pinned;
                        });
                        ui.close_menu();
                        return false;
                    }
                    true
                }
                GraphId::Face(fce) => {
                    if let Some(graph) = self.graph.try_lock() {
                        if graph.graph.faces[fce].label.status == FaceStatus::Goal {
                            if ui.button("Solve").clicked() {
                                rt.run("Solve", move |vm| async {
                                    let graph = vm.graph.lock().await;
                                    vm.insert_and_run(&format!(
                                        "solve {}",
                                        graph.graph.faces[fce].label.name
                                    )).await;
                                });
                                ui.close_menu();
                                return false;
                            }
                            if ui.button("Decompose").clicked() {
                                rt.run("Decompose", move |vm| async {
                                    vm.planar_split(fce).await;
                                });
                                ui.close_menu();
                                return false;
                            }
                            if ui.button("Shrink").clicked() {
                                rt.run("Shrink", move |vm| async {
                                    let graph = vm.graph.lock().await;
                                    vm.insert_and_run(&format!(
                                        "shrink {}",
                                        graph.graph.faces[fce].label.name
                                    )).await;
                                });
                                ui.close_menu();
                                return false;
                            }
                            if ui.button("Merge with").clicked() {
                                rt.run("Start merge", move |vm| async {
                                    let merge = InteractiveAction::merge(GraphId::Face(fce)).await;
                                    vm.start_interactive(merge).await;
                                });
                                ui.close_menu();
                                return false;
                            }
                            if ui.button("Pull").clicked() {
                                rt.run("Pull", move |vm| async {
                                    let graph = vm.graph.lock().await;
                                    vm.insert_and_run(&format!(
                                        "pull {}, *",
                                        graph.graph.faces[fce].label.name
                                    )).await;
                                });
                                ui.close_menu();
                                return false;
                            }
                            if ui.button("Push").clicked() {
                                rt.run("Push", move |vm| async {
                                    let graph = vm.graph.lock().await;
                                    vm.insert_and_run(&format!(
                                        "push {}, *",
                                        graph.graph.faces[fce].label.name
                                    )).await;
                                });
                                ui.close_menu();
                                return false;
                            }
                        }
                    }
                    if ui.button("Toggle term").clicked() {
                        rt.run("Toggle term", move |vm| async {
                            let graph = vm.graph.lock().await;
                            graph.graph.faces[fce].label.folded = 
                                !graph.graph.faces[fce].label.folded;
                        });
                        ui.close_menu();
                        return false;
                    }
                    true
                }
            }
        }).inner
    }
}
