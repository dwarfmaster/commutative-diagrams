use super::{Action, ActionResult, ContextMenuResult, Modifier, VM};
use super::InteractiveAction::Insert;
use crate::data::{Feature, Tag};
use crate::graph::GraphId;
use crate::remote::Remote;
use crate::runtime::Runtime;
use egui::{Context, Ui, Vec2};

pub enum InsertKind {
    Object,
    Morphism,
    Equality,
}

pub struct InsertState {
    kind: InsertKind,
    text: String,
    error_msg: Option<String>,
    finished: bool,
    aborted: bool,
}

impl InsertState {
    pub fn new(kind: InsertKind) -> Self {
        Self {
            kind,
            text: "".to_string(),
            error_msg: None,
            finished: false,
            aborted: false,
        }
    }

    async fn do_insert<R: Remote>(vm: &mut VM<R>) {
        use InsertKind::*;
        let config = &mut vm.config.lock().await;
        let ctx = &mut vm.ctx.lock().await;
        let graph = &mut vm.graph.lock().await;
        let interactive = &mut vm.current_action.lock().await;
        let ins = match interactive.as_mut() {
            Some((_,Insert(ins))) => ins,
            _ => panic!(),
        };
        let r = ctx.remote.parse(ins.text.clone()).unwrap();
        match r {
            Ok(id) => match ins.kind {
                Object => {
                    let tps = ctx.get_stored_query(id, Tag::Object);
                    if tps.is_empty() {
                        ins.error_msg = Some(format!("\"{}\" is not an object", ins.text));
                        return;
                    }
                    for tp in tps {
                        if let Feature::Object { cat } = tp {
                            vm.insert_node(id, cat);
                        }
                    }
                    graph.particles_for_graph(&config);
                    ins.finished = true;
                }
                Morphism => {
                    let tps = ctx.get_stored_query(id, Tag::Morphism);
                    if tps.is_empty() {
                        ins.error_msg = Some(format!("\"{}\" is not a morphism", ins.text));
                        return;
                    }
                    for tp in tps {
                        if let Feature::Morphism { cat, .. } = tp {
                            vm.insert_mph(id, cat);
                        }
                    }
                    graph.particles_for_graph(&config);
                    ins.finished = true;
                }
                Equality => {
                    let tps = ctx.get_stored_query(id, Tag::Equality);
                    if tps.is_empty() {
                        ins.error_msg = Some(format!("\"{}\" is not an equality", ins.text));
                        return;
                    }
                    for tp in tps {
                        if let Feature::Equality { cat, .. } = tp {
                            vm.insert_eq(id, cat);
                        }
                    }
                    graph
                        .particles_for_graph(&config);
                    ins.finished = true;
                }
            },
            Err(msg) => ins.error_msg = Some(format!("Couldn't parse \"{}\": {}", ins.text, msg)),
        }
    }

    pub fn compile<R: Remote>(&self, _vm: &VM<R>) -> String {
        use InsertKind::*;
        let kind = match self.kind {
            Object => "node",
            Morphism => "morphism",
            Equality => "equality",
        };
        format!("insert {} \"{}\"", kind, self.text)
    }

    pub fn display<RT: Runtime>(
        &mut self,
        rt: &mut RT,
        vm: &VM<RT::Rem>,
        ui: &Context,
    ) -> ActionResult {
        if self.aborted {
            return ActionResult::Stop;
        } else if self.finished {
            return ActionResult::Commit;
        }

        if let Some(errmsg) = &mut self.error_msg {
            let mut open = true;
            egui::Window::new("Error!")
                .id(egui::Id::new("Insert error"))
                .open(&mut open)
                .show(ui, |ui| {
                    egui::TextEdit::multiline(errmsg)
                        .interactive(false)
                        .show(ui);
                });
            if !open {
                self.error_msg = None;
            }
        }

        let mut open = true;
        let kind = match self.kind {
            InsertKind::Object => "object",
            InsertKind::Morphism => "morphism",
            InsertKind::Equality => "equality",
        };
        egui::Window::new(format!("Insert {}", kind))
            .id(egui::Id::new("Insert"))
            .open(&mut open)
            .show(ui, |ui| {
                ui.add_enabled_ui(self.error_msg.is_none(), |ui| {
                    ui.with_layout(egui::Layout::bottom_up(egui::Align::RIGHT), |ui| {
                        ui.allocate_ui_with_layout(
                            Vec2::new(100.0, 40.0),
                            egui::Layout::right_to_left(egui::Align::Center),
                            |ui| {
                                let running = rt.running();
                                ui.add_enabled_ui(running.is_none(), |ui| {
                                    if ui.button("Insert").clicked() {
                                        rt.run("Insert object", move |vm| async {
                                            Self::do_insert::<RT::Rem>(&mut vm).await;
                                        });
                                    }
                                });
                                if ui.button("Cancel").clicked() {
                                    self.aborted = true;
                                }
                            },
                        );
                        ui.add_sized(
                            ui.available_size(),
                            egui::TextEdit::multiline(&mut self.text),
                        );
                    })
                })
            });
        if !open {
            self.aborted = true;
        }

        ActionResult::Continue
    }

    pub fn context_menu<RT: Runtime>(
        &mut self,
        _rt: &mut RT,
        _vm: &VM<RT::Rem>,
        _on: GraphId,
        _ui: &mut Ui,
    ) -> ContextMenuResult {
        ContextMenuResult::Nothing
    }

    pub fn action<RT: Runtime>(
        &mut self,
        _rt: &mut RT,
        _vm: &VM<RT::Rem>,
        _act: Action,
        _ui: &mut Ui,
    ) -> bool {
        true
    }

    pub fn modifier<R: Remote>(&self, _vm: &VM<R>, _on: GraphId) -> Modifier {
        Modifier {
            active: false,
            candidate: false,
            selected: false,
        }
    }
}
