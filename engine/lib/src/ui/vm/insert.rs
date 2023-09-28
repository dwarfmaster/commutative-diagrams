use super::{Action, ActionResult, ContextMenuResult, Modifier, VM};
use crate::data::{Feature, Tag};
use crate::graph::GraphId;
use crate::remote::Remote;
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

    fn do_insert<R: Remote>(&mut self, vm: &mut VM<R>) {
        use InsertKind::*;
        let r = vm.ctx.remote.parse(self.text.clone()).unwrap();
        match r {
            Ok(id) => match self.kind {
                Object => {
                    let tps = vm.ctx.get_stored_query(id, Tag::Object);
                    if tps.is_empty() {
                        self.error_msg = Some(format!("\"{}\" is not an object", self.text));
                        return;
                    }
                    for tp in tps {
                        if let Feature::Object { cat } = tp {
                            vm.insert_node(id, cat);
                        }
                    }
                    vm.layout.particles_for_graph(&vm.config, &mut vm.graph);
                    self.finished = true;
                }
                Morphism => {
                    let tps = vm.ctx.get_stored_query(id, Tag::Morphism);
                    if tps.is_empty() {
                        self.error_msg = Some(format!("\"{}\" is not a morphism", self.text));
                        return;
                    }
                    for tp in tps {
                        if let Feature::Morphism { cat, .. } = tp {
                            vm.insert_mph(id, cat);
                        }
                    }
                    vm.layout.particles_for_graph(&vm.config, &mut vm.graph);
                    self.finished = true;
                }
                Equality => {
                    let tps = vm.ctx.get_stored_query(id, Tag::Equality);
                    if tps.is_empty() {
                        self.error_msg = Some(format!("\"{}\" is not an equality", self.text));
                        return;
                    }
                    for tp in tps {
                        if let Feature::Equality { cat, .. } = tp {
                            vm.insert_eq(id, cat);
                        }
                    }
                    vm.layout.particles_for_graph(&vm.config, &mut vm.graph);
                    self.finished = true;
                }
            },
            Err(msg) => self.error_msg = Some(format!("Couldn't parse \"{}\": {}", self.text, msg)),
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

    pub fn display<R: Remote>(&mut self, vm: &mut VM<R>, ui: &Context) -> ActionResult {
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
                                if ui.button("Insert").clicked() {
                                    self.do_insert(vm);
                                }
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

    pub fn context_menu<R: Remote>(
        &mut self,
        _vm: &mut VM<R>,
        _on: GraphId,
        _ui: &mut Ui,
    ) -> ContextMenuResult {
        ContextMenuResult::Nothing
    }

    pub fn action<R: Remote>(&mut self, _vm: &mut VM<R>, _act: Action, _ui: &mut Ui) -> bool {
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
