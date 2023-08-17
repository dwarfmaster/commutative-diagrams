use super::{Action, ActionResult, ContextMenuResult, Modifier, VM};
use crate::graph::GraphId;
use crate::remote::Remote;
use egui::{Context, Ui};

pub struct MergeState {
    pub merging: GraphId,
    pub result: Option<(String, String)>,
    pub error_msg: Option<String>,
    pub cancel: bool,
}

impl MergeState {
    pub fn new(id: GraphId) -> Self {
        MergeState {
            merging: id,
            result: None,
            error_msg: None,
            cancel: false,
        }
    }

    pub fn compile<R: Remote + Sync + Send>(self, _vm: &VM<R>) -> String {
        if let Some((name1, name2)) = &self.result {
            format!("merge {} {}", name1, name2)
        } else {
            String::new()
        }
    }

    fn target<R: Remote + Sync + Send>(&mut self, vm: &mut VM<R>, target: GraphId) {
        let result = (vm.get_name(self.merging), vm.get_name(target));
        if vm.merge_dwim(self.merging, target) {
            self.result = Some(result);
        } else {
            self.error_msg = Some(format!(
                "Couldn't merge {} with {}",
                vm.get_name(self.merging),
                vm.get_name(target)
            ));
        }
    }

    pub fn display<R: Remote + Sync + Send>(
        &mut self,
        _vm: &mut VM<R>,
        ui: &Context,
    ) -> ActionResult {
        if self.cancel {
            return ActionResult::Stop;
        } else if self.result.is_some() {
            return ActionResult::Commit;
        }
        if let Some(emsg) = &mut self.error_msg {
            let mut open = true;
            egui::Window::new("Error!")
                .id(egui::Id::new("Merge error"))
                .open(&mut open)
                .show(ui, |ui| {
                    egui::TextEdit::multiline(emsg).interactive(false).show(ui);
                });
            if !open {
                self.error_msg = None;
            }
        }

        ActionResult::Continue
    }

    pub fn context_menu<R: Remote + Sync + Send>(
        &mut self,
        vm: &mut VM<R>,
        on: GraphId,
        ui: &mut Ui,
    ) -> ContextMenuResult {
        if !self.merging.same_nature(&on) {
            return ContextMenuResult::Nothing;
        }
        let mut r = ContextMenuResult::Added;
        if self.merging == on {
            if ui.button("Cancel").clicked() {
                self.cancel = true;
                ui.close_menu();
                r = ContextMenuResult::Closed;
            }
        } else {
            if ui.button("Merge").clicked() {
                self.target(vm, on);
                ui.close_menu();
                r = ContextMenuResult::Closed;
            }
        }
        r
    }

    pub fn action<R: Remote + Sync + Send>(
        &mut self,
        vm: &mut VM<R>,
        act: Action,
        _ui: &mut Ui,
    ) -> bool {
        match act {
            Action::Click(id) => {
                if id != self.merging && id.same_nature(&self.merging) {
                    self.target(vm, id);
                    false
                } else {
                    true
                }
            }
            _ => true,
        }
    }

    pub fn modifier<R: Remote + Sync + Send>(&self, _vm: &VM<R>, on: GraphId) -> Modifier {
        Modifier {
            active: on == self.merging,
            selected: false,
            candidate: on.same_nature(&self.merging),
        }
    }
}
