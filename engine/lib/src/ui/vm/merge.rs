use super::{Action, ActionResult, ContextMenuResult, Modifier, VM};
use super::InteractiveAction::Merge;
use crate::graph::GraphId;
use crate::remote::Remote;
use crate::runtime::Runtime;
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

    pub fn compile<R: Remote>(self, _vm: &VM<R>) -> String {
        if let Some((name1, name2)) = &self.result {
            format!("merge {} {}", name1, name2)
        } else {
            String::new()
        }
    }

    async fn target<R: Remote>(vm: &mut VM<R>, target: GraphId) {
        let ctx = &mut vm.ctx.lock().await;
        let interactive = &mut vm.current_action.lock().await;
        let mrg = match interactive.as_mut() {
            Some((_,Merge(mrg))) => mrg,
            _ => panic!(),
        };
        let result = {
            let graph = vm.graph.lock().await;
            (graph.get_name(mrg.merging), graph.get_name(target))
        };
        let state = ctx.save_state();
        if vm.merge_dwim(mrg.merging, target).await {
            mrg.result = Some(result);
            let () = vm.relabel().await;
        } else {
            let () = ctx.restore_state(state);
            mrg.error_msg = Some(format!("Couldn't merge {} with {}", result.0, result.1,));
        }
    }

    pub fn display<RT: Runtime>(
        &mut self,
        rt: &mut RT,
        _vm: &VM<RT::Rem>,
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

    pub fn context_menu<RT: Runtime>(
        &mut self,
        rt: &mut RT,
        vm: &VM<RT::Rem>,
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
            let running = rt.running();
            ui.add_enabled_ui(running.is_none(), |ui| {
                if ui.button("Merge").clicked() {
                    rt.run("Merge", move |vm| async {
                        Self::target(&mut vm, on).await;
                    });
                    ui.close_menu();
                    r = ContextMenuResult::Closed;
                }
            });
        }
        r
    }

    pub fn action<RT: Runtime>(
        &mut self,
        rt: &mut RT,
        vm: &VM<RT::Rem>,
        act: Action,
        _ui: &mut Ui,
    ) -> bool {
        match act {
            Action::Click(id) => {
                if id != self.merging && id.same_nature(&self.merging) && rt.running().is_none() {
                    rt.run("Merge", move |vm| async {
                        Self::target(&mut vm, id).await;
                    });
                    false
                } else {
                    true
                }
            }
            _ => true,
        }
    }

    pub fn modifier<R: Remote>(&self, _vm: &VM<R>, on: GraphId) -> Modifier {
        Modifier {
            active: on == self.merging,
            selected: false,
            candidate: on.same_nature(&self.merging),
        }
    }
}
