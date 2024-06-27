use super::{Action, ActionResult, ContextMenuResult, Modifier, VM};
use crate::graph::GraphId;
use crate::graph::GraphId::*;
use crate::remote::Remote;
use egui::{Context, Ui};

pub struct ComposeState {
    src: usize,
    mphs: Vec<usize>,
    current: usize,
}

impl ComposeState {
    pub fn new_id(nd: usize) -> Self {
        ComposeState {
            src: nd,
            mphs: Vec::new(),
            current: nd,
        }
    }

    pub fn new<R: Remote>(vm: &VM<R>, src: usize, mph: usize) -> Self {
        ComposeState {
            src,
            mphs: [mph].to_vec(),
            current: vm.graph.graph.edges[src][mph].0,
        }
    }

    pub fn compile<R: Remote>(self, vm: &VM<R>) -> String {
        let mut src = self.src;
        let r = self.mphs.iter().fold(
            format!("compose {}", vm.get_name(Node(self.src))),
            |acc, mph| {
                let r = format!("{} {}", acc, vm.get_name(Morphism(src, *mph)));
                src = vm.graph.graph.edges[src][*mph].0;
                r
            },
        );
        log::info!("Composing to \"{}\"", r);
        r
    }

    pub fn display<R: Remote>(&mut self, vm: &mut VM<R>, ui: &Context) -> ActionResult {
        let mut open = true;
        let mut r = ActionResult::Continue;
        egui::Window::new("Composing")
            .open(&mut open)
            .show(ui, |ui| {
                if ui.button("Cancel").clicked() {
                    r = ActionResult::Stop;
                } else if ui.button("Compose").clicked() {
                    vm.path_to_edge(self.src, &self.mphs[..]);
                    r = ActionResult::Commit;
                }
            });
        if r == ActionResult::Continue && !open {
            r = ActionResult::Stop;
        }
        r
    }

    pub fn context_menu<R: Remote>(
        &mut self,
        _vm: &mut VM<R>,
        _on: GraphId,
        _ui: &mut Ui,
    ) -> ContextMenuResult {
        ContextMenuResult::Nothing
    }

    pub fn action<R: Remote>(&mut self, vm: &mut VM<R>, act: Action, _ui: &mut Ui) -> bool {
        if let Action::Click(Morphism(src, mph)) = act {
            if src == self.current {
                self.mphs.push(mph);
                self.current = vm.graph.graph.edges[src][mph].0;
                false
            } else {
                true
            }
        } else {
            true
        }
    }

    pub fn modifier<R: Remote>(&self, vm: &VM<R>, on: GraphId) -> Modifier {
        match on {
            Node(nd) => Modifier {
                active: nd == self.current,
                selected: nd == self.src,
                candidate: true,
            },
            Morphism(src, mph) => {
                let mut s = self.src;
                let mut selected = false;
                self.mphs.iter().for_each(|m| {
                    if src == s && mph == *m {
                        selected = true;
                    }
                    s = vm.graph.graph.edges[s][*m].0;
                });
                Modifier {
                    active: false,
                    selected,
                    candidate: src == self.current,
                }
            }
            _ => Modifier {
                active: false,
                selected: false,
                candidate: false,
            },
        }
    }
}
