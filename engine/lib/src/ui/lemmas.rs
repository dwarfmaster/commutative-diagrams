use super::graph::graph_lemma;
use super::vm::InteractiveAction;
use crate::remote::Remote;
use crate::ui::VM;
use crate::vm::{Lemma, LemmaTree};

pub fn lemmas_window<Rm: Remote>(ctx: &egui::Context, vm: &mut VM<Rm>) {
    if let Some(lem) = vm.lemmas.selected_lemma {
        let mut open = true;
        let mut should_close = false;
        egui::Window::new(vm.lemmas.lemmas[lem].complete_name.clone())
            .id(egui::Id::new(vm.lemmas.lemmas[lem].complete_name.as_str()))
            .open(&mut open)
            .show(ctx, |ui| {
                ui.with_layout(egui::Layout::bottom_up(egui::Align::RIGHT), |ui| {
                    if ui.button("Start matching").clicked() {
                        let apply = InteractiveAction::apply(vm, lem);
                        vm.start_interactive(apply);
                        should_close = true;
                    }
                    ui.add(graph_lemma(&mut vm.lemmas.lemmas[lem]));
                })
            });
        if !open || should_close {
            vm.lemmas.selected_lemma = None;
        }
    }
}

pub fn lemmas_menu<Rm: Remote>(ui: &mut egui::Ui, vm: &mut VM<Rm>) {
    egui::ScrollArea::vertical().show(ui, |ui| {
        let mut selected = None;
        display_lemma_tree(
            ui,
            &vm.lemmas.lemma_tree,
            &vm.lemmas.lemmas,
            &vm.lemmas.selected_lemma,
            &mut selected,
        );
        if let Some(lem) = selected {
            vm.lemmas.lemmas[lem].get_pattern(&mut vm.ctx, &vm.config);
            vm.lemmas.selected_lemma = Some(lem);
        }
    });
}

fn display_lemma_tree(
    ui: &mut egui::Ui,
    tree: &[Box<LemmaTree>],
    lemmas: &[Lemma],
    selected: &Option<usize>,
    to_select: &mut Option<usize>,
) {
    use LemmaTree::*;
    ui.vertical(|ui| {
        for sub in tree {
            match sub.as_ref() {
                Node(name, sub) => {
                    egui::CollapsingHeader::new(name)
                        .default_open(false)
                        .show(ui, |ui| {
                            display_lemma_tree(ui, sub, lemmas, selected, to_select)
                        });
                }

                Leaf(lem) => {
                    let sel = *selected == Some(*lem);
                    let resp = ui.selectable_label(sel, &lemmas[*lem].name);
                    if resp.clicked() {
                        *to_select = Some(*lem);
                    }
                }
            }
        }
    });
}
