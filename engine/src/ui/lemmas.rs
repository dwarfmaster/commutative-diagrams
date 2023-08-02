use crate::remote::Remote;
use crate::ui::vm::InteractiveAction;
use crate::ui::{graph_lemma, VM};
use crate::vm::{Lemma, LemmaTree};

pub fn lemmas_window<Rm: Remote + Sync + Send>(ctx: &egui::Context, vm: &mut VM<Rm>) {
    if let Some(lem) = vm.selected_lemma {
        let mut open = true;
        let mut should_close = false;
        egui::Window::new(vm.lemmas[lem].name.clone())
            .id(egui::Id::new("Lemma graph"))
            .open(&mut open)
            .show(ctx, |ui| {
                ui.with_layout(egui::Layout::bottom_up(egui::Align::RIGHT), |ui| {
                    if ui.button("Apply").clicked() {
                        let apply = InteractiveAction::apply(vm, lem);
                        vm.start_interactive(apply);
                        should_close = true;
                    }
                    ui.add(graph_lemma(&mut vm.lemmas[lem]));
                })
            });
        if !open || should_close {
            vm.selected_lemma = None;
        }
    }

    if vm.lemma_window_open {
        egui::Window::new("Lemmas")
            .open(&mut vm.lemma_window_open)
            .show(ctx, |ui| {
                egui::ScrollArea::vertical().show(ui, |ui| {
                    let mut selected = None;
                    display_lemma_tree(
                        ui,
                        &vm.lemma_tree,
                        &vm.lemmas,
                        &vm.selected_lemma,
                        &mut selected,
                    );
                    if let Some(lem) = selected {
                        vm.lemmas[lem].get_pattern(&mut vm.ctx);
                        vm.selected_lemma = Some(lem);
                    }
                });
            });
    }
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
