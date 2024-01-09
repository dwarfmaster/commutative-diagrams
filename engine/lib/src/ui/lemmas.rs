use super::graph::graph_lemma;
use super::vm::InteractiveAction;
use crate::runtime::Runtime;
use crate::ui::VM;
use crate::vm::{Lemma, LemmaTree};

pub fn lemmas_window<RT: Runtime>(ctx: &egui::Context, rt: &mut RT, vm: &VM<RT::Rem>) {
    if let Some(lemmas) = vm.lemmas.try_lock().as_mut() {
        if let Some(lem) = lemmas.selected_lemma {
            let mut open = true;
            let mut should_close = false;
            egui::Window::new(lemmas.lemmas[lem].complete_name.clone())
                .id(egui::Id::new(lemmas.lemmas[lem].complete_name.as_str()))
                .open(&mut open)
                .show(ctx, |ui| {
                    ui.with_layout(egui::Layout::bottom_up(egui::Align::RIGHT), |ui| {
                        let running = rt.running();
                        ui.add_enabled_ui(running.is_none(), |ui| {
                            if ui.button("Start matching").clicked() {
                                rt.run("Start matching", move |vm| async {
                                    let apply = InteractiveAction::apply(vm, lem).await;
                                    vm.start_interactive(apply).await
                                });
                                should_close = true;
                            }
                        });
                        ui.add(graph_lemma(rt, &mut lemmas.lemmas[lem]));
                    })
                });
            if !open || should_close {
                lemmas.selected_lemma = None;
            }
        }
    }
}

pub fn lemmas_menu<RT: Runtime>(ui: &mut egui::Ui, rt: &mut RT, vm: &VM<RT::Rem>) {
    egui::ScrollArea::vertical().show(ui, |ui| {
        let mut selected = None;
        if let Some(lemmas) = vm.lemmas.try_lock().as_mut() {
            let can_run = rt.running().is_none();
            display_lemma_tree(
                ui,
                can_run,
                &lemmas.lemma_tree,
                &lemmas.lemmas,
                &lemmas.selected_lemma,
                &mut selected,
            );
            if let Some(lem) = selected {
                rt.run("Opening lemma", |vm| async {
                    let lemmas = vm.lemmas.lock().await;
                    let config = vm.config.lock().await;
                    let ctx = &mut vm.ctx.lock().await;
                    let () = lemmas.lemmas[lem].get_pattern(ctx, &config).await;
                    lemmas.selected_lemma = Some(lem);
                });
            }
        } else {
            ui.spinner();
        }
    });
}

fn display_lemma_tree(
    ui: &mut egui::Ui,
    enabled: bool,
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
                            display_lemma_tree(ui, enabled, sub, lemmas, selected, to_select)
                        });
                }

                Leaf(lem) => {
                    ui.add_enabled_ui(enabled, |ui| {
                        let sel = *selected == Some(*lem);
                        let resp = ui.selectable_label(sel, &lemmas[*lem].name);
                        if enabled && resp.clicked() {
                            *to_select = Some(*lem);
                        }
                    });
                }
            }
        }
    });
}
