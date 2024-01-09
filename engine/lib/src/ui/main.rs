use super::code::code;
use super::graph::graph_vm;
use super::lemmas::{lemmas_menu, lemmas_window};
use super::toolbar::toolbar;
use super::{ActionResult, VM};
use crate::runtime::Runtime;

pub fn main<RT: Runtime>(ctx: &egui::Context, rt: &mut RT, vm: &VM<RT::Rem>) {
    // Do one layout step
    if let Some(config) = vm.config.try_lock() {
        if let Some(mut graph) = vm.graph.try_lock() {
            let fixed = |id| vm.graphical.dragged == Some(id) || graph.graph.pinned(id);
            graph.apply_forces(&config, &fixed);
            graph.layout.update(&config);
        }

        if let Some(mut lemmas) = vm.lemmas.try_lock() {
            if let Some(lem) = lemmas.selected_lemma {
                if let Some(mut pattern) = lemmas.lemmas[lem].pattern.try_lock() {
                    if let Some(pattern) = pattern.as_mut() {
                        let dragged = lemmas.lemmas[lem].graphical.dragged;
                        let lem = &mut lemmas.lemmas[lem];
                        let fixed = |id| dragged == Some(id) || pattern.graph.pinned(id);
                        pattern.apply_forces(&config, &fixed);
                        pattern.layout.update(&config);
                    }
                }
            }
        }

        if let Some(action) = vm.current_action.try_lock() {
            use crate::ui::InteractiveAction::*;
            match *action {
                Some((_, LemmaApplication(state))) => {
                    if let Some(lemmas) = vm.lemmas.try_lock() {
                        if lemmas.selected_lemma != Some(state.lemma) {
                            let fixed = |id| state.dragged == Some(id) || state.graph.pinned(id);
                            // TODO use the graphstate in the lemma, store the
                            // previous graph in the state and restore it when
                            // canceling/commiting the action
                            // if let Some(pattern) = lemmas.lemmas[state.lemma].pattern.try_lock() {
                            //     pattern.apply_forces(&vm.config, &state.graph, &fixed);
                            //     pattern.layout.update(&vm.config);
                            // }
                        }
                    }
                }
                Some((_, Merge(..))) => (),
                Some((_, Insert(..))) => (),
                None => (),
            }
        }
    }

    // TODO
    // lemmas_window(ctx, rt, vm);
    // code(ctx, rt, vm);
    if let Some(mut action) = vm.current_action.try_lock() {
        if let Some((last, mut interactive)) = action.take() {
            // let r = interactive.display(rt, vm, ctx);
            *action = Some((last, interactive));
            // if r == ActionResult::Stop {
            //     vm.stop_interactive();
            // } else if r == ActionResult::Commit {
            //     vm.commit_interactive();
            // }
        }
    }
    egui::SidePanel::left("Lemmas").show(ctx, |ui| lemmas_menu(ui, rt, vm));

    egui::CentralPanel::default().show(ctx, |ui| {
        toolbar(ui, rt, vm);
        //     ui.add(graph_vm(rt, vm))
    });
}
