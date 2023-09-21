use super::code::code;
use super::graph::graph_vm;
use super::lemmas::{lemmas_menu, lemmas_window};
use super::toolbar::toolbar;
use super::{ActionResult, VM};
use crate::remote::Remote;

pub fn main<RPC: Remote + Sync + Send>(ctx: &egui::Context, vm: &mut VM<RPC>) {
    // Do one layout step
    {
        let fixed = |id| vm.dragged_object == Some(id) || vm.graph.pinned(id);
        vm.layout.apply_forces(&vm.config, &vm.graph, &fixed);
        vm.layout.update(&vm.config);

        if let Some(lem) = vm.selected_lemma {
            if vm.lemmas[lem].pattern.is_some() {
                let dragged = vm.lemmas[lem].graphical_state.dragged;
                let lem = &mut vm.lemmas[lem];
                let fixed = |id| dragged == Some(id) || lem.pattern.as_ref().unwrap().pinned(id);
                lem.graphical_state.layout.apply_forces(
                    &vm.config,
                    lem.pattern.as_ref().unwrap(),
                    &fixed,
                );
                lem.graphical_state.layout.update(&vm.config);
            }
        }
        {
            use crate::ui::InteractiveAction::*;
            match &vm.current_action {
                Some((_, LemmaApplication(state))) => {
                    if vm.selected_lemma != Some(state.lemma) {
                        let fixed = |id| state.dragged == Some(id) || state.graph.pinned(id);
                        vm.lemmas[state.lemma].graphical_state.layout.apply_forces(
                            &vm.config,
                            &state.graph,
                            &fixed,
                        );
                        vm.lemmas[state.lemma]
                            .graphical_state
                            .layout
                            .update(&vm.config);
                    }
                }
                Some((_, Merge(..))) => (),
                Some((_, Insert(..))) => (),
                None => (),
            }
        }
    }

    lemmas_window(ctx, vm);
    code(ctx, vm);
    if let Some((last, mut interactive)) = vm.current_action.take() {
        let r = interactive.display(vm, ctx);
        vm.current_action = Some((last, interactive));
        if r == ActionResult::Stop {
            vm.stop_interactive();
        } else if r == ActionResult::Commit {
            vm.commit_interactive();
        }
    }
    egui::SidePanel::left("Lemmas").show(ctx, |ui| lemmas_menu(ui, vm));

    egui::CentralPanel::default().show(ctx, |ui| {
        toolbar(ui, vm);
        ui.add(graph_vm(vm))
    });
}
