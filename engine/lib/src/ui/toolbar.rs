use super::vm::insert::InsertKind::*;
use super::InteractiveAction;
use crate::remote::Remote;
use crate::ui::VM;
use egui::menu;

pub fn toolbar<Rm: Remote>(ui: &mut egui::Ui, vm: &mut VM<Rm>) {
    menu::bar(ui, |ui| {
        ui.menu_button("Proof", |ui| {
            if ui.button("Finish").clicked() {
                vm.insert_and_run("succeed");
                ui.close_menu();
            }
            if ui.button("Fail").clicked() {
                vm.insert_and_run("fail");
                ui.close_menu();
            }
            if ui.button("View script").clicked() {
                vm.code.code_window_open = true;
                ui.close_menu();
            }
        });
        ui.menu_button("Edit", |ui| {
            if ui.button("Undo").clicked() {
                vm.undo();
                ui.close_menu();
            }
            if ui.button("Redo").clicked() {
                vm.redo();
                ui.close_menu();
            }
            ui.menu_button("Insert", |ui| {
                if ui.button("Object").clicked() {
                    vm.start_interactive(InteractiveAction::insert(Object));
                    ui.close_menu();
                }
                if ui.button("Morphism").clicked() {
                    vm.start_interactive(InteractiveAction::insert(Morphism));
                    ui.close_menu();
                }
                if ui.button("Equality").clicked() {
                    vm.start_interactive(InteractiveAction::insert(Equality));
                    ui.close_menu();
                }
            });
        });
        ui.menu_button("Layout", |ui| {
            if ui
                .checkbox(&mut vm.config.layout.edge_repulse, "Repulse edges")
                .clicked()
            {
                ui.close_menu();
            }
            ui.horizontal(|ui| {
                ui.label("Speed");
                ui.add(egui::Slider::new(&mut vm.config.layout.speed, 0.0..=2.0))
            });
        });
        ui.menu_button("Ui", |ui| {
            const MIN_PPP: f32 = 0.2f32;
            const MAX_PPP: f32 = 4f32;
            if ui.button("Reset zoom").clicked() {
                if let Some(ppp) = vm.graphical.init_ppp {
                    vm.graphical.ppp = Some(ppp);
                }
                ui.close_menu();
            }
            if ui.button("Zoom in").clicked() {
                if vm.graphical.init_ppp.is_none() {
                    vm.graphical.init_ppp = Some(ui.ctx().pixels_per_point());
                }
                let mut ppp = vm
                    .graphical
                    .ppp
                    .unwrap_or_else(|| ui.ctx().pixels_per_point());
                ppp += 0.1f32;
                ppp = ppp.clamp(MIN_PPP, MAX_PPP);
                ppp = (ppp * 10f32).round() / 10f32;
                vm.graphical.ppp = Some(ppp);
                ui.close_menu();
            }
            if ui.button("Zoom out").clicked() {
                if vm.graphical.init_ppp.is_none() {
                    vm.graphical.init_ppp = Some(ui.ctx().pixels_per_point());
                }
                let mut ppp = vm
                    .graphical
                    .ppp
                    .unwrap_or_else(|| ui.ctx().pixels_per_point());
                ppp -= 0.1f32;
                ppp = ppp.clamp(MIN_PPP, MAX_PPP);
                ppp = (ppp * 10f32).round() / 10f32;
                vm.graphical.ppp = Some(ppp);
                ui.close_menu();
            }
        });
    });
}
