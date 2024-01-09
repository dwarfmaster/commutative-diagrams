use super::vm::insert::InsertKind::*;
use super::InteractiveAction;
use crate::runtime::Runtime;
use crate::ui::VM;
use egui::menu;

pub fn toolbar<RT: Runtime>(ui: &mut egui::Ui, rt: &mut RT, vm: &VM<RT::Rem>) {
    let running = rt.running();
    menu::bar(ui, |ui| {
        ui.menu_button("Proof", |ui| {
            ui.add_enabled_ui(running.is_none(), |ui| {
                if ui.button("Finish").clicked() {
                    rt.run("Succeeding", |vm| async {
                        vm.insert_and_run("succeed").await
                    });
                    ui.close_menu();
                }
                if ui.button("Fail").clicked() {
                    rt.run("Failing", |vm| async { vm.insert_and_run("fail").await });
                    ui.close_menu();
                }
                if ui.button("View script").clicked() {
                    rt.run("Opening script window", |vm| async {
                        let code = vm.code.lock().await;
                        code.code_window_open = true;
                    });
                    ui.close_menu();
                }
            })
        });
        ui.menu_button("Edit", |ui| {
            ui.add_enabled_ui(running.is_none(), |ui| {
                if ui.button("Undo").clicked() {
                    rt.run("Undoing", |vm| async { vm.undo().await });
                    ui.close_menu();
                }
                if ui.button("Redo").clicked() {
                    rt.run("Redoing", |vm| async { vm.redo().await });
                    ui.close_menu();
                }
            });
            ui.menu_button("Insert", |ui| {
                ui.add_enabled_ui(running.is_none(), |ui| {
                    if ui.button("Object").clicked() {
                        rt.run("Inserting object", |vm| async {
                            vm.start_interactive(InteractiveAction::insert(Object).await)
                                .await
                        });
                        ui.close_menu();
                    }
                    if ui.button("Morphism").clicked() {
                        rt.run("Inserting morphism", |vm| async {
                            vm.start_interactive(InteractiveAction::insert(Morphism).await)
                                .await
                        });
                        ui.close_menu();
                    }
                    if ui.button("Equality").clicked() {
                        rt.run("Inserting equality", |vm| async {
                            vm.start_interactive(InteractiveAction::insert(Equality).await)
                                .await
                        });
                        ui.close_menu();
                    }
                });
            });
        });
        ui.menu_button("Layout", |ui| {
            if let Some(config) = vm.config.try_lock().as_mut() {
                if ui
                    .checkbox(&mut config.layout.edge_repulse, "Repulse edges")
                    .clicked()
                {
                    ui.close_menu();
                }
                ui.horizontal(|ui| {
                    ui.label("Speed");
                    ui.add(egui::Slider::new(&mut config.layout.speed, 0.0..=2.0))
                });
            } else {
                ui.add(egui::Spinner::new());
            }
        });
        ui.menu_button("Ui", |ui| {
            const MIN_PPP: f32 = 0.2f32;
            const MAX_PPP: f32 = 4f32;
            if ui.button("Reset zoom").clicked() {
                let display = &mut vm.display;
                if let Some(ppp) = display.init_ppp {
                    display.ppp = Some(ppp);
                }
                ui.close_menu();
            }
            if ui.button("Zoom in").clicked() {
                let display = &mut vm.display;
                if display.init_ppp.is_none() {
                    display.init_ppp = Some(ui.ctx().pixels_per_point());
                }
                let mut ppp = display
                    .ppp
                    .unwrap_or_else(|| ui.ctx().pixels_per_point());
                ppp += 0.1f32;
                ppp = ppp.clamp(MIN_PPP, MAX_PPP);
                ppp = (ppp * 10f32).round() / 10f32;
                display.ppp = Some(ppp);
                ui.close_menu();
            }
            if ui.button("Zoom out").clicked() {
                let display = &mut vm.display;
                if display.init_ppp.is_none() {
                    display.init_ppp = Some(ui.ctx().pixels_per_point());
                }
                let mut ppp = display
                    .ppp
                    .unwrap_or_else(|| ui.ctx().pixels_per_point());
                ppp -= 0.1f32;
                ppp = ppp.clamp(MIN_PPP, MAX_PPP);
                ppp = (ppp * 10f32).round() / 10f32;
                display.ppp = Some(ppp);
                ui.close_menu();
            }
        });
    });
}
