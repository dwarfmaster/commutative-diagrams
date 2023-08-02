use crate::remote::Remote;
use crate::ui::VM;
use egui::menu;

pub fn toolbar<Rm: Remote + Sync + Send>(ui: &mut egui::Ui, vm: &mut VM<Rm>) {
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
                vm.code_window_open = true;
                ui.close_menu();
            }
        });
    });
}
