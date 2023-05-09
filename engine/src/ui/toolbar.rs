use crate::ui::VM;
use egui::menu;

pub fn toolbar(ui: &mut egui::Ui, vm: &mut VM) {
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
            if ui.button("Open lemmas").clicked() {
                vm.lemma_window_open = true;
                ui.close_menu();
            }
        });
    });
}
