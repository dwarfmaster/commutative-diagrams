use crate::vm;

pub fn code(ui: &mut egui::Ui, vm: &mut vm::VM) {
    ui.with_layout(egui::Layout::top_down_justified(egui::Align::RIGHT), |ui| {
        egui::ScrollArea::vertical()
            .auto_shrink([false, false])
            .max_height(ui.available_height() - 150.0)
            .show(ui, |ui| {
                ui.add_sized(
                    ui.available_size(),
                    egui::TextEdit::multiline(&mut vm.code).code_editor(),
                )
            });
        egui::ScrollArea::vertical()
            .max_height(100.0)
            .id_source("error_msg_scroll_area")
            .show(ui, |ui| {
                ui.add_sized(
                    ui.available_size(),
                    egui::TextEdit::multiline(&mut vm.error_msg)
                        .font(egui::TextStyle::Monospace)
                        .interactive(false),
                )
            });
        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            if ui.add(egui::Button::new("Run")).clicked() {
                log::debug!("Running");
                if vm.recompile() {
                    // TODO execute the ast
                }
            }
            if ui.add(egui::Button::new("Check")).clicked() {
                log::debug!("Recompiling");
                vm.recompile();
            }
        });
    });
}
