use super::code::code_text_box;
use crate::remote::Remote;
use crate::ui::VM;

// Return true when the application should exit
pub fn exit<Rm: Remote>(ctx: &mut egui::Context, vm: &mut VM<Rm>) -> bool {
    let mut exit = false;
    egui::CentralPanel::default().show(ctx, |ui| {
        ui.with_layout(egui::Layout::top_down_justified(egui::Align::RIGHT), |ui| {
            code_text_box(ui, vm, 40.0, false);
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                if ui.add(egui::Button::new("Done")).clicked() {
                    exit = true;
                }
            });
        })
    });
    exit
}
