use super::code::code_text_box;
use crate::runtime::Runtime;
use crate::ui::VM;

// Return true when the application should exit
pub fn exit<RT: Runtime>(ctx: &egui::Context, rt: &mut RT, vm: &mut VM<RT::Rem>) -> bool {
    let mut exit = false;
    egui::CentralPanel::default().show(ctx, |ui| {
        ui.with_layout(egui::Layout::top_down_justified(egui::Align::RIGHT), |ui| {
            code_text_box(ui, rt, vm, 40.0, false);
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                if ui.add(egui::Button::new("Done")).clicked() {
                    exit = true;
                }
            });
        })
    });
    exit
}
