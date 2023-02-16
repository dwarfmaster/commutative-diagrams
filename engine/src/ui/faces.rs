use crate::ui::GraphDisplay;

pub fn faces(ui: &mut egui::Ui, gr: &mut GraphDisplay) {
    egui::ScrollArea::vertical().show(ui, |ui| {
        for fce in 0..gr.graph.faces.len() {
            let label = &gr.graph.faces[fce].label.label;
            ui.add(egui::Label::new(label).wrap(false));
        }
    });
}
