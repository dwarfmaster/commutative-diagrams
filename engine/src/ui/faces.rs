use crate::ui::GraphDisplay;
use lens_rs::*;

pub fn faces<NL, EL, FL, PG, NNG, PtG, PtNG, FcNG: Copy>(
    ui: &mut egui::Ui,
    gr: &mut GraphDisplay<NL, EL, FL, PG, NNG, PtG, PtNG, FcNG>,
) where
    FL: LensRef<FcNG, String>,
{
    egui::ScrollArea::vertical().show(ui, |ui| {
        for fce in 0..gr.graph.faces.len() {
            let label = gr.graph.faces[fce].label.view_ref(gr.face_name);
            ui.add(egui::Label::new(label).wrap(false));
        }
    });
}
