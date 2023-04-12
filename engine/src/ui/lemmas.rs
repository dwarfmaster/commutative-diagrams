use crate::vm::VM;
use egui::Vec2;

pub fn lemmas_window(ctx: &egui::Context, vm: &mut VM) {
    egui::Window::new("Lemmas").show(ctx, |ui| {
        egui::ScrollArea::vertical().show(ui, |ui| {
            ui.vertical(|ui| {
                let style = ui.style().clone();
                let mut pair = false;
                ui.style_mut().visuals.widgets.noninteractive.rounding.nw = 0.0;
                ui.style_mut().visuals.widgets.noninteractive.rounding.ne = 0.0;
                ui.style_mut().visuals.widgets.noninteractive.rounding.sw = 0.0;
                ui.style_mut().visuals.widgets.noninteractive.rounding.se = 0.0;
                ui.style_mut().spacing.item_spacing = Vec2::ZERO;

                for lemma in &mut vm.lemmas {
                    let bg = if pair {
                        style.visuals.widgets.noninteractive.bg_fill
                    } else {
                        style.visuals.extreme_bg_color
                    };
                    ui.style_mut().visuals.extreme_bg_color = bg;
                    ui.style_mut().visuals.widgets.noninteractive.bg_stroke.color = bg;
                    pair = !pair;

                    let _resp = ui.add(
                        egui::TextEdit::singleline(&mut lemma.name)
                            .interactive(false)
                            .font(egui::TextStyle::Monospace)
                            .desired_width(f32::INFINITY),
                    ).on_hover_text(&lemma.name);
                }
            });
        });
    });
}
