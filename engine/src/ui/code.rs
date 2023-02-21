use crate::vm;

pub fn code(ui: &mut egui::Ui, vm: &mut vm::VM) {
    let error = vm.error_at.clone();
    // TODO memoize
    let mut layouter = |ui: &egui::Ui, string: &str, _width: f32| {
        let format = egui::TextFormat {
            font_id: egui::FontId::monospace(14.0),
            color: ui.style().noninteractive().fg_stroke.color,
            background: ui.style().visuals.extreme_bg_color,
            italics: false,
            underline: egui::Stroke::NONE,
            strikethrough: egui::Stroke::NONE,
            valign: egui::Align::BOTTOM,
        };
        let under_stroke = egui::Stroke {
            color: ui.style().visuals.error_fg_color,
            width: ui.style().visuals.text_cursor_width,
        };
        let job = match error {
            Some((start, end)) => egui::text::LayoutJob {
                text: string.to_string(),
                sections: vec![
                    egui::text::LayoutSection {
                        leading_space: 0.0,
                        byte_range: std::ops::Range {
                            start: 0,
                            end: start,
                        },
                        format: format.clone(),
                    },
                    egui::text::LayoutSection {
                        leading_space: 0.0,
                        byte_range: std::ops::Range { start, end },
                        format: egui::TextFormat {
                            underline: under_stroke,
                            ..format.clone()
                        },
                    },
                    egui::text::LayoutSection {
                        leading_space: 0.0,
                        byte_range: std::ops::Range {
                            start: end,
                            end: string.len(),
                        },
                        format,
                    },
                ],
                ..Default::default()
            },
            None => egui::text::LayoutJob {
                text: string.to_string(),
                sections: vec![egui::text::LayoutSection {
                    leading_space: 0.0,
                    byte_range: std::ops::Range {
                        start: 0,
                        end: string.len(),
                    },
                    format,
                }],
                ..Default::default()
            },
        };
        //
        //     job.sections.push(egui::text::LayoutSection {
        //         leading_space: 0.0,
        //         byte_range: std::ops::Range { start, end },
        //         format: Default::default(),
        //     })
        // }
        ui.fonts().layout_job(job)
    };

    ui.with_layout(egui::Layout::top_down_justified(egui::Align::RIGHT), |ui| {
        egui::ScrollArea::vertical()
            .auto_shrink([false, false])
            .max_height(ui.available_height() - 150.0)
            .show(ui, |ui| {
                let event = ui.add_sized(
                    ui.available_size(),
                    egui::TextEdit::multiline(&mut vm.code)
                        .code_editor()
                        .cursor_at_end(true)
                        .layouter(&mut layouter),
                );
                if event.changed() {
                    vm.error_at = None;
                }
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
                    vm.run();
                }
            }
            if ui.add(egui::Button::new("Check")).clicked() {
                log::debug!("Recompiling");
                vm.recompile();
            }
        });
    });
}
