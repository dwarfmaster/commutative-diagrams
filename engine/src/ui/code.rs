use crate::ui::VM;
use crate::vm;
use itertools::Itertools;

pub fn code(ui: &mut egui::Ui, vm: &mut VM) {
    let format_none = egui::TextFormat {
        font_id: egui::FontId::monospace(14.0),
        color: ui.style().noninteractive().fg_stroke.color,
        background: ui.style().visuals.extreme_bg_color,
        italics: false,
        underline: egui::Stroke::NONE,
        strikethrough: egui::Stroke::NONE,
        valign: egui::Align::BOTTOM,
    };
    let format_err = egui::TextFormat {
        underline: egui::Stroke {
            color: ui.style().visuals.error_fg_color,
            width: ui.style().visuals.text_cursor_width,
        },
        ..format_none.clone()
    };
    let format_run = egui::TextFormat {
        background: ui.style().visuals.faint_bg_color,
        ..format_none.clone()
    };
    let sections = vm
        .code_style
        .iter()
        .chain(std::iter::once(&(vm.code.len(), vm::CodeStyle::None)))
        .tuple_windows()
        .map(|((start, style), (end, _))| egui::text::LayoutSection {
            leading_space: 0.0,
            byte_range: *start..*end,
            format: match style {
                vm::CodeStyle::None => format_none.clone(),
                vm::CodeStyle::Error => format_err.clone(),
                vm::CodeStyle::Run => format_run.clone(),
            },
        })
        .collect::<Vec<_>>();
    let mut layouter = |ui: &egui::Ui, string: &str, _width: f32| {
        // TODO memoize to avoid to realloc on each frame
        let mut sections = sections.clone();
        // When the layouter is called, string may not be vm.code
        sections.last_mut().unwrap().byte_range.end = string.len();
        let job = egui::text::LayoutJob {
            text: string.to_string(),
            sections,
            wrap: egui::epaint::text::TextWrapping {
                max_width: 300.0,
                max_rows: 0,
                break_anywhere: true,
                overflow_character: Some('…'),
            },
            ..Default::default()
        };
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
                    vm.reset_style();
                    vm.style_range(0..vm.run_until, vm::CodeStyle::Run);
                    vm.sync_code();
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
                if let Some(ast) = vm.recompile() {
                    vm.run(ast);
                }
            }
            if ui.add(egui::Button::new("Check")).clicked() {
                log::debug!("Recompiling");
                vm.recompile();
            }
        });
    });
}
