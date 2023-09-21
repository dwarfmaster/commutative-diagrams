use crate::remote::Remote;
use crate::ui::VM;
use crate::vm;
use itertools::Itertools;

pub fn code<Rm: Remote + Sync + Send>(ctx: &egui::Context, vm: &mut VM<Rm>) {
    if vm.code_window_open {
        let mut open = vm.code_window_open;
        egui::Window::new("Script")
            .open(&mut open)
            .show(ctx, |ui| code_impl(ui, vm));
        vm.code_window_open = open;
    }
}

fn code_impl<Rm: Remote + Sync + Send>(ui: &mut egui::Ui, vm: &mut VM<Rm>) {
    ui.with_layout(egui::Layout::top_down_justified(egui::Align::RIGHT), |ui| {
        code_text_box(ui, vm, 150.0, true);
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

pub fn code_text_box<Rm: Remote + Sync + Send>(
    ui: &mut egui::Ui,
    vm: &mut VM<Rm>,
    height: f32,
    format: bool,
) {
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
        background: ui.style().visuals.code_bg_color,
        ..format_none.clone()
    };

    let sections = if format {
        vm.code_style
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
            .collect::<Vec<_>>()
    } else {
        vec![egui::text::LayoutSection {
            leading_space: 0.0,
            byte_range: 0..vm.code.len(),
            format: format_none,
        }]
    };

    let mut layouter = |ui: &egui::Ui, string: &str, _width: f32| {
        // TODO memoize to avoid to realloc on each frame
        let sections = sections
            .iter()
            .filter_map(|sec| {
                if sec.byte_range.start >= string.len() {
                    None
                } else if sec.byte_range.end > string.len() {
                    Some(egui::text::LayoutSection {
                        byte_range: sec.byte_range.start..string.len(),
                        ..sec.clone()
                    })
                } else {
                    Some(sec.clone())
                }
            })
            .collect();
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
        ui.fonts(|f| f.layout_job(job))
    };

    egui::ScrollArea::vertical()
        .auto_shrink([false, false])
        .max_height(ui.available_height() - height)
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
}
