use crate::vm::{FaceStatus, VM};

pub fn faces(ui: &mut egui::Ui, vm: &mut VM) {
    let prev = vm.selected_face;

    ui.with_layout(egui::Layout::top_down(egui::Align::LEFT), |ui| {
        egui::ScrollArea::vertical()
            .auto_shrink([false, false])
            .max_height(ui.available_height() - 50.0)
            .show(ui, |ui| faces_menu(ui, vm));

        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            if ui.add(egui::Button::new("End")).clicked() {
                vm.insert_and_run("succeed")
            }
            if ui.add(egui::Button::new("Fail")).clicked() {
                vm.insert_and_run("fail")
            }
        });
    });

    if vm.selected_face != prev {
        if let Some(fce) = prev {
            vm.unshow_face(fce);
        }
        if let Some(fce) = vm.selected_face {
            vm.show_face(fce);
        }
    }
}

fn faces_menu(ui: &mut egui::Ui, vm: &mut VM) {
    ui.radio_value(&mut vm.selected_face, None, "No face");
    let len = vm.face_goal_order.len() + vm.face_hyps_order.len();
    for id in 0..len {
        let fce = if id >= vm.face_goal_order.len() {
            vm.face_hyps_order[id - vm.face_goal_order.len()]
        } else {
            vm.face_goal_order[id]
        };

        if vm.graph.faces[fce].label.hidden {
            continue;
        }
        let label = format!(
            "{}: {}",
            vm.graph.faces[fce].label.name, &vm.graph.faces[fce].label.label
        );
        match vm.graph.faces[fce].label.status {
            FaceStatus::Goal => ui.visuals_mut().override_text_color = Some(egui::Color32::GOLD),
            FaceStatus::Refined => {
                ui.visuals_mut().override_text_color = Some(egui::Color32::GREEN)
            }
            FaceStatus::Hypothesis => ui.visuals_mut().override_text_color = None,
        }

        let resp = ui.radio_value(&mut vm.selected_face, Some(fce), label);
        if resp.double_clicked() {
            vm.insert_and_run(&format!("solve {}", vm.graph.faces[fce].label.name));
        }
        resp.context_menu(|ui| {
            let mut empty = true;
            if vm.graph.faces[fce].label.status == FaceStatus::Goal {
                empty = false;
                if ui.button("Solve").clicked() {
                    vm.insert_and_run(&format!("solve {}", vm.graph.faces[fce].label.name));
                    ui.close_menu();
                }
                if ui.button("Shrink").clicked() {
                    vm.insert_and_run(&format!("shrink {}", vm.graph.faces[fce].label.name));
                    ui.close_menu();
                }
                if ui.button("Pull").clicked() {
                    vm.insert_and_run(&format!("pull {}, *", vm.graph.faces[fce].label.name));
                    ui.close_menu();
                }
                if ui.button("Push").clicked() {
                    vm.insert_and_run(&format!("push {}, *", vm.graph.faces[fce].label.name));
                    ui.close_menu();
                }
            }
            if empty {
                ui.close_menu();
            }
        });
    }
}
