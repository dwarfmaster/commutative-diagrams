use crate::vm::{FaceStatus, VM};

pub fn faces(ui: &mut egui::Ui, vm: &mut VM) {
    let prev = vm.selected_face;

    egui::ScrollArea::vertical().show(ui, |ui| {
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
                FaceStatus::Goal => {
                    ui.visuals_mut().override_text_color = Some(egui::Color32::GOLD)
                }
                FaceStatus::Refined => {
                    ui.visuals_mut().override_text_color = Some(egui::Color32::GREEN)
                }
                FaceStatus::Hypothesis => ui.visuals_mut().override_text_color = None,
            }

            let resp = ui.radio_value(&mut vm.selected_face, Some(fce), label);
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
