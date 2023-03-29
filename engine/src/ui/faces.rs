use crate::vm::{FaceStatus, VM};

pub fn faces(ui: &mut egui::Ui, vm: &mut VM) {
    let prev = vm.selected_face;

    egui::ScrollArea::vertical().show(ui, |ui| {
        ui.radio_value(&mut vm.selected_face, None, "No face");
        for fce in vm
            .face_goal_order
            .iter()
            .copied()
            .chain(vm.face_hyps_order.iter().copied())
        {
            if vm.graph.faces[fce].label.hidden {
                continue;
            }
            let label = format!(
                "{}[{}]: {}",
                vm.graph.faces[fce].label.name, fce, &vm.graph.faces[fce].label.label
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
            ui.radio_value(&mut vm.selected_face, Some(fce), label);
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
