use crate::vm::VM;

pub fn faces(ui: &mut egui::Ui, vm: &mut VM) {
    let prev = vm.selected_face;

    egui::ScrollArea::vertical().show(ui, |ui| {
        ui.radio_value(&mut vm.selected_face, None, "No face");
        for fce in 0..vm.graph.faces.len() {
            if vm.graph.faces[fce].label.hidden {
                continue;
            }
            let label = format!(
                "{}[{}]: {}",
                vm.graph.faces[fce]
                    .label
                    .name
                    .as_ref()
                    .unwrap_or(&"".to_string()),
                fce,
                &vm.graph.faces[fce].label.label
            );
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
