use crate::data::Morphism;
use crate::vm::{EdgeLabel, VM};

pub fn faces(ui: &mut egui::Ui, vm: &mut VM) {
    let prev = vm.selected_face;

    egui::ScrollArea::vertical().show(ui, |ui| {
        ui.radio_value(&mut vm.selected_face, None, "No face");
        for fce in 0..vm.graph.faces.len() {
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
            on_path(
                &mut vm.graph.edges,
                vm.graph.faces[fce].start,
                &vm.graph.faces[fce].left,
                |lbl| {
                    lbl.style.left = false;
                },
            );
            on_path(
                &mut vm.graph.edges,
                vm.graph.faces[fce].start,
                &vm.graph.faces[fce].right,
                |lbl| {
                    lbl.style.right = false;
                },
            );
        }
        if let Some(fce) = vm.selected_face {
            on_path(
                &mut vm.graph.edges,
                vm.graph.faces[fce].start,
                &vm.graph.faces[fce].left,
                |lbl| {
                    lbl.style.left = true;
                },
            );
            on_path(
                &mut vm.graph.edges,
                vm.graph.faces[fce].start,
                &vm.graph.faces[fce].right,
                |lbl| {
                    lbl.style.right = true;
                },
            );
        }
    }
}

fn on_path<F>(
    edges: &mut Vec<Vec<(usize, EdgeLabel, Morphism)>>,
    mut node: usize,
    nexts: &[usize],
    f: F,
) where
    F: Fn(&mut EdgeLabel),
{
    for nid in 0..nexts.len() {
        let (dst, lbl, _) = &mut edges[node][nexts[nid]];
        f(lbl);
        node = *dst;
    }
}
