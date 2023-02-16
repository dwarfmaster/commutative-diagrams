use crate::data::Morphism;
use crate::ui::{EdgeLabel, GraphDisplay};

pub fn faces(ui: &mut egui::Ui, gr: &mut GraphDisplay) {
    let prev = gr.selected_face;

    egui::ScrollArea::vertical().show(ui, |ui| {
        ui.radio_value(&mut gr.selected_face, None, "No face");
        for fce in 0..gr.graph.faces.len() {
            let label = &gr.graph.faces[fce].label.label;
            ui.radio_value(&mut gr.selected_face, Some(fce), label);
        }
    });

    if gr.selected_face != prev {
        if let Some(fce) = prev {
            on_path(
                &mut gr.graph.edges,
                gr.graph.faces[fce].start,
                &gr.graph.faces[fce].left,
                |lbl| {
                    lbl.style.left = false;
                },
            );
            on_path(
                &mut gr.graph.edges,
                gr.graph.faces[fce].start,
                &gr.graph.faces[fce].right,
                |lbl| {
                    lbl.style.right = false;
                },
            );
        }
        if let Some(fce) = gr.selected_face {
            on_path(
                &mut gr.graph.edges,
                gr.graph.faces[fce].start,
                &gr.graph.faces[fce].left,
                |lbl| {
                    lbl.style.left = true;
                },
            );
            on_path(
                &mut gr.graph.edges,
                gr.graph.faces[fce].start,
                &gr.graph.faces[fce].right,
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
