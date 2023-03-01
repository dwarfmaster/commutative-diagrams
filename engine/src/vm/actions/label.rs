use crate::vm::asm;
use crate::vm::graph::{GraphId, LabelSource};
use crate::vm::VM;
use hashconsing::HConsed;
use lens_rs::{optics, LensMut, Optics};

type Ins = asm::Instruction;

fn should_render<T>(src: &LabelSource, obj: &HConsed<T>) -> bool {
    use LabelSource::*;
    match src {
        None => true,
        Render(id) => *id != obj.uid(),
        Manual => false,
    }
}

fn label_upd<Label>(uid: u64, label: &Label, text: String) -> asm::Updater<Label>
where
    Label: LensMut<Optics![label], String> + LensMut<Optics![label_source], LabelSource>,
{
    let direct = move |lbl: &mut Label| {
        *lbl.view_mut(optics!(label)) = text.clone();
        *lbl.view_mut(optics!(label_source)) = LabelSource::Render(uid);
    };
    let prev_label = label.view_ref(optics!(label)).clone();
    let prev_label_source = label.view_ref(optics!(label_source)).clone();
    let reverse = move |lbl: &mut Label| {
        *lbl.view_mut(optics!(label)) = prev_label.clone();
        *lbl.view_mut(optics!(label_source)) = prev_label_source.clone();
    };
    asm::Updater {
        direct: Box::new(direct),
        reverse: Box::new(reverse),
    }
}

impl VM {
    pub fn relabel(&mut self) {
        for id in 0..self.graph.nodes.len() {
            let (nd, lbl) = &self.graph.nodes[id];
            if should_render(&lbl.label_source, &nd) {
                let nlbl = nd.render(&mut self.ctx, 100);
                self.register_instruction(Ins::UpdateNodeLabel(id, label_upd(nd.uid(), lbl, nlbl)));
            }
        }
        for src in 0..self.graph.nodes.len() {
            for mph_id in 0..self.graph.edges[src].len() {
                let (_, lbl, mph) = &self.graph.edges[src][mph_id];
                if should_render(&lbl.label_source, &mph) {
                    let nlbl = mph.render(&mut self.ctx, 100);
                    self.register_instruction(Ins::UpdateMorphismLabel(
                        src,
                        mph_id,
                        label_upd(mph.uid(), lbl, nlbl),
                    ));
                }
            }
        }
        for id in 0..self.graph.faces.len() {
            let fce = &self.graph.faces[id];
            if should_render(&fce.label.label_source, &fce.eq) {
                let nlbl = fce.eq.render(&mut self.ctx, 100);
                self.register_instruction(Ins::UpdateFaceLabel(
                    id,
                    label_upd(fce.eq.uid(), &fce.label, nlbl),
                ));
            }
        }
    }
}
