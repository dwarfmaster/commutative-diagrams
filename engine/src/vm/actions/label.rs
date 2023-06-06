use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::graph::{EdgeLabel, FaceLabel, LabelSource, NodeLabel};
use crate::vm::{Interactive, VM};
use hashconsing::HConsed;

type Ins = asm::Instruction;

fn should_render<T>(src: &LabelSource, obj: &HConsed<T>) -> bool {
    use LabelSource::*;
    match src {
        None => true,
        Render(id) => *id != obj.uid(),
        Manual => false,
    }
}

trait HasLabel {
    fn label<'a>(&'a mut self) -> &'a mut String;
    fn get_label(&self) -> String;
    fn label_source<'a>(&'a mut self) -> &'a mut LabelSource;
    fn get_label_source(&self) -> LabelSource;
}
macro_rules! derive_has_label {
    ($t:ty) => {
        impl HasLabel for $t {
            fn label<'a>(&'a mut self) -> &'a mut String {
                &mut self.label
            }
            fn get_label(&self) -> String {
                self.label.clone()
            }
            fn label_source<'a>(&'a mut self) -> &'a mut LabelSource {
                &mut self.label_source
            }
            fn get_label_source(&self) -> LabelSource {
                self.label_source.clone()
            }
        }
    };
}
derive_has_label!(NodeLabel);
derive_has_label!(EdgeLabel);
derive_has_label!(FaceLabel);

fn label_upd<Label: HasLabel>(uid: u64, label: &Label, text: String) -> asm::Updater<Label> {
    let direct = move |lbl: &mut Label| {
        *lbl.label() = text.clone();
        *lbl.label_source() = LabelSource::Render(uid);
    };
    let prev_label = label.get_label();
    let prev_label_source = label.get_label_source();
    let reverse = move |lbl: &mut Label| {
        *lbl.label() = prev_label.clone();
        *lbl.label_source() = prev_label_source.clone();
    };
    asm::Updater {
        direct: Box::new(direct),
        reverse: Box::new(reverse),
    }
}

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
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
