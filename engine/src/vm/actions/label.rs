use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::graph::{EdgeLabel, FaceLabel, NodeLabel};
use crate::vm::{Interactive, VM};

type Ins = asm::Instruction;

trait HasLabel {
    fn label<'a>(&'a mut self) -> &'a mut String;
    fn get_label(&self) -> String;
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
        }
    };
}
derive_has_label!(NodeLabel);
derive_has_label!(EdgeLabel);
derive_has_label!(FaceLabel);

fn label_upd<Label: HasLabel>(label: &Label, text: String) -> asm::Updater<Label> {
    let direct = move |lbl: &mut Label| {
        *lbl.label() = text.clone();
    };
    let prev_label = label.get_label();
    let reverse = move |lbl: &mut Label| {
        *lbl.label() = prev_label.clone();
    };
    asm::Updater {
        direct: Box::new(direct),
        reverse: Box::new(reverse),
    }
}

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    pub fn relabel(&mut self) {
        for id in 0..self.graph.nodes.len() {
            let (nd, _, lbl) = &self.graph.nodes[id];
            let nlbl = self.ctx.get_stored_label(*nd);
            self.register_instruction(Ins::UpdateNodeLabel(id, label_upd(lbl, nlbl)));
        }
        for src in 0..self.graph.nodes.len() {
            for mph_id in 0..self.graph.edges[src].len() {
                let (_, lbl, mph) = &self.graph.edges[src][mph_id];
                let nlbl = self.ctx.get_stored_label(*mph);
                self.register_instruction(Ins::UpdateMorphismLabel(
                    src,
                    mph_id,
                    label_upd(lbl, nlbl),
                ));
            }
        }
        for id in 0..self.graph.faces.len() {
            let fce = &self.graph.faces[id];
            let nlbl = "<<todo>>".to_string(); // todo!
            self.register_instruction(Ins::UpdateFaceLabel(id, label_upd(&fce.label, nlbl)));
        }
    }
}
