use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::graph::{EdgeLabel, FaceLabel, NodeLabel};
use crate::vm::{Interactive, VM};

type Ins = asm::Instruction;

trait HasHidden {
    fn hidden<'a>(&'a mut self) -> &'a mut bool;
    fn is_hidden(&self) -> bool;
}
macro_rules! derive_has_hidden {
    ($t:ty) => {
        impl HasHidden for $t {
            fn hidden<'a>(&'a mut self) -> &'a mut bool {
                &mut self.hidden
            }
            fn is_hidden(&self) -> bool {
                self.hidden
            }
        }
    };
}
derive_has_hidden!(NodeLabel);
derive_has_hidden!(EdgeLabel);
derive_has_hidden!(FaceLabel);

fn hidden_upd<T: HasHidden + Clone>(val: &T, new: bool) -> asm::Updater<T> {
    let direct = move |v: &mut T| {
        *v.hidden() = new;
    };
    let old = val.is_hidden();
    let reverse = move |v: &mut T| {
        *v.hidden() = old;
    };
    asm::Updater {
        direct: Box::new(direct),
        reverse: Box::new(reverse),
    }
}

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    fn hide_node(&mut self, id: usize) {
        if self.graph.nodes[id].2.hidden {
            return;
        }

        self.register_instruction(Ins::UpdateNodeLabel(
            id,
            hidden_upd(&self.graph.nodes[id].2, true),
        ));
        for src in 0..self.graph.nodes.len() {
            for mph in 0..self.graph.edges[src].len() {
                if src == id || self.graph.edges[src][mph].0 == id {
                    self.register_instruction(Ins::UpdateMorphismLabel(
                        src,
                        mph,
                        hidden_upd(&self.graph.edges[src][mph].1, true),
                    ));
                }
            }
        }
    }

    // When hiding a node, hide all adjacent edges
    pub fn hide(&mut self, id: GraphId) {
        use GraphId::*;
        match id {
            Node(n) => self.hide_node(n),
            Morphism(src, mph) => {
                self.register_instruction(Ins::UpdateMorphismLabel(
                    src,
                    mph,
                    hidden_upd(&self.graph.edges[src][mph].1, true),
                ));
            }
            Face(f) => {
                self.register_instruction(Ins::UpdateFaceLabel(
                    f,
                    hidden_upd(&self.graph.faces[f].label, true),
                ));
            }
        }
    }

    // When revealing an edge, reveal its source and target nodes
    pub fn reveal(&mut self, id: GraphId) {
        use GraphId::*;
        match id {
            Node(n) => self.register_instruction(Ins::UpdateNodeLabel(
                n,
                hidden_upd(&self.graph.nodes[n].2, false),
            )),
            Morphism(src, mph) => {
                let dst = self.graph.edges[src][mph].0;
                self.register_instruction(Ins::UpdateMorphismLabel(
                    src,
                    mph,
                    hidden_upd(&self.graph.edges[src][mph].1, false),
                ));
                self.register_instruction(Ins::UpdateNodeLabel(
                    src,
                    hidden_upd(&self.graph.nodes[src].2, false),
                ));
                self.register_instruction(Ins::UpdateNodeLabel(
                    dst,
                    hidden_upd(&self.graph.nodes[dst].2, false),
                ));
            }
            Face(f) => self.register_instruction(Ins::UpdateFaceLabel(
                f,
                hidden_upd(&self.graph.faces[f].label, false),
            )),
        }
    }
}
