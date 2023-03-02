use crate::data::ActualMorphism;
use crate::vm;
use crate::vm::asm;
use crate::vm::graph::GraphId;
use crate::vm::graph::{EdgeLabel, FaceLabel, NodeLabel};
use crate::vm::VM;
use std::iter;
use std::ops::Deref;

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

fn left_upd(lbl: &FaceLabel, new: Vec<usize>) -> asm::Updater<FaceLabel> {
    let direct = move |v: &mut FaceLabel| {
        v.left = Some(new.clone());
    };
    let old = lbl.left.clone();
    let reverse = move |v: &mut FaceLabel| {
        v.left = old.clone();
    };
    asm::Updater {
        direct: Box::new(direct),
        reverse: Box::new(reverse),
    }
}

fn right_upd(lbl: &FaceLabel, new: Vec<usize>) -> asm::Updater<FaceLabel> {
    let direct = move |v: &mut FaceLabel| {
        v.right = Some(new.clone());
    };
    let old = lbl.right.clone();
    let reverse = move |v: &mut FaceLabel| {
        v.right = old.clone();
    };
    asm::Updater {
        direct: Box::new(direct),
        reverse: Box::new(reverse),
    }
}

impl VM {
    fn hide_node(&mut self, id: usize) {
        if self.graph.nodes[id].1.hidden {
            return;
        }

        self.register_instruction(Ins::UpdateNodeLabel(
            id,
            hidden_upd(&self.graph.nodes[id].1, true),
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
                hidden_upd(&self.graph.nodes[n].1, false),
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
                    hidden_upd(&self.graph.nodes[src].1, false),
                ));
                self.register_instruction(Ins::UpdateNodeLabel(
                    dst,
                    hidden_upd(&self.graph.nodes[dst].1, false),
                ));
            }
            Face(f) => self.register_instruction(Ins::UpdateFaceLabel(
                f,
                hidden_upd(&self.graph.faces[f].label, false),
            )),
        }
        self.layout()
    }

    // Hide a morphism, and create an alias for all faces that go through that
    // morphism
    pub fn hide_and_replace_morphism(&mut self, src: usize, mph: usize, rep_fce: usize) {
        assert_eq!(src, self.graph.faces[rep_fce].start);
        assert_eq!(self.graph.edges[src][mph].0, self.graph.faces[rep_fce].end);

        // Replacement
        {
            let rep = self.get_right_side(rep_fce).clone();
            let replace =
                |node: &mut usize, nxt: &usize| -> Option<Box<dyn Iterator<Item = usize>>> {
                    let (dst, _, _) = &self.graph.edges[*node][*nxt];
                    let prev = *node;
                    *node = *dst;
                    if prev == src && *nxt == mph {
                        Some(Box::new(rep.iter().map(|i| *i)))
                    } else {
                        Some(Box::new(iter::once(*nxt)))
                    }
                };
            for fce in 0..self.graph.faces.len() {
                let start = self.graph.faces[fce].start;
                let left = vm::get_left_side(&self.graph.faces, fce)
                    .iter()
                    .scan(start, replace)
                    .flatten()
                    .collect::<Vec<_>>();
                self.instructions.push(Ins::UpdateFaceLabel(
                    fce,
                    left_upd(&self.graph.faces[fce].label, left.clone()),
                ));
                self.graph.faces[fce].label.left = Some(left);

                let right = vm::get_right_side(&self.graph.faces, fce)
                    .iter()
                    .scan(start, replace)
                    .flatten()
                    .collect::<Vec<_>>();
                self.instructions.push(Ins::UpdateFaceLabel(
                    fce,
                    right_upd(&self.graph.faces[fce].label, right.clone()),
                ));
                self.graph.faces[fce].label.right = Some(right);
            }
        }

        // Hiding
        self.hide(GraphId::Morphism(src, mph))
    }

    // Hide all identities in the graph
    pub fn hide_identities(&mut self) {
        for src in 0..self.graph.edges.len() {
            for mph in 0..self.graph.edges[src].len() {
                if let ActualMorphism::Identity(_) = self.graph.edges[src][mph].2.deref() {
                    self.hide(GraphId::Morphism(src, mph));
                }
            }
        }
    }
}
