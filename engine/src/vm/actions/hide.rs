use crate::data::ActualMorphism;
use crate::vm;
use crate::vm::asm;
use crate::vm::graph::GraphId;
use crate::vm::VM;
use lens_rs::optics;
use std::iter;
use std::ops::Deref;

type Ins = asm::Instruction;

impl VM {
    fn hide_node(&mut self, id: usize) {
        if self.graph.nodes[id].1.hidden {
            return;
        }

        self.register_instruction(Ins::UpdateNodeLabel(
            id,
            asm::Updater::from_lens(&self.graph.nodes[id].1, optics!(hidden), true),
        ));
        for src in 0..self.graph.nodes.len() {
            for mph in 0..self.graph.edges[src].len() {
                if src == id || self.graph.edges[src][mph].0 == id {
                    self.register_instruction(Ins::UpdateMorphismLabel(
                        src,
                        mph,
                        asm::Updater::from_lens(
                            &self.graph.edges[src][mph].1,
                            optics!(hidden),
                            true,
                        ),
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
                    asm::Updater::from_lens(&self.graph.edges[src][mph].1, optics!(hidden), true),
                ));
            }
            Face(f) => {
                self.register_instruction(Ins::UpdateFaceLabel(
                    f,
                    asm::Updater::from_lens(&self.graph.faces[f].label, optics!(hidden), true),
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
                asm::Updater::from_lens(&self.graph.nodes[n].1, optics!(hidden), false),
            )),
            Morphism(src, mph) => {
                let dst = self.graph.edges[src][mph].0;
                self.register_instruction(Ins::UpdateMorphismLabel(
                    src,
                    mph,
                    asm::Updater::from_lens(&self.graph.edges[src][mph].1, optics!(hidden), false),
                ));
                self.register_instruction(Ins::UpdateNodeLabel(
                    src,
                    asm::Updater::from_lens(&self.graph.nodes[src].1, optics!(hidden), false),
                ));
                self.register_instruction(Ins::UpdateNodeLabel(
                    dst,
                    asm::Updater::from_lens(&self.graph.nodes[dst].1, optics!(hidden), false),
                ));
            }
            Face(f) => self.register_instruction(Ins::UpdateFaceLabel(
                f,
                asm::Updater::from_lens(&self.graph.faces[f].label, optics!(hidden), false),
            )),
        }
        self.layout()
    }

    // Hide a morphism, and create an alias for all faces that go through that
    // morphism
    pub fn hide_and_replace_morphism(&mut self, src: usize, mph: usize, rep_fce: usize) {
        assert_eq!(src, self.graph.faces[rep_fce].start);
        assert_eq!(self.graph.edges[src][mph].0, self.graph.faces[rep_fce].end);

        // Unshow face
        if let Some(fce) = self.selected_face {
            self.unshow_face(fce)
        }

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
                let left = Some(
                    vm::get_left_side(&self.graph.faces, fce)
                        .iter()
                        .scan(start, replace)
                        .flatten()
                        .collect(),
                );
                self.instructions.push(Ins::UpdateFaceLabel(
                    fce,
                    asm::Updater::from_lens(
                        &self.graph.faces[fce].label,
                        optics!(left),
                        left.clone(),
                    ),
                ));
                self.graph.faces[fce].label.left = left;

                let right = Some(
                    vm::get_right_side(&self.graph.faces, fce)
                        .iter()
                        .scan(start, replace)
                        .flatten()
                        .collect(),
                );
                self.instructions.push(Ins::UpdateFaceLabel(
                    fce,
                    asm::Updater::from_lens(
                        &self.graph.faces[fce].label,
                        optics!(right),
                        right.clone(),
                    ),
                ));
                self.graph.faces[fce].label.right = right;
            }
        }

        // Reshow-face
        if let Some(fce) = self.selected_face {
            self.show_face(fce)
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
