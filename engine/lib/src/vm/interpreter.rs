use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::asm::Instruction;
use crate::vm::{Interactive, VM};

pub struct InterpreterStatus {
    // Should the graph be re-layouted after the execution
    should_relayout: bool,
    // Should the graph be re-labeled after the execution
    should_relabel: bool,
}

impl InterpreterStatus {
    pub fn new() -> Self {
        Self {
            should_relayout: false,
            should_relabel: false,
        }
    }
}

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    // Execute the instruction and register it in the vm
    pub fn register_instruction(&mut self, ins: Instruction) {
        self.execute_instruction(&ins);
        self.ins.instructions.push(ins)
    }

    // Undo last instruction
    pub fn pop_instruction(&mut self) {
        if let Some(ins) = self.ins.instructions.pop() {
            self.undo_instruction(&ins)
        }
    }

    // Must be called everytime the proof assistant state is changed by the execution
    pub fn change_state(&mut self) {
        self.register_instruction(Instruction::DirtyState);
    }

    // Setup to do before executing instructions
    pub fn initialize_execution(&mut self) {
        self.ins.eval_status = InterpreterStatus::new();
        if let Some(face) = self.graph.selected_face {
            self.unshow_face(face)
        }
    }

    // After executing potentially multiple instruction, finish the evaluation
    // and prepare the vm for display
    pub fn finalize_execution(&mut self) {
        self.ctx.save_state();
        if self.ins.eval_status.should_relabel {
            self.relabel()
        }
        if self.ins.eval_status.should_relayout {
            self.graph
                .layout
                .particles_for_graph(&self.config, &mut self.graph.graph);
        }
        if let Some(face) = self.graph.selected_face {
            self.show_face(face)
        }
        self.recompute_face_statuses();
    }

    // Execute one instruction
    fn execute_instruction(&mut self, ins: &Instruction) {
        use Instruction::*;
        match ins {
            InsertNode(obj, cat) => {
                self.graph
                    .graph
                    .nodes
                    .push((*obj, *cat, Default::default()));
                self.autoname_node(self.graph.graph.nodes.len() - 1);
                self.graph.graph.edges.push(vec![]);
                self.ins.eval_status.should_relayout = true;
                self.ins.eval_status.should_relabel = true;
            }
            UpdateNode(nd, old, new) => {
                assert_eq!(self.graph.graph.nodes[*nd].0, *old);
                self.graph.graph.nodes[*nd].0 = new.clone();
                self.ins.eval_status.should_relabel = true;
            }
            UpdateNodeLabel(nd, upd) => {
                upd.apply(&mut self.graph.graph.nodes[*nd].2);
                self.ins.eval_status.should_relayout = true;
            }
            RenameNode(nd, prev, new) => {
                assert_eq!(&self.graph.graph.nodes[*nd].2.name, prev);
                self.graph.graph.nodes[*nd].2.name = new.clone();
                self.graph.names.remove(prev);
                self.graph.names.insert(new.clone(), GraphId::Node(*nd));
            }
            InsertMorphism(src, dst, mph, morph) => {
                self.graph.graph.edges[*src].push((
                    *dst,
                    Default::default(),
                    mph.clone(),
                    morph.clone(),
                ));
                self.autoname_morphism(*src, self.graph.graph.edges[*src].len() - 1);
                self.ins.eval_status.should_relayout = true;
                self.ins.eval_status.should_relabel = true;
            }
            UpdateMorphism(src, mph, old, new) => {
                assert_eq!(self.graph.graph.edges[*src][*mph].2, *old);
                self.graph.graph.edges[*src][*mph].2 = new.clone();
                self.ins.eval_status.should_relabel = true;
            }
            RelocateMorphismSrc(old_src, new_src, mph) => {
                self.graph.names.insert(
                    self.graph.graph.edges[*old_src]
                        .last()
                        .unwrap()
                        .1
                        .name
                        .clone(),
                    GraphId::Morphism(*old_src, *mph),
                );
                let edge = self.graph.graph.edges[*old_src].swap_remove(*mph);
                let new_mph = self.graph.graph.edges[*new_src].len();
                self.graph
                    .names
                    .insert(edge.1.name.clone(), GraphId::Morphism(*new_src, new_mph));
                self.graph.graph.edges[*new_src].push(edge);
                self.ins.eval_status.should_relayout = true;
            }
            RelocateMorphismDst(src, mph, old_dst, new_dst) => {
                assert_eq!(self.graph.graph.edges[*src][*mph].0, *old_dst);
                self.graph.graph.edges[*src][*mph].0 = *new_dst;
                self.ins.eval_status.should_relayout = true;
            }
            UpdateMorphismLabel(src, mph, upd) => {
                upd.apply(&mut self.graph.graph.edges[*src][*mph].1);
                self.ins.eval_status.should_relayout = true;
            }
            RenameMorphism(src, mph, prev, new) => {
                assert_eq!(&self.graph.graph.edges[*src][*mph].1.name, prev);
                self.graph.graph.edges[*src][*mph].1.name = new.clone();
                self.graph.names.remove(prev);
                self.graph
                    .names
                    .insert(new.clone(), GraphId::Morphism(*src, *mph));
            }
            InsertFace(fce) => {
                self.graph.graph.faces.push(fce.clone());
                let nfce = self.graph.graph.faces.len() - 1;
                self.autoname_face(nfce);
                if let Some(parent) = fce.label.parent {
                    self.graph.graph.faces[parent].label.children.push(nfce);
                    if let Some(sel) = self.graph.selected_face {
                        if sel == parent {
                            self.graph.selected_face = Some(self.graph.graph.faces.len() - 1);
                        }
                    }
                }
                self.set_face_status(nfce);
                self.order_new_face(nfce);
                self.ins.eval_status.should_relayout = true;
                self.ins.eval_status.should_relabel = true;
            }
            UpdateFace(fce, old, new) => {
                assert_eq!(self.graph.graph.faces[*fce].eq, *old);
                self.graph.graph.faces[*fce].eq = new.clone();
                self.set_face_status(*fce);
                self.ins.eval_status.should_relabel = true;
            }
            RelocateFaceSrc(fce, old, new) => {
                assert_eq!(self.graph.graph.faces[*fce].start, *old);
                self.graph.graph.faces[*fce].start = *new;
                self.ins.eval_status.should_relayout = true;
            }
            RelocateFaceDst(fce, old, new) => {
                assert_eq!(self.graph.graph.faces[*fce].end, *old);
                self.graph.graph.faces[*fce].end = *new;
                self.ins.eval_status.should_relayout = true;
            }
            RelocateFaceLeft(fce, old, new) => {
                assert_eq!(self.graph.graph.faces[*fce].left, *old);
                self.graph.graph.faces[*fce].left = new.clone();
                self.ins.eval_status.should_relayout = true;
            }
            RelocateFaceRight(fce, old, new) => {
                assert_eq!(self.graph.graph.faces[*fce].right, *old);
                self.graph.graph.faces[*fce].right = new.clone();
                self.ins.eval_status.should_relayout = true;
            }
            UpdateFaceLabel(fce, upd) => {
                upd.apply(&mut self.graph.graph.faces[*fce].label);
                self.ins.eval_status.should_relayout = true;
            }
            RenameFace(fce, prev, new) => {
                assert_eq!(&self.graph.graph.faces[*fce].label.name, prev);
                self.graph.graph.faces[*fce].label.name = new.clone();
                self.graph.names.remove(prev);
                self.graph.names.insert(new.clone(), GraphId::Face(*fce));
            }
            DirtyState => self.ins.eval_status.should_relabel = true,
        }
    }

    // Undo one instruction
    pub fn undo_instruction(&mut self, ins: &Instruction) {
        use Instruction::*;
        match ins {
            InsertNode(..) => {
                if let Some(nd) = self.graph.graph.nodes.pop() {
                    self.graph.names.remove(&nd.2.name);
                }
                self.graph.graph.edges.pop();
                self.ins.eval_status.should_relayout = true;
            }
            UpdateNode(nd, old, new) => {
                assert_eq!(self.graph.graph.nodes[*nd].0, *new);
                self.graph.graph.nodes[*nd].0 = old.clone();
                self.ins.eval_status.should_relabel = true;
            }
            UpdateNodeLabel(nd, upd) => {
                upd.undo(&mut self.graph.graph.nodes[*nd].2);
                self.ins.eval_status.should_relayout = true;
            }
            RenameNode(nd, prev, new) => {
                assert_eq!(&self.graph.graph.nodes[*nd].2.name, new);
                self.graph.graph.nodes[*nd].2.name = prev.clone();
                self.graph.names.remove(new);
                self.graph.names.insert(prev.clone(), GraphId::Node(*nd));
            }
            InsertMorphism(src, _, _, _) => {
                if let Some(mph) = self.graph.graph.edges[*src].pop() {
                    self.graph.names.remove(&mph.1.name);
                }
                self.ins.eval_status.should_relayout = true;
            }
            UpdateMorphism(src, mph, old, new) => {
                assert_eq!(self.graph.graph.edges[*src][*mph].2, *new);
                self.graph.graph.edges[*src][*mph].2 = old.clone();
                self.ins.eval_status.should_relabel = true;
            }
            RelocateMorphismSrc(old_src, new_src, mph) => {
                let mut edge = self.graph.graph.edges[*new_src].pop().unwrap();
                self.graph
                    .names
                    .insert(edge.1.name.clone(), GraphId::Morphism(*old_src, *mph));
                if *mph < self.graph.graph.edges[*old_src].len() {
                    std::mem::swap(&mut edge, &mut self.graph.graph.edges[*old_src][*mph]);
                    self.graph.names.insert(
                        edge.1.name.clone(),
                        GraphId::Morphism(*old_src, self.graph.graph.edges[*old_src].len()),
                    );
                }
                self.graph.graph.edges[*old_src].push(edge);
                self.ins.eval_status.should_relayout = true;
            }
            RelocateMorphismDst(src, mph, old_dst, new_dst) => {
                assert_eq!(self.graph.graph.edges[*src][*mph].0, *new_dst);
                self.graph.graph.edges[*src][*mph].0 = *old_dst;
                self.ins.eval_status.should_relayout = true;
            }
            UpdateMorphismLabel(src, mph, upd) => {
                upd.undo(&mut self.graph.graph.edges[*src][*mph].1);
                self.ins.eval_status.should_relayout = true;
            }
            RenameMorphism(src, mph, prev, new) => {
                assert_eq!(&self.graph.graph.edges[*src][*mph].1.name, new);
                self.graph.graph.edges[*src][*mph].1.name = prev.clone();
                self.graph.names.remove(new);
                self.graph
                    .names
                    .insert(prev.clone(), GraphId::Morphism(*src, *mph));
            }
            InsertFace(_) => {
                if let Some(fce) = self.graph.graph.faces.pop() {
                    if let Some(focused) = self.graph.selected_face {
                        if focused == self.graph.graph.faces.len() {
                            self.graph.selected_face = fce.label.parent;
                        }
                    }
                    self.graph.names.remove(&fce.label.name);
                    if let Some(parent) = fce.label.parent {
                        if self.graph.graph.faces[parent].label.name.is_empty()
                            && self.graph.graph.faces[parent].label.hidden
                        {
                            self.set_name(GraphId::Face(parent), fce.label.name);
                        }
                        self.graph.graph.faces[parent].label.children.pop();
                    }
                    self.order_rm_face(self.graph.graph.faces.len());
                }
                self.ins.eval_status.should_relayout = true;
            }
            UpdateFace(fce, old, new) => {
                assert_eq!(self.graph.graph.faces[*fce].eq, *new);
                self.graph.graph.faces[*fce].eq = old.clone();
                self.set_face_status(*fce);
                self.ins.eval_status.should_relabel = true;
            }
            RelocateFaceSrc(fce, old, new) => {
                assert_eq!(self.graph.graph.faces[*fce].start, *new);
                self.graph.graph.faces[*fce].start = *old;
                self.ins.eval_status.should_relayout = true;
            }
            RelocateFaceDst(fce, old, new) => {
                assert_eq!(self.graph.graph.faces[*fce].end, *new);
                self.graph.graph.faces[*fce].end = *old;
                self.ins.eval_status.should_relayout = true;
            }
            RelocateFaceLeft(fce, old, new) => {
                assert_eq!(self.graph.graph.faces[*fce].left, *new);
                self.graph.graph.faces[*fce].left = old.clone();
                self.ins.eval_status.should_relayout = true;
            }
            RelocateFaceRight(fce, old, new) => {
                assert_eq!(self.graph.graph.faces[*fce].right, *new);
                self.graph.graph.faces[*fce].right = old.clone();
                self.ins.eval_status.should_relayout = true;
            }
            UpdateFaceLabel(fce, upd) => {
                upd.undo(&mut self.graph.graph.faces[*fce].label);
                self.ins.eval_status.should_relayout = true;
            }
            RenameFace(fce, prev, new) => {
                assert_eq!(&self.graph.graph.faces[*fce].label.name, new);
                self.graph.graph.faces[*fce].label.name = prev.clone();
                self.graph.names.remove(new);
                self.graph.names.insert(prev.clone(), GraphId::Face(*fce));
            }
            DirtyState => self.ins.eval_status.should_relabel = true,
        }
    }
}
