use crate::graph::GraphId;
use crate::vm::asm::Instruction;
use crate::vm::{Interactive, VM};

pub struct InterpreterStatus {
    should_relayout: bool,
}

impl InterpreterStatus {
    pub fn new() -> Self {
        Self {
            should_relayout: false,
        }
    }
}

impl<I: Interactive + Sync + Send> VM<I> {
    // Execute the instruction and register it in the vm
    pub fn register_instruction(&mut self, ins: Instruction) {
        self.execute_instruction(&ins);
        self.instructions.push(ins)
    }

    // Undo last instruction
    pub fn pop_instruction(&mut self) {
        if let Some(ins) = self.instructions.pop() {
            self.undo_instruction(&ins)
        }
    }

    // Setup to do before executing instructions
    pub fn initialize_execution(&mut self) {
        self.eval_status.should_relayout = false;
        if let Some(face) = self.selected_face {
            self.unshow_face(face)
        }
    }

    // After executing potentially multiple instruction, finish the evaluation
    // and prepare the vm for display
    pub fn finalize_execution(&mut self) {
        if self.eval_status.should_relayout {
            Self::layout(&mut self.graph);
        }
        if let Some(face) = self.selected_face {
            self.show_face(face)
        }
    }

    // Log current state of refinements
    fn log_debug_refinements(&self, extend: bool) {
        log::trace!("{} refinements:", if extend { "Extend" } else { "Retract" });
        for (ex, tm) in &self.refinements {
            log::trace!("  {} => {}", ex, tm.render(&self.ctx, 100));
        }
    }

    // Execute one instruction
    fn execute_instruction(&mut self, ins: &Instruction) {
        use Instruction::*;
        self.eval_status.should_relayout = true;
        match ins {
            InsertNode(obj) => {
                self.graph.nodes.push((obj.clone(), Default::default()));
                self.autoname_node(self.graph.nodes.len() - 1);
                self.graph.edges.push(vec![]);
            }
            UpdateNode(nd, old, new) => {
                assert_eq!(self.graph.nodes[*nd].0, *old);
                self.graph.nodes[*nd].0 = new.clone();
            }
            UpdateNodeLabel(nd, upd) => upd.apply(&mut self.graph.nodes[*nd].1),
            RenameNode(nd, prev, new) => {
                assert_eq!(&self.graph.nodes[*nd].1.name, prev);
                self.graph.nodes[*nd].1.name = new.clone();
                self.names.remove(prev);
                self.names.insert(new.clone(), GraphId::Node(*nd));
            }
            InsertMorphism(src, dst, mph) => {
                self.graph.edges[*src].push((*dst, Default::default(), mph.clone()));
                self.autoname_morphism(*src, self.graph.edges[*src].len() - 1);
            }
            UpdateMorphism(src, mph, old, new) => {
                assert_eq!(self.graph.edges[*src][*mph].2, *old);
                self.graph.edges[*src][*mph].2 = new.clone();
            }
            RelocateMorphismSrc(old_src, new_src, mph) => {
                let edge = self.graph.edges[*old_src].swap_remove(*mph);
                self.graph.edges[*new_src].push(edge);
            }
            RelocateMorphismDst(src, mph, old_dst, new_dst) => {
                assert_eq!(self.graph.edges[*src][*mph].0, *old_dst);
                self.graph.edges[*src][*mph].0 = *new_dst;
            }
            UpdateMorphismLabel(src, mph, upd) => upd.apply(&mut self.graph.edges[*src][*mph].1),
            RenameMorphism(src, mph, prev, new) => {
                assert_eq!(&self.graph.edges[*src][*mph].1.name, prev);
                self.graph.edges[*src][*mph].1.name = new.clone();
                self.names.remove(prev);
                self.names
                    .insert(new.clone(), GraphId::Morphism(*src, *mph));
            }
            InsertFace(fce) => {
                self.graph.faces.push(fce.clone());
                let nfce = self.graph.faces.len() - 1;
                self.autoname_face(nfce);
                if let Some(parent) = fce.label.parent {
                    self.graph.faces[parent].label.children.push(nfce);
                    if let Some(sel) = self.selected_face {
                        if sel == parent {
                            self.selected_face = Some(self.graph.faces.len() - 1);
                        }
                    }
                }
                self.set_face_status(nfce);
                self.order_new_face(nfce);
            }
            UpdateFace(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].eq, *old);
                self.graph.faces[*fce].eq = new.clone();
                self.set_face_status(*fce);
            }
            RelocateFaceSrc(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].start, *old);
                self.graph.faces[*fce].start = *new;
            }
            RelocateFaceDst(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].end, *old);
                self.graph.faces[*fce].end = *new;
            }
            RelocateFaceLeft(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].left, *old);
                self.graph.faces[*fce].left = new.clone();
            }
            RelocateFaceRight(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].right, *old);
                self.graph.faces[*fce].right = new.clone();
            }
            UpdateFaceLabel(fce, upd) => upd.apply(&mut self.graph.faces[*fce].label),
            ExtendRefinements(sigma) => {
                self.refinements.extend(sigma.iter().map(|p| p.clone()));
                self.log_debug_refinements(true);
            }
            RenameFace(fce, prev, new) => {
                assert_eq!(&self.graph.faces[*fce].label.name, prev);
                self.graph.faces[*fce].label.name = new.clone();
                self.names.remove(prev);
                self.names.insert(new.clone(), GraphId::Face(*fce));
            }
        }
    }

    // Undo one instruction
    fn undo_instruction(&mut self, ins: &Instruction) {
        use Instruction::*;
        self.eval_status.should_relayout = true;
        match ins {
            InsertNode(_) => {
                if let Some(nd) = self.graph.nodes.pop() {
                    self.names.remove(&nd.1.name);
                }
                self.graph.edges.pop();
            }
            UpdateNode(nd, old, new) => {
                assert_eq!(self.graph.nodes[*nd].0, *new);
                self.graph.nodes[*nd].0 = old.clone();
            }
            UpdateNodeLabel(nd, upd) => upd.undo(&mut self.graph.nodes[*nd].1),
            RenameNode(nd, prev, new) => {
                assert_eq!(&self.graph.nodes[*nd].1.name, new);
                self.graph.nodes[*nd].1.name = prev.clone();
                self.names.remove(new);
                self.names.insert(prev.clone(), GraphId::Node(*nd));
            }
            InsertMorphism(src, _, _) => {
                if let Some(mph) = self.graph.edges[*src].pop() {
                    self.names.remove(&mph.1.name);
                }
            }
            UpdateMorphism(src, mph, old, new) => {
                assert_eq!(self.graph.edges[*src][*mph].2, *new);
                self.graph.edges[*src][*mph].2 = old.clone();
            }
            RelocateMorphismSrc(old_src, new_src, mph) => {
                let mut edge = self.graph.edges[*new_src].pop().unwrap();
                if *mph < self.graph.edges[*old_src].len() {
                    std::mem::swap(&mut edge, &mut self.graph.edges[*old_src][*mph]);
                }
                self.graph.edges[*old_src].push(edge);
            }
            RelocateMorphismDst(src, mph, old_dst, new_dst) => {
                assert_eq!(self.graph.edges[*src][*mph].0, *new_dst);
                self.graph.edges[*src][*mph].0 = *old_dst;
            }
            UpdateMorphismLabel(src, mph, upd) => upd.undo(&mut self.graph.edges[*src][*mph].1),
            RenameMorphism(src, mph, prev, new) => {
                assert_eq!(&self.graph.edges[*src][*mph].1.name, new);
                self.graph.edges[*src][*mph].1.name = prev.clone();
                self.names.remove(new);
                self.names
                    .insert(prev.clone(), GraphId::Morphism(*src, *mph));
            }
            InsertFace(_) => {
                if let Some(fce) = self.graph.faces.pop() {
                    if let Some(focused) = self.selected_face {
                        if focused == self.graph.faces.len() {
                            self.selected_face = fce.label.parent;
                        }
                    }
                    self.names.remove(&fce.label.name);
                    if let Some(parent) = fce.label.parent {
                        self.set_name(GraphId::Face(parent), fce.label.name);
                        self.graph.faces[parent].label.children.pop();
                    }
                    self.order_rm_face(self.graph.faces.len());
                }
            }
            UpdateFace(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].eq, *new);
                self.graph.faces[*fce].eq = old.clone();
                self.set_face_status(*fce);
            }
            RelocateFaceSrc(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].start, *new);
                self.graph.faces[*fce].start = *old;
            }
            RelocateFaceDst(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].end, *new);
                self.graph.faces[*fce].end = *old;
            }
            RelocateFaceLeft(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].left, *new);
                self.graph.faces[*fce].left = old.clone();
            }
            RelocateFaceRight(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].right, *new);
                self.graph.faces[*fce].right = old.clone();
            }
            UpdateFaceLabel(fce, upd) => upd.undo(&mut self.graph.faces[*fce].label),
            ExtendRefinements(sigma) => {
                self.refinements
                    .truncate(self.refinements.len().saturating_sub(sigma.len()));
                self.log_debug_refinements(false);
            }
            RenameFace(fce, prev, new) => {
                assert_eq!(&self.graph.faces[*fce].label.name, new);
                self.graph.faces[*fce].label.name = prev.clone();
                self.names.remove(new);
                self.names.insert(prev.clone(), GraphId::Face(*fce));
            }
        }
    }
}
