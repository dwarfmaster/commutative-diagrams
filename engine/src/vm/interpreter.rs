use crate::vm::asm::Instruction;
use crate::vm::VM;

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

impl VM {
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
            self.layout()
        }
        if let Some(face) = self.selected_face {
            self.show_face(face)
        }
    }

    // Execute one instruction
    fn execute_instruction(&mut self, ins: &Instruction) {
        use Instruction::*;
        self.eval_status.should_relayout = true;
        match ins {
            InsertNode(obj) => {
                self.graph.nodes.push((obj.clone(), Default::default()));
                self.graph.edges.push(vec![]);
            }
            UpdateNode(nd, old, new) => {
                assert_eq!(self.graph.nodes[*nd].0, *old);
                self.graph.nodes[*nd].0 = new.clone();
            }
            UpdateNodeLabel(nd, upd) => upd.apply(&mut self.graph.nodes[*nd].1),
            InsertMorphism(src, dst, mph) => {
                self.graph.edges[*src].push((*dst, Default::default(), mph.clone()))
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
            InsertFace(fce) => self.graph.faces.push(fce.clone()),
            UpdateFace(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].eq, *old);
                self.graph.faces[*fce].eq = new.clone();
            }
            UpdateFaceLabel(fce, upd) => upd.apply(&mut self.graph.faces[*fce].label),
            ExtendRefinements(sigma) => self.refinements.extend(sigma.iter().map(|p| p.clone())),
        }
    }

    // Undo one instruction
    fn undo_instruction(&mut self, ins: &Instruction) {
        use Instruction::*;
        self.eval_status.should_relayout = true;
        match ins {
            InsertNode(_) => {
                self.graph.nodes.pop();
                self.graph.edges.pop();
            }
            UpdateNode(nd, old, new) => {
                assert_eq!(self.graph.nodes[*nd].0, *new);
                self.graph.nodes[*nd].0 = old.clone();
            }
            UpdateNodeLabel(nd, upd) => upd.undo(&mut self.graph.nodes[*nd].1),
            InsertMorphism(src, _, _) => {
                self.graph.edges[*src].pop();
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
            InsertFace(_) => {
                self.graph.faces.pop();
            }
            UpdateFace(fce, old, new) => {
                assert_eq!(self.graph.faces[*fce].eq, *new);
                self.graph.faces[*fce].eq = old.clone();
            }
            UpdateFaceLabel(fce, upd) => upd.undo(&mut self.graph.faces[*fce].label),
            ExtendRefinements(sigma) => self
                .refinements
                .truncate(self.refinements.len().saturating_sub(sigma.len())),
        }
    }
}
