use crate::data::{Feature, Tag};
use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::ast;
use crate::vm::ast::Action;
use crate::vm::vm;
use crate::vm::{CodeStyle, EndStatus, Interactive, VM};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ExecutionResult {
    Success,
    Failure,
    ExecutionError,
    Unfinished,
}

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    fn execute(&mut self, act: ast::Annot<ast::Action>) -> ExecutionResult {
        use Action::*;
        use ExecutionResult::*;
        let mut result = Unfinished;
        let start = self.instructions.len();
        match act.value.clone() {
            InsertNode(node) => {
                let node = self.ctx.remote.parse(node.value.clone()).unwrap();
                match node {
                    Ok(node) => {
                        let tps = self.ctx.get_stored_query(node, Tag::Object);
                        for tp in tps {
                            if let Feature::Object { cat } = tp {
                                self.insert_node(node, cat);
                            }
                        }
                    }
                    Err(err) => {
                        self.error_msg = format!("Couldn't parse object: {:#?}", err);
                        result = ExecutionError;
                    }
                }
            }
            InsertMorphism(mph) => {
                let mph = self.ctx.remote.parse(mph.value.clone()).unwrap();
                match mph {
                    Ok(mph) => {
                        let tps = self.ctx.get_stored_query(mph, Tag::Morphism);
                        for tp in tps {
                            if let Feature::Morphism {
                                cat,
                                src: _,
                                dst: _,
                            } = tp
                            {
                                self.insert_mph(mph, cat);
                            }
                        }
                    }
                    Err(err) => {
                        self.error_msg = format!("Couldn't parse morphism: {:#?}", err);
                        result = ExecutionError;
                    }
                }
            }
            InsertMorphismAt(node, mph) => {
                let mph = self.ctx.remote.parse(mph.value.clone()).unwrap();
                match mph {
                    Ok(mph) => {
                        if let Some(GraphId::Node(id)) = self.names.get(&node.value) {
                            self.insert_mph_at(*id, mph);
                        } else {
                            self.error_msg = format!("{} is not a valid node name", node.value);
                            result = ExecutionError;
                        }
                    }
                    Err(err) => {
                        self.error_msg = format!("Couldn't parse morphism: {:#?}", err);
                        result = ExecutionError;
                    }
                }
            }
            Split(mph) => {
                if let Some(GraphId::Morphism(src, mph)) = self.names.get(&mph.value) {
                    self.split(*src, *mph)
                } else {
                    self.error_msg = format!("{} is not a valid morphism name", mph.value);
                    result = ExecutionError;
                }
            }
            HideNode(n) => {
                if let Some(GraphId::Node(n)) = self.names.get(&n.value) {
                    self.hide(GraphId::Node(*n))
                } else {
                    self.error_msg = format!("{} is not a valid node name", n.value);
                    result = ExecutionError;
                }
            }
            RevealNode(n) => {
                if let Some(GraphId::Node(n)) = self.names.get(&n.value) {
                    self.reveal(GraphId::Node(*n))
                } else {
                    self.error_msg = format!("{} is not a valid node name", n.value);
                    result = ExecutionError;
                }
            }
            HideMorphism(m) => {
                if let Some(GraphId::Morphism(s, m)) = self.names.get(&m.value) {
                    self.hide(GraphId::Morphism(*s, *m))
                } else {
                    self.error_msg = format!("{} is not a valid morphism name", m.value);
                    result = ExecutionError;
                }
            }
            RevealMorphism(m) => {
                if let Some(GraphId::Morphism(s, m)) = self.names.get(&m.value) {
                    self.reveal(GraphId::Morphism(*s, *m))
                } else {
                    self.error_msg = format!("{} is not a valid morphism name", m.value);
                    result = ExecutionError;
                }
            }
            HideFace(f) => {
                if let Some(GraphId::Face(f)) = self.names.get(&f.value) {
                    self.hide(GraphId::Face(*f))
                } else {
                    self.error_msg = format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            RevealFace(f) => {
                if let Some(GraphId::Face(f)) = self.names.get(&f.value) {
                    self.reveal(GraphId::Face(*f))
                } else {
                    self.error_msg = format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            Solve(_size, f) => {
                if let Some(GraphId::Face(_f)) = self.names.get(&f.value) {
                    // if !self.solve_face(f, size.map(|a| a.value).unwrap_or(5)) {
                    //     self.error_msg = format!("Couldn't solve face {}", f);
                    //     result = ExecutionError;
                    // }
                    todo!()
                } else {
                    self.error_msg = format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            PullFace(f, size) => {
                if let Some(GraphId::Face(f)) = self.names.get(&f.value) {
                    if !self.shrink(*f, Some(0), size) {
                        self.error_msg = "Couldn't pull previous face".to_string();
                        result = ExecutionError;
                    }
                } else {
                    self.error_msg = format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            PushFace(f, size) => {
                if let Some(GraphId::Face(f)) = self.names.get(&f.value) {
                    if !self.shrink(*f, size, Some(0)) {
                        self.error_msg = "Couldn't push previous face".to_string();
                        result = ExecutionError;
                    }
                } else {
                    self.error_msg = format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            ShrinkFace(f) => {
                if let Some(GraphId::Face(f)) = self.names.get(&f.value) {
                    if !self.shrink(*f, None, None) {
                        self.error_msg = "Couldn't shrink previous face".to_string();
                        result = ExecutionError;
                    }
                } else {
                    self.error_msg = format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            Lemma(_lem, _matching) => {
                todo!()
                // let lemma = self.find_lemma(&lem.value);
                // if let Some(lemma) = lemma {
                //     let matching = matching
                //         .iter()
                //         .map(|(lem, goal)| {
                //             let lemid = match &lem.value {
                //                 ast::Id::Name(name) => {
                //                     self.lemmas[lemma].graphical_state.names.get(name)
                //                 }
                //                 ast::Id::Id(_) => None,
                //             };
                //             let goalid = match &goal.value {
                //                 ast::Id::Name(name) => self.names.get(name),
                //                 ast::Id::Id(_) => None,
                //             };
                //             match (lemid, goalid) {
                //                 (Some(lem), Some(goal)) => Ok((lem.clone(), goal.clone())),
                //                 (None, _) => {
                //                     Err(format!("Couldn't find {:#?} in lemma", lem.value))
                //                 }
                //                 _ => Err(format!("Couldn't find {:#?} in goal", goal.value)),
                //             }
                //         })
                //         .collect::<Result<Vec<_>, String>>();
                //     match matching {
                //         Ok(matching) => {
                //             if !self.apply_lemma(lemma, &matching[..]) {
                //                 self.error_msg =
                //                     "Couldn't compute the lemma application".to_string();
                //                 result = ExecutionError;
                //             }
                //         }
                //         Err(msg) => {
                //             self.error_msg = msg;
                //             result = ExecutionError;
                //         }
                //     }
                // } else {
                //     self.error_msg = format!("Couldn't find lemma {}", lem.value);
                //     result = ExecutionError;
                // }
            }
            Refine(_, _) => todo!(),
            Succeed => result = Success,
            Fail => result = Failure,
        }
        if result == ExecutionError {
            // Undo all new instructions
            let tail = self.instructions.split_off(start);
            for _ in 0..tail.len() {
                self.pop_instruction()
            }
        } else {
            // Register the action as having been executed
            self.store_action(act, start);
        }
        result
    }

    pub fn store_action(&mut self, act: ast::Annot<Action>, from: usize) {
        self.run_until = act.range.end;
        self.reset_style();
        self.style_range(0..self.run_until, CodeStyle::Run);
        self.ast.push(vm::Action {
            act: act.value,
            text: act.range,
            asm: from..self.instructions.len(),
        });
    }

    fn clear_interactive(&mut self) {
        if let Some((last_act, act)) = self.current_action.take() {
            act.terminate();
            // Undo partial execution of the action
            while self.instructions.len() > last_act {
                self.pop_instruction();
            }
        }
    }

    // Cancel the current interactive action
    pub fn stop_interactive(&mut self) {
        self.initialize_execution();
        self.clear_interactive();
        self.finalize_execution();
    }

    pub fn run(&mut self, ast: ast::AST) {
        use ExecutionResult::*;
        self.initialize_execution();
        self.clear_interactive();
        for a in ast {
            match self.execute(a) {
                Success => self.end_status = EndStatus::Success,
                Failure => self.end_status = EndStatus::Failure,
                _ => (),
            }
        }
        self.finalize_execution();
        self.prev_code = self.code.clone();
    }

    // On code change, undo all actions that were downstream the edit
    pub fn sync_code(&mut self) {
        // Find first change
        let mut first_change_id: usize = self.code.len().min(self.prev_code.len());
        for i in 0..self.code.len().min(self.prev_code.len()) {
            if self.code.as_bytes()[i] != self.prev_code.as_bytes()[i] {
                first_change_id = i;
                break;
            }
        }

        // If nothing has changed in the part that has been run, there is
        // nothing to do
        if first_change_id >= self.run_until {
            return;
        }

        // Find first modified action
        let first_modified = self
            .ast
            .binary_search_by(|act: &vm::Action| {
                use std::cmp::Ordering::*;
                if first_change_id < act.text.start {
                    Greater
                } else if first_change_id >= act.text.end {
                    Less
                } else {
                    Equal
                }
            })
            .unwrap_or_else(|i| i);
        if first_modified >= self.ast.len() {
            return;
        }

        // Undo all these actions and remove them from the ast
        let tail = self.ast.split_off(first_modified);
        self.initialize_execution();
        self.clear_interactive();
        for act in tail.iter().rev() {
            for _ in act.asm.clone() {
                self.pop_instruction();
            }
        }
        self.finalize_execution();

        // Update run_until
        self.reset_style();
        if let Some(lst) = self.ast.last() {
            self.run_until = lst.text.end;
            self.style_range(0..self.run_until, CodeStyle::Run);
        } else {
            self.run_until = 0;
        }
    }
}
