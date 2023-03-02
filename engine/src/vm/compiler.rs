use crate::vm::ast;
use crate::vm::ast::Action;
use crate::vm::graph::GraphId;
use crate::vm::vm;
use crate::vm::{EndStatus, VM};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ExecutionResult {
    Success,
    Failure,
    ExecutionError,
    Unfinished,
}

impl VM {
    fn execute(&mut self, act: ast::Annot<ast::Action>) -> ExecutionResult {
        use Action::*;
        use ExecutionResult::*;
        let mut result = Unfinished;
        let start = self.instructions.len();
        match act.value.clone() {
            InsertNode(node) => {
                let node = self.descr_as_object(&node.value);
                match node {
                    Some((node, _)) => {
                        self.insert_node(node);
                    }
                    None => {
                        self.error_msg = "Couldn't interpret object description".to_string();
                        result = ExecutionError;
                    }
                }
            }
            InsertMorphism(mph) => {
                let mph = self.descr_as_morphism(&mph.value);
                match mph {
                    Some((mph, _)) => {
                        self.insert_mph(mph);
                    }
                    None => {
                        self.error_msg = "Couldn't interpret morphism description".to_string();
                        result = ExecutionError;
                    }
                }
            }
            InsertMorphismAt(node, mph) => {
                let mph = self.descr_as_morphism(&mph.value);
                match mph {
                    Some((mph, _)) => {
                        self.insert_mph_at(node.value, mph);
                    }
                    None => {
                        self.error_msg = "Couldn't interpret morphism description".to_string();
                        result = ExecutionError;
                    }
                }
            }
            Split(mph) => {
                let mph = self.identify_edge(&mph.value);
                match mph {
                    Some((src, mph)) => self.split(src, mph),
                    None => {
                        self.error_msg = "Couldn't interpret morphism description".to_string();
                        result = ExecutionError;
                    }
                }
            }
            HideNode(n) => {
                let n = self.identify_node(&n.value);
                match n {
                    Some(n) => self.hide(GraphId::Node(n)),
                    None => {
                        self.error_msg = "Couldn't interpret node description".to_string();
                        result = ExecutionError;
                    }
                }
            }
            RevealNode(n) => {
                let n = self.identify_node(&n.value);
                match n {
                    Some(n) => self.reveal(GraphId::Node(n)),
                    None => {
                        self.error_msg = "Couldn't interpret node description".to_string();
                        result = ExecutionError;
                    }
                }
            }
            HideMorphism(m) => {
                let m = self.identify_edge(&m.value);
                match m {
                    Some((s, m)) => self.hide(GraphId::Morphism(s, m)),
                    None => {
                        self.error_msg = "Couldn't interpret edge description".to_string();
                        result = ExecutionError;
                    }
                }
            }
            RevealMorphism(m) => {
                let m = self.identify_edge(&m.value);
                match m {
                    Some((s, m)) => self.reveal(GraphId::Morphism(s, m)),
                    None => {
                        self.error_msg = "Couldn't interpret edge description".to_string();
                        result = ExecutionError;
                    }
                }
            }
            HideFace(f) => {
                let f = self.identify_face(&f.value);
                match f {
                    Some(f) => self.hide(GraphId::Face(f)),
                    None => {
                        self.error_msg = "Couldn't interpret face description".to_string();
                        result = ExecutionError;
                    }
                }
            }
            RevealFace(f) => {
                let f = self.identify_face(&f.value);
                match f {
                    Some(f) => self.reveal(GraphId::Face(f)),
                    None => {
                        self.error_msg = "Couldn't interpret face description".to_string();
                        result = ExecutionError;
                    }
                }
            }
            Solve(size, f) => {
                let f = self.identify_face(&f.value);
                match f {
                    Some(f) => {
                        if !self.solve_face(f, size.map(|a| a.value).unwrap_or(5)) {
                            self.error_msg = format!("Couldn't solve face {}", f);
                            result = ExecutionError;
                        }
                    }
                    None => {
                        self.error_msg = "Couldn't interpret face description".to_string();
                        result = ExecutionError;
                    }
                }
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
            self.run_until = act.range.end;
            self.ast.push(vm::Action {
                act: act.value,
                text: act.range,
                asm: start..self.instructions.len(),
            });
        }
        result
    }

    pub fn run(&mut self, ast: ast::AST) {
        use ExecutionResult::*;
        self.initialize_execution();
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
        for act in tail.iter().rev() {
            for _ in act.asm.clone() {
                self.pop_instruction();
            }
        }
        self.finalize_execution();

        // Update run_until
        if let Some(lst) = self.ast.last() {
            self.run_until = lst.text.end;
        } else {
            self.run_until = 0;
        }
    }
}
