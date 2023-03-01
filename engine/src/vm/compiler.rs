use crate::vm::ast::Action;
use crate::vm::graph::GraphId;
use crate::vm::{EndStatus, VM};

pub enum ExecutionResult {
    Success,
    Failure,
    ExecutionError,
    Unfinished,
}

impl VM {
    fn execute(&mut self, act: usize) -> ExecutionResult {
        use Action::*;
        use ExecutionResult::*;
        match self.ast[act].value.clone() {
            InsertNode(node) => {
                let node = self.descr_as_object(&node.value);
                match node {
                    Some((node, _)) => {
                        self.insert_node(node);
                    }
                    None => {
                        self.error_msg = "Couldn't interpret object description".to_string();
                        return ExecutionError;
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
                        return ExecutionError;
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
                        return ExecutionError;
                    }
                }
            }
            Split(mph) => {
                let mph = self.identify_edge(&mph.value);
                match mph {
                    Some((src, mph)) => self.split(src, mph),
                    None => {
                        self.error_msg = "Couldn't interpret morphism description".to_string();
                        return ExecutionError;
                    }
                }
            }
            HideNode(n) => {
                let n = self.identify_node(&n.value);
                match n {
                    Some(n) => self.hide(GraphId::Node(n)),
                    None => {
                        self.error_msg = "Couldn't interpret node description".to_string();
                        return ExecutionError;
                    }
                }
            }
            RevealNode(n) => {
                let n = self.identify_node(&n.value);
                match n {
                    Some(n) => self.reveal(GraphId::Node(n)),
                    None => {
                        self.error_msg = "Couldn't interpret node description".to_string();
                        return ExecutionError;
                    }
                }
            }
            HideMorphism(m) => {
                let m = self.identify_edge(&m.value);
                match m {
                    Some((s, m)) => self.hide(GraphId::Morphism(s, m)),
                    None => {
                        self.error_msg = "Couldn't interpret edge description".to_string();
                        return ExecutionError;
                    }
                }
            }
            RevealMorphism(m) => {
                let m = self.identify_edge(&m.value);
                match m {
                    Some((s, m)) => self.reveal(GraphId::Morphism(s, m)),
                    None => {
                        self.error_msg = "Couldn't interpret edge description".to_string();
                        return ExecutionError;
                    }
                }
            }
            HideFace(f) => {
                let f = self.identify_face(&f.value);
                match f {
                    Some(f) => self.hide(GraphId::Face(f)),
                    None => {
                        self.error_msg = "Couldn't interpret face description".to_string();
                        return ExecutionError;
                    }
                }
            }
            RevealFace(f) => {
                let f = self.identify_face(&f.value);
                match f {
                    Some(f) => self.reveal(GraphId::Face(f)),
                    None => {
                        self.error_msg = "Couldn't interpret face description".to_string();
                        return ExecutionError;
                    }
                }
            }
            Solve(size, f) => {
                let f = self.identify_face(&f.value);
                match f {
                    Some(f) => {
                        if !self.solve_face(f, size.map(|a| a.value).unwrap_or(5)) {
                            self.error_msg = format!("Couldn't solve face {}", f);
                            return ExecutionError;
                        }
                    }
                    None => {
                        self.error_msg = "Couldn't interpret face description".to_string();
                        return ExecutionError;
                    }
                }
            }
            Refine(_, _) => todo!(),
            Succeed => return Success,
            Fail => return Failure,
        }
        Unfinished
    }

    pub fn run(&mut self) {
        use ExecutionResult::*;
        self.initialize_execution();
        for a in 0..self.ast.len() {
            match self.execute(a) {
                Success => self.end_status = EndStatus::Success,
                Failure => self.end_status = EndStatus::Failure,
                _ => (),
            }
        }
        self.finalize_execution();
    }
}
