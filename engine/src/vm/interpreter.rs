use crate::vm::ast::Action;
use crate::vm::graph::GraphId;
use crate::vm::VM;

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
        match self.ast[act].clone() {
            InsertNode(node) => {
                let node = self.descr_as_object(&node);
                match node {
                    Some((node, _)) => self.insert_node(node),
                    None => {
                        self.error_msg = "Couldn't interpret object description".to_string();
                        return ExecutionError;
                    }
                }
            }
            InsertMorphism(mph) => {
                let mph = self.descr_as_morphism(&mph);
                match mph {
                    Some((mph, _)) => self.insert_mph(mph),
                    None => {
                        self.error_msg = "Couldn't interpret morphism description".to_string();
                        return ExecutionError;
                    }
                }
            }
            InsertMorphismAt(node, mph) => {
                let mph = self.descr_as_morphism(&mph);
                match mph {
                    Some((mph, _)) => self.insert_mph_at(node, mph),
                    None => {
                        self.error_msg = "Couldn't interpret morphism description".to_string();
                        return ExecutionError;
                    }
                }
            }
            Split(mph) => {
                let mph = self.identify_edge(&mph);
                match mph {
                    Some((src, mph)) => self.split(src, mph),
                    None => {
                        self.error_msg = "Couldn't interpret morphism description".to_string();
                        return ExecutionError;
                    }
                }
            }
            HideNode(n) => {
                let n = self.identify_node(&n);
                match n {
                    Some(n) => self.hide(GraphId::Node(n)),
                    None => {
                        self.error_msg = "Couldn't interpret node description".to_string();
                        return ExecutionError;
                    }
                }
            }
            RevealNode(n) => {
                let n = self.identify_node(&n);
                match n {
                    Some(n) => self.reveal(GraphId::Node(n)),
                    None => {
                        self.error_msg = "Couldn't interpret node description".to_string();
                        return ExecutionError;
                    }
                }
            }
            HideMorphism(m) => {
                let m = self.identify_edge(&m);
                match m {
                    Some((s, m)) => self.hide(GraphId::Morphism(s, m)),
                    None => {
                        self.error_msg = "Couldn't interpret edge description".to_string();
                        return ExecutionError;
                    }
                }
            }
            RevealMorphism(m) => {
                let m = self.identify_edge(&m);
                match m {
                    Some((s, m)) => self.reveal(GraphId::Morphism(s, m)),
                    None => {
                        self.error_msg = "Couldn't interpret edge description".to_string();
                        return ExecutionError;
                    }
                }
            }
            HideFace(f) => {
                let f = self.identify_face(&f);
                match f {
                    Some(f) => self.hide(GraphId::Face(f)),
                    None => {
                        self.error_msg = "Couldn't interpret face description".to_string();
                        return ExecutionError;
                    }
                }
            }
            RevealFace(f) => {
                let f = self.identify_face(&f);
                match f {
                    Some(f) => self.reveal(GraphId::Face(f)),
                    None => {
                        self.error_msg = "Couldn't interpret face description".to_string();
                        return ExecutionError;
                    }
                }
            }
            Solve(_) => todo!(),
            Refine(_, _) => todo!(),
            Succeed => return Success,
            Fail => return Failure,
        }
        Unfinished
    }

    pub fn run(&mut self) -> ExecutionResult {
        use ExecutionResult::*;
        for a in 0..self.ast.len() {
            match self.execute(a) {
                Success => return Success,
                Failure => return Failure,
                ExecutionError => return ExecutionError,
                _ => (),
            }
        }
        Unfinished
    }
}
