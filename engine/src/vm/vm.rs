use bevy::ecs::system::Resource;

use crate::vm::ast::AST;
use crate::vm::parser;
use crate::vm::graph::Graph;
use std::collections::HashMap;
use egui::Vec2;

#[derive(Debug,Clone,Copy)]
pub enum GraphId {
    Node(usize),
    Morphism(usize, usize),
    Face(usize),
}

#[derive(Resource)]
pub struct VM {
    pub code: String,
    pub ast: AST,
    pub names: HashMap<String, GraphId>,
    pub ids: HashMap<usize, GraphId>,
    pub error_at: Option<(usize, usize)>,
    pub error_msg: String,
    pub run_until: usize,
    pub graph: Graph,
    pub offset: Vec2,
    pub zoom: f32,
    pub selected_face: Option<usize>,
}

impl VM {
    pub fn new(gd: Graph) -> Self {
        Self {
            code: String::new(),
            ast: Vec::new(),
            names: HashMap::new(),
            ids: HashMap::new(),
            error_at: None,
            error_msg: String::new(),
            run_until: 0,
            graph: gd,
            offset: Vec2::ZERO,
            zoom: 1.0,
            selected_face: None,
        }
    }

    // Compile the code, but do not run it
    pub fn recompile(&mut self) -> bool {
        let r = parser::script(&self.code);
        match r {
            Ok((_, ast)) => {
                self.ast = ast;
                self.error_msg.clear();
                self.error_at = None;
                true
            }
            Err(err) => {
                let err = match err {
                    nom::Err::Incomplete(_) => panic!("Using complete parsers"),
                    nom::Err::Error(err) => err,
                    nom::Err::Failure(err) => err,
                };
                let start = unsafe { err.input.as_ptr().offset_from(self.code.as_ptr()) as usize };
                let end = start + err.input.len();
                self.ast.clear();
                self.error_msg = format!("{}:{}: {}", start, end, err);
                self.error_at = Some((start, end));
                false
            }
        }
    }
}
