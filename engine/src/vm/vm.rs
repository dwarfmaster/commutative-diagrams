use bevy::ecs::system::Resource;

use crate::data::Context;
use crate::vm::ast::AST;
use crate::vm::graph::{Graph, GraphId};
use crate::vm::parser;
use egui::Vec2;
use std::collections::HashMap;

#[derive(Resource)]
pub struct VM {
    pub ctx: Context,
    pub code: String,
    pub ast: AST,
    pub names: HashMap<String, GraphId>,
    pub error_at: Option<(usize, usize)>,
    pub error_msg: String,
    pub run_until: usize,
    pub graph: Graph,
    pub offset: Vec2,
    pub zoom: f32,
    pub selected_face: Option<usize>,
}

impl VM {
    pub fn new(ctx: Context, gd: Graph) -> Self {
        let mut res = Self {
            ctx,
            code: String::new(),
            ast: Vec::new(),
            names: HashMap::new(),
            error_at: None,
            error_msg: String::new(),
            run_until: 0,
            graph: gd,
            offset: Vec2::ZERO,
            zoom: 1.0,
            selected_face: None,
        };
        res.layout();
        res.renumber_edges();
        res.autoname();
        res
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

    fn renumber_edges(&mut self) {
        let mut count = 0;
        for src in 0..self.graph.edges.len() {
            for mph in 0..self.graph.edges[src].len() {
                self.graph.edges[src][mph].1.id = count;
                count += 1;
            }
        }
    }
}
