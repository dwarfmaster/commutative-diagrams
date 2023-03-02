use crate::anyterm::AnyTerm;
use crate::data::Context;
use crate::substitution::Substitutable;
use crate::vm::asm;
use crate::vm::ast;
use crate::vm::graph::{Graph, GraphId};
use crate::vm::interpreter;
use crate::vm::parser;
use bevy::ecs::system::Resource;
use core::ops::Range;
use egui::Vec2;
use std::collections::HashMap;

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq, Default)]
pub enum EndStatus {
    Success,
    Failure,
    #[default]
    Running,
}

pub struct Action {
    pub act: ast::Action,
    pub text: Range<usize>,
    pub asm: Range<usize>,
}

#[derive(Resource)]
pub struct VM {
    pub ctx: Context,
    pub prev_code: String,
    pub code: String,
    pub ast: Vec<Action>,
    pub instructions: Vec<asm::Instruction>,
    pub run_until: usize, // In bytes
    pub eval_status: interpreter::InterpreterStatus,
    pub names: HashMap<String, GraphId>,
    pub error_at: Option<(usize, usize)>,
    pub error_msg: String,
    pub graph: Graph,
    pub offset: Vec2,
    pub zoom: f32,
    pub selected_face: Option<usize>,
    pub end_status: EndStatus,
    pub refinements: Vec<(u64, AnyTerm)>,
}

impl VM {
    pub fn new(ctx: Context, gd: Graph) -> Self {
        let mut res = Self {
            ctx,
            prev_code: String::new(),
            code: String::new(),
            ast: Vec::new(),
            instructions: Vec::new(),
            eval_status: interpreter::InterpreterStatus::new(),
            names: HashMap::new(),
            error_at: None,
            error_msg: String::new(),
            run_until: 0,
            graph: gd,
            offset: Vec2::ZERO,
            zoom: 1.0,
            selected_face: None,
            end_status: EndStatus::Running,
            // The final substitution, given as a sequential substitution. Before sending
            // it to the proof assistant, all elements must be substituted with the tail
            // of the vector, using finalize_refinements
            refinements: Vec::new(),
        };
        res.renumber_edges();
        res.autoname();
        res.hide_identities();
        res.relabel();
        res.layout();
        res
    }

    // Compile the code, but do not run it
    pub fn recompile(&mut self) -> Option<ast::AST> {
        let p = parser::Parser::new(self.run_until, &self.code[self.run_until..]);
        let r = p.parse();
        match r {
            Ok((_, ast)) => {
                self.error_msg.clear();
                self.error_at = None;
                Some(ast)
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
                None
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

    // Build the refinements to send to the proof assistant
    pub fn finalize_refinements(&self) -> Vec<(u64, AnyTerm)> {
        (0..self.refinements.len())
            .into_iter()
            .map(|n| {
                let (id, term) = &self.refinements[n];
                (
                    *id,
                    term.clone()
                        .subst_slice(&self.ctx, &self.refinements[n + 1..]),
                )
            })
            .collect()
    }
}
