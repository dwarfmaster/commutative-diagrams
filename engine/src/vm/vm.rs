use crate::anyterm::AnyTerm;
use crate::data::Context;
use crate::graph::GraphId;
use crate::substitution::{Substitutable, Substitution};
use crate::vm::asm;
use crate::vm::ast;
use crate::vm::graph::Graph;
use crate::vm::interpreter;
use crate::vm::lemmas::Lemma;
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

#[derive(Copy, Hash, Clone, Debug, Eq, PartialEq, Default)]
pub enum CodeStyle {
    Run,
    Error,
    #[default]
    None,
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
    // Means that at offset .0, the text must be styled with style .1, and the
    // offset are kept stored in increasing order. The first offset is always 0.
    pub code_style: Vec<(usize, CodeStyle)>,
    pub error_msg: String,
    pub graph: Graph,
    pub offset: Vec2,
    pub zoom: f32,
    pub selected_face: Option<usize>,
    pub focused_object: Option<GraphId>,
    pub hovered_object: Option<GraphId>,
    pub face_goal_order: Vec<usize>,
    pub face_hyps_order: Vec<usize>,
    pub end_status: EndStatus,
    pub refinements: Vec<(u64, AnyTerm)>,
    pub lemmas: Vec<Lemma>,
}

impl VM {
    pub fn new(
        ctx: Context,
        gd: Graph,
        init_sigma: Substitution,
        lemmas: Vec<(String, Graph)>,
    ) -> Self {
        let lemmas = lemmas
            .into_iter()
            .map(|(name, graph)| crate::lemmas::Lemma::new(name, graph))
            .collect();
        let mut res = Self {
            ctx,
            prev_code: String::new(),
            code: String::new(),
            ast: Vec::new(),
            instructions: Vec::new(),
            eval_status: interpreter::InterpreterStatus::new(),
            names: HashMap::new(),
            code_style: vec![(0, CodeStyle::None)],
            error_msg: String::new(),
            run_until: 0,
            graph: gd,
            offset: Vec2::ZERO,
            zoom: 1.0,
            selected_face: None,
            focused_object: None,
            hovered_object: None,
            face_goal_order: Vec::new(),
            face_hyps_order: Vec::new(),
            end_status: EndStatus::Running,
            // The final substitution, given as a sequential substitution. Before sending
            // it to the proof assistant, all elements must be substituted with the tail
            // of the vector, using finalize_refinements
            refinements: init_sigma,
            lemmas,
        };
        res.renumber_edges();
        res.relabel();
        res.autoname();
        res.recompute_face_statuses();
        res.init_face_order();
        VM::layout(&mut res.graph);
        res.prepare_lemmas();
        res
    }

    fn prepare_lemmas(&mut self) {
        self.lemmas.iter_mut().for_each(|lemma| {
            VM::layout(&mut lemma.pattern);
        });
    }

    pub fn recompile_to(&mut self, to: usize) -> Option<ast::AST> {
        let p = parser::Parser::new(self.run_until, &self.code[self.run_until..to]);
        let r = p.parse();
        match r {
            Ok((_, ast)) => {
                self.error_msg.clear();
                self.reset_style();
                self.style_range(0..self.run_until, CodeStyle::Run);
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
                self.style_range(start..end, CodeStyle::Error);
                None
            }
        }
    }

    // Compile the code, but do not run it
    pub fn recompile(&mut self) -> Option<ast::AST> {
        self.recompile_to(self.code.len())
    }

    // Insert new code after the last executed instruction, parse it and run it
    pub fn insert_and_run(&mut self, code: &str) {
        if self.run_until == 0 {
            self.code.insert_str(self.run_until, code);
        } else {
            self.code.insert_str(self.run_until, &format!("\n{}", code));
        }
        let end = self.run_until + code.len() + (if self.run_until == 0 { 0 } else { 1 });
        if let Some(ast) = self.recompile_to(end) {
            self.run(ast);
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
