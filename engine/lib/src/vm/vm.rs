use super::config::Config;
use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::ast;
use crate::vm::graph::Graph;
use crate::vm::interpreter;
use crate::vm::layout::LayoutEngine;
use crate::vm::lemmas::{Lemma, LemmaTree};
use crate::vm::parser;
use crate::vm::store::Context;
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

pub trait Interactive: Sized {
    fn compile<R: Remote>(self, vm: &VM<R, Self>) -> String;
    fn terminate(self);
}
impl Interactive for () {
    fn compile<R: Remote>(self, _: &VM<R, ()>) -> String {
        "".to_string()
    }
    fn terminate(self) {}
}

pub struct GraphState {
    pub graph: Graph,
    pub names: HashMap<String, GraphId>,
    pub layout: LayoutEngine,
    pub face_goal_order: Vec<usize>,
    pub face_hyps_order: Vec<usize>,
    pub selected_face: Option<usize>,
}

pub struct LemmaState {
    pub lemmas: Vec<Lemma>,
    pub lemma_tree: Vec<Box<LemmaTree>>,
    pub selected_lemma: Option<usize>,
}

pub struct InstructionsState {
    pub instructions: Vec<asm::Instruction>,
    pub eval_status: interpreter::InterpreterStatus,
}

pub struct CodeState {
    pub prev_code: String,
    pub code: String,
    pub ast: Vec<Action>,
    pub run_until: usize, // In bytes
    // Means that at offset .0, the text must be styled with style .1, and the
    // offset are kept stored in increasing order. The first offset is always 0.
    pub code_style: Vec<(usize, CodeStyle)>,
    pub error_msg: String,
    // The proof-assistant states corresponding to the execution of actions.
    // The nth actions has initial state states[n] and final state states[n+1]
    // (which may be the same). As such states should never be empty, and
    // always be of length exactly one more than the length of ast.
    pub states: Vec<u64>,
    pub code_window_open: bool,
}

pub struct GraphicalState {
    pub offset: Vec2,
    pub zoom: f32,
    pub focused_object: Option<GraphId>,
    pub hovered_object: Option<GraphId>,
    pub dragged_object: Option<GraphId>,
    pub init_ppp: Option<f32>,
    pub ppp: Option<f32>,
}

pub struct VM<Rm: Remote, I: Interactive> {
    // State
    pub ctx: Context<Rm>,
    pub config: Config,
    pub graph: GraphState,
    pub lemmas: LemmaState,

    // Execution
    pub ins: InstructionsState,
    pub end_status: EndStatus,

    // Code
    pub code: CodeState,

    // Used to handle partial action execution. Indeed, some actions executions
    // are interactive, and as such can be in a state of being partially
    // executed in the interface. If any other action is run, this one must be
    // rolled back. The interaction may emit instructions. On successfull
    // application, it becomes an action and it is assumed the instructions
    // emitted interactively have the same resulting effect as if it was
    // executed at once.
    pub current_action: Option<(usize, I)>,

    // Graphical status
    pub graphical: GraphicalState,
}

impl<R: Remote, I: Interactive> VM<R, I> {
    pub fn start(remote: R) -> Self {
        log::info!("Starting VM");
        let mut ctx = Context::new(remote);
        let graph_parsed = ctx.remote.goal().unwrap_or_else(|err| {
            log::warn!("Couldn't parse goal answer: {:#?}", err);
            panic!()
        });
        let graph = graph_parsed.prepare(&mut ctx);
        let lemmas: Vec<Lemma> = ctx
            .remote
            .lemmas()
            .unwrap_or_else(|err| {
                log::warn!("Couldn't parse lemma list: {:#?}", err);
                panic!()
            })
            .into_iter()
            .map(|(id, name, namespace)| Lemma::new(id, name, namespace))
            .collect();
        let lemma_tree = LemmaTree::new(&lemmas[..]);
        let init_state = ctx.save_state();
        let mut vm = Self {
            ctx,
            config: Config::new(),
            end_status: EndStatus::Running,
            graph: GraphState {
                graph,
                names: HashMap::new(),
                layout: LayoutEngine::new(),
                face_goal_order: Vec::new(),
                face_hyps_order: Vec::new(),
                selected_face: None,
            },
            ins: InstructionsState {
                instructions: Vec::new(),
                eval_status: interpreter::InterpreterStatus::new(),
            },
            code: CodeState {
                prev_code: String::new(),
                code: String::new(),
                ast: Vec::new(),
                code_style: vec![(0, CodeStyle::None)],
                error_msg: String::new(),
                run_until: 0,
                states: vec![init_state],
                code_window_open: false,
            },
            lemmas: LemmaState {
                lemmas,
                lemma_tree,
                selected_lemma: None,
            },
            current_action: None,
            graphical: GraphicalState {
                offset: Vec2::ZERO,
                zoom: 1.0,
                focused_object: None,
                hovered_object: None,
                dragged_object: None,
                init_ppp: None,
                ppp: None,
            },
        };
        vm.relabel();
        vm.recompute_face_statuses();
        vm.autoname();
        vm.init_face_order();
        vm.graph
            .layout
            .particles_for_graph(&vm.config, &mut vm.graph.graph);
        vm
    }

    fn recompile_to(&mut self, to: usize, one: bool) -> Option<ast::AST> {
        let p = parser::Parser::new(
            self.code.run_until,
            &self.code.code[self.code.run_until..to],
        );
        let r = if one { p.parse_one() } else { p.parse() };
        match r {
            Ok((_, ast)) => {
                self.code.error_msg.clear();
                self.reset_style();
                self.style_range(0..self.code.run_until, CodeStyle::Run);
                Some(ast)
            }
            Err(err) => {
                let err = match err {
                    nom::Err::Incomplete(_) => panic!("Using complete parsers"),
                    nom::Err::Error(err) => err,
                    nom::Err::Failure(err) => err,
                };
                let start =
                    unsafe { err.input.as_ptr().offset_from(self.code.code.as_ptr()) as usize };
                let end = start + err.input.len();
                self.code.ast.clear();
                self.code.error_msg = format!("{}:{}: {}", start, end, err);
                self.style_range(start..end, CodeStyle::Error);
                None
            }
        }
    }

    // Compile the code, but do not run it
    pub fn recompile(&mut self) -> Option<ast::AST> {
        self.recompile_to(self.code.code.len(), false)
    }
    pub fn recompile_one(&mut self) -> Option<ast::AST> {
        self.recompile_to(self.code.code.len(), true)
    }

    // Insert new code the last executed instruction and parse it
    fn insert_and_parse(&mut self, code: &str) -> Option<ast::AST> {
        let start = self.code.run_until + (if self.code.run_until == 0 { 0 } else { 1 });
        let end = self.code.run_until + code.len() + (if self.code.run_until == 0 { 0 } else { 1 });
        if !(self.code.code.len() > end
            && &self.code.code[start..end] == code
            && (self.code.code.len() == end || self.code.code.chars().nth(end) == Some('\n')))
        {
            if self.code.run_until == 0 {
                self.code
                    .code
                    .insert_str(self.code.run_until, &format!("{}\n", code));
            } else {
                self.code
                    .code
                    .insert_str(self.code.run_until, &format!("\n{}", code));
            }
        }
        self.recompile_to(end, false)
    }

    // Insert new code after the last executed instruction, parse it and run it
    pub fn insert_and_run(&mut self, code: &str) {
        if let Some(ast) = self.insert_and_parse(code) {
            self.run(ast);
        }
    }

    // Start a new interactive action
    pub fn start_interactive(&mut self, int: I) {
        if self.current_action.is_some() {
            self.stop_interactive();
        }
        self.current_action = Some((self.ins.instructions.len(), int));
    }

    // Commit the current interactive action
    pub fn commit_interactive(&mut self) {
        if let Some((last, interactive)) = self.current_action.take() {
            let code = interactive.compile(&self);
            let ast = self.insert_and_parse(&code).unwrap();
            assert_eq!(ast.len(), 1);
            let act = ast.into_iter().next().unwrap();
            self.store_action(act, last);
            self.finalize_execution();
        }
    }
}
