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
use async_trait::async_trait;
use core::ops::Range;
use egui::Vec2;
use futures::lock::Mutex;
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

#[async_trait]
pub trait Interactive: Sized {
    async fn compile<R: Remote + Sync + Send>(self, vm: &VM<R, Self>) -> String;
    async fn terminate(self);
}
#[async_trait]
impl Interactive for () {
    async fn compile<R: Remote + Sync + Send>(self, _: &VM<R, ()>) -> String {
        "".to_string()
    }
    async fn terminate(self) {}
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct GraphicalState {
    pub offset: Vec2,
    pub zoom: f32,
    pub focused: Option<GraphId>,
    pub hovered: Option<GraphId>,
    pub dragged: Option<GraphId>,
}

#[derive(Debug, Clone)]
pub struct DisplayState {
    pub init_ppp: Option<f32>,
    pub ppp: Option<f32>,
}

pub struct VM<Rm: Remote, I: Interactive> {
    // State
    pub ctx: Mutex<Context<Rm>>,
    pub config: Mutex<Config>,
    pub graph: Mutex<GraphState>,
    pub lemmas: Mutex<LemmaState>,

    // Execution
    pub ins: Mutex<InstructionsState>,
    pub end_status: Mutex<EndStatus>,

    // Code
    pub code: Mutex<CodeState>,

    // Used to handle partial action execution. Indeed, some actions executions
    // are interactive, and as such can be in a state of being partially
    // executed in the interface. If any other action is run, this one must be
    // rolled back. The interaction may emit instructions. On successfull
    // application, it becomes an action and it is assumed the instructions
    // emitted interactively have the same resulting effect as if it was
    // executed at once.
    pub current_action: Mutex<Option<(usize, I)>>,

    // Graphical status
    pub graphical: GraphicalState,
    pub display: DisplayState,
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
            ctx: Mutex::new(ctx),
            config: Mutex::new(Config::new()),
            end_status: Mutex::new(EndStatus::Running),
            graph: Mutex::new(GraphState {
                graph,
                names: HashMap::new(),
                layout: LayoutEngine::new(),
                face_goal_order: Vec::new(),
                face_hyps_order: Vec::new(),
                selected_face: None,
            }),
            ins: Mutex::new(InstructionsState {
                instructions: Vec::new(),
                eval_status: interpreter::InterpreterStatus::new(),
            }),
            code: Mutex::new(CodeState {
                prev_code: String::new(),
                code: String::new(),
                ast: Vec::new(),
                code_style: vec![(0, CodeStyle::None)],
                error_msg: String::new(),
                run_until: 0,
                states: vec![init_state],
                code_window_open: false,
            }),
            lemmas: Mutex::new(LemmaState {
                lemmas,
                lemma_tree,
                selected_lemma: None,
            }),
            current_action: Mutex::new(None),
            graphical: GraphicalState {
                offset: Vec2::ZERO,
                zoom: 1.0,
                focused: None,
                hovered: None,
                dragged: None,
            },
            display: DisplayState {
                init_ppp: None,
                ppp: None,
            },
        };
        vm.relabel();
        vm.recompute_face_statuses();
        vm.autoname();
        {
            let graph = &mut vm.graph.try_lock().unwrap();
            graph.init_face_order();
            graph.particles_for_graph(&vm.config.try_lock().unwrap());
        }
        vm
    }

    async fn recompile_to(&self, to: usize, one: bool) -> Option<ast::AST> {
        let code = &mut self.code.lock().await;
        let p = parser::Parser::new(code.run_until, &code.code[code.run_until..to]);
        let r = if one { p.parse_one() } else { p.parse() };
        match r {
            Ok((_, ast)) => {
                code.error_msg.clear();
                code.reset_style();
                let run_until = code.run_until;
                code.style_range(0..run_until, CodeStyle::Run);
                Some(ast)
            }
            Err(err) => {
                let err = match err {
                    nom::Err::Incomplete(_) => panic!("Using complete parsers"),
                    nom::Err::Error(err) => err,
                    nom::Err::Failure(err) => err,
                };
                let start = unsafe { err.input.as_ptr().offset_from(code.code.as_ptr()) as usize };
                let end = start + err.input.len();
                code.error_msg = format!("{}:{}: {}", start, end, err);
                code.ast.clear();
                code.style_range(start..end, CodeStyle::Error);
                None
            }
        }
    }

    // Compile the code, but do not run it
    pub async fn recompile(&self) -> Option<ast::AST> {
        let len = self.code.lock().await.code.len();
        self.recompile_to(len, false).await
    }
    pub async fn recompile_one(&self) -> Option<ast::AST> {
        let len = self.code.lock().await.code.len();
        self.recompile_to(len, true).await
    }

    // Insert new code the last executed instruction and parse it
    async fn insert_and_parse(&self, to_insert: &str) -> Option<ast::AST> {
        let end = {
            let code = &mut self.code.lock().await;
            let start = code.run_until + (if code.run_until == 0 { 0 } else { 1 });
            let end = code.run_until + to_insert.len() + (if code.run_until == 0 { 0 } else { 1 });
            if !(code.code.len() > end
                && &code.code[start..end] == to_insert
                && (code.code.len() == end || code.code.chars().nth(end) == Some('\n')))
            {
                let run_until = code.run_until;
                if run_until == 0 {
                    code.code.insert_str(run_until, &format!("{}\n", to_insert));
                } else {
                    code.code.insert_str(run_until, &format!("\n{}", to_insert));
                }
            }
            end
        };
        self.recompile_to(end, false).await
    }

    // Insert new code after the last executed instruction, parse it and run it
    pub async fn insert_and_run(&self, code: &str) {
        if let Some(ast) = self.insert_and_parse(code).await {
            self.run(ast);
        }
    }

    // Start a new interactive action
    pub async fn start_interactive(&self, int: I) {
        if self.current_action.lock().await.is_some() {
            self.stop_interactive();
        }
        *self.current_action.lock().await = Some((self.ins.lock().await.instructions.len(), int));
    }
}

impl <Rm: Remote + Sync + Send, I: Interactive> VM<Rm,I> {
    // Commit the current interactive action
    pub async fn commit_interactive(&self) {
        let current = self.current_action.lock().await.take();
        if let Some((last, interactive)) = current {
            let code = interactive.compile(&self).await;
            let ast = self.insert_and_parse(&code).await.unwrap();
            assert_eq!(ast.len(), 1);
            let act = ast.into_iter().next().unwrap();
            self.store_action(act, last);
            self.finalize_execution();
        }
    }
}
