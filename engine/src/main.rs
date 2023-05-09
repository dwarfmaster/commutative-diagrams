pub mod anyterm;
pub mod autofill;
pub mod data;
pub mod dsl;
pub mod graph;
pub mod lemmas;
pub mod normalize;
pub mod parser;
pub mod pretty;
pub mod rpc;
pub mod simpleq;
pub mod substitution;
pub mod ui;
pub mod unification;
pub mod vm;

use anyterm::IsTerm;
use dsl::{cat, eq, funct, mph, obj};
use graph::Graph;
use substitution::Substitutable;

use std::fs::File;
use std::io::{Read, Write};
use std::vec::Vec;

use clap::{Parser, Subcommand};

use bevy::app::AppExit;
use bevy::prelude::*;
use bevy_egui::{egui, EguiContext, EguiPlugin};

type SolveGraph = Graph<(), (), ()>;

#[derive(Resource)]
struct AppConfig {
    file: Option<String>,
}

fn test_ui() {
    // Build the graph
    let ctx = data::Context::new();
    let cat = cat!(ctx, (:0));
    let x = obj!(ctx, (:1) in cat);
    let y = obj!(ctx, (:2) in cat);
    let z = obj!(ctx, (:3) in cat);
    let f = mph!(ctx, (:4) : x -> y);
    let g = mph!(ctx, (:5) : x -> y);
    let h = mph!(ctx, (:6) : x -> z);
    let i = mph!(ctx, (:7) : x -> z);
    let mut gr: vm::Graph = Graph {
        nodes: vec![
            (x, vm::NodeLabel::new()),
            (y, vm::NodeLabel::new()),
            (z, vm::NodeLabel::new()),
        ],
        edges: vec![
            vec![
                (1, vm::EdgeLabel::new(), f),
                (1, vm::EdgeLabel::new(), g),
                (2, vm::EdgeLabel::new(), h),
                (2, vm::EdgeLabel::new(), i),
            ],
            vec![],
            vec![],
        ],
        faces: vec![],
    };
    gr.edges[0][1].1.style.left = true;
    gr.edges[0][1].1.style.right = true;
    gr.edges[0][2].1.style.right = true;
    gr.edges[0][2].1.style.highlight = true;
    gr.edges[0][3].1.hidden = true;

    // Run the ui
    let vm = ui::VM::new(ctx, gr, Vec::new(), Vec::new());
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(EguiPlugin)
        .insert_resource(vm)
        .add_system(goal_ui_system)
        .run();
}

fn test_main() {
    let mut ctx = data::Context::new();

    let cat = cat!(ctx, :0);
    let src_cat = cat!(ctx, :1);
    let a = obj!(ctx, (:2) in cat);
    let b = obj!(ctx, (:3) in cat);
    let c = obj!(ctx, (:4) in src_cat);
    let f = funct!(ctx, (:5) : src_cat => cat);
    let c = obj!(ctx, f _0 c);
    let m1 = mph!(ctx, (?0) : a -> b);
    let m2 = mph!(ctx, (?1) : b -> c);
    let gr: graph::Graph<(), (), ()> = graph::Graph {
        nodes: vec![(a, ()), (b, ()), (c.clone(), ())],
        edges: vec![
            vec![(1, (), m1.clone())],
            vec![(2, (), m2.clone())],
            Vec::new(),
        ],
        faces: Vec::new(),
    };

    let cat_unk = cat!(ctx, ?2);
    let x = obj!(ctx, (?3) in cat_unk);
    let y = obj!(ctx, (?4) in cat_unk);
    let f = mph!(ctx, (:6) : x -> y);
    let gr2: graph::Graph<(), (), ()> = graph::Graph {
        nodes: vec![(x, ()), (y, ())],
        edges: vec![vec![(1, (), f)], Vec::new()],
        faces: Vec::new(),
    };

    let mces = graph::mces::MCES::new(&mut ctx, &gr, &gr2);
    for (sol, sigma) in mces {
        let gr_subst = gr.clone().subst(&ctx, &sigma);
        let gr2_subst = gr2.clone().subst(&ctx, &sigma);
        graph::span_viz(
            &mut std::io::stdout(),
            &mut ctx,
            &gr_subst,
            &gr2_subst,
            &sol,
        )
        .unwrap()
    }
}

// Return true if the code has succeeded, ie no ui should be started
fn init_vm_code<In, Out>(client: &mut rpc::Client<In, Out>, vm: &mut ui::VM, path: &str) -> bool
where
    In: std::io::Read,
    Out: std::io::Write,
{
    let file = File::open(path);
    if file.is_err() {
        return false;
    }
    let mut file = file.unwrap();

    if file.read_to_string(&mut vm.code).is_err() {
        return false;
    }
    let ast = vm.recompile();
    if ast.is_none() {
        return false;
    }
    let ast = ast.unwrap();

    vm.run(ast);
    match vm.end_status {
        vm::EndStatus::Success => {
            on_success(client, vm);
            true
        }
        vm::EndStatus::Failure => {
            on_failure(client);
            true
        }
        _ => false,
    }
}

fn goal_graph<In, Out>(
    mut ctx: data::Context,
    mut client: rpc::Client<In, Out>,
    state: Option<String>,
) where
    In: std::io::Read + std::marker::Sync + std::marker::Send + 'static,
    Out: std::io::Write + std::marker::Sync + std::marker::Send + 'static,
{
    // Getting the goal
    log::info!("Asking for graph goal");
    let goal_req = client.send_msg("goal", ()).unwrap();
    let (goal, sigma): (vm::Graph, _) = client
        .receive_msg(
            goal_req,
            parser::Parser::<vm::GraphParsed>::new(ctx.clone()),
        )
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse goal answer: {:#?}", err);
            panic!()
        })
        .prepare(&mut ctx);
    log::info!("Goal received");

    // Getting the lemmas
    log::info!("Asking for lemmas");
    let lemmas_req = client.send_msg("lemmas", ()).unwrap();
    let lemmas: Vec<(String, vm::Graph)> = client
        .receive_msg(
            lemmas_req,
            parser::ParserVec::new(parser::ParserPair::new(
                core::marker::PhantomData::<String>::default(),
                parser::Parser::<vm::Graph>::new(ctx.clone()),
            )),
        )
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse lemmas answer: {:#?}", err);
            panic!()
        });
    lemmas
        .iter()
        .for_each(|(name, _)| log::trace!("Received lemma: {}", name));
    log::info!("{} lemmas received", lemmas.len());

    // Open the vm
    let mut vm = vm::VM::new(ctx, goal, sigma, lemmas);

    // Open the file
    if let Some(path) = &state {
        log::info!("Reading code from file");
        if init_vm_code(&mut client, &mut vm, path) {
            log::info!("Ending without opening the ui");
            return;
        }
    }

    // Run the ui if necessary
    log::info!("Running the ui");
    let mut app = App::new();
    app.add_plugins(DefaultPlugins.build().disable::<bevy::log::LogPlugin>())
        .add_plugin(EguiPlugin)
        .add_state(vm::EndStatus::Running)
        .insert_resource(vm)
        .insert_resource(client)
        .insert_resource(AppConfig { file: state })
        .add_system_set(SystemSet::on_update(vm::EndStatus::Running).with_system(goal_ui_system))
        .add_system_set(
            SystemSet::on_enter(vm::EndStatus::Success).with_system(success_system::<In, Out>),
        )
        .add_system_set(
            SystemSet::on_enter(vm::EndStatus::Failure).with_system(failure_system::<In, Out>),
        );
    app.run();
}

fn goal_ui_system(
    mut egui_context: ResMut<EguiContext>,
    mut vm: ResMut<ui::VM>,
    mut state: ResMut<State<vm::EndStatus>>,
) {
    ui::lemmas_window(egui_context.ctx_mut(), &mut vm.as_mut());
    egui::SidePanel::left("Code").show(egui_context.ctx_mut(), |ui| ui::code(ui, vm.as_mut()));

    egui::CentralPanel::default().show(egui_context.ctx_mut(), |ui| {
        ui::toolbar(ui, &mut vm.as_mut());
        ui.add(ui::graph_vm(&mut vm.as_mut()))
    });

    match vm.end_status {
        vm::EndStatus::Success => state.set(vm::EndStatus::Success).unwrap(),
        vm::EndStatus::Failure => state.set(vm::EndStatus::Failure).unwrap(),
        _ => (),
    }
}

fn save_code_on_exit(path: &str, vm: &ui::VM) {
    let path = std::path::Path::new(path);
    let prefix = path.parent().unwrap();
    std::fs::create_dir_all(prefix).unwrap();
    let mut file = File::create(path).unwrap();
    file.write_all(&vm.code[0..vm.run_until].as_bytes())
        .unwrap();
}

fn on_failure<In, Out>(client: &mut rpc::Client<In, Out>)
where
    In: std::io::Read,
    Out: std::io::Write,
{
    log::info!("Entering failed state");

    // Notify server of failure before exiting
    let solve_req = client.send_msg("failed", ()).unwrap();
    client
        .receive_msg(solve_req, core::marker::PhantomData::<()>::default())
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse failed answer: {:#?}", err);
            panic!()
        });
}

fn failure_system<In, Out>(
    mut exit: EventWriter<AppExit>,
    mut client: ResMut<rpc::Client<In, Out>>,
    vm: Res<ui::VM>,
    cfg: Res<AppConfig>,
) where
    In: std::io::Read + std::marker::Sync + std::marker::Send + 'static,
    Out: std::io::Write + std::marker::Sync + std::marker::Send + 'static,
{
    on_failure(client.as_mut());
    if let Some(path) = &cfg.file {
        save_code_on_exit(path, vm.as_ref());
    }
    exit.send(AppExit)
}

fn on_success<In, Out>(client: &mut rpc::Client<In, Out>, vm: &mut ui::VM)
where
    In: std::io::Read,
    Out: std::io::Write,
{
    log::info!("Entering success state");

    // Result is a vector of existentials and their instantiation
    let result = vm.finalize_refinements();
    log::info!("Sending refinements");
    let refine_req = client.send_msg("refine", result).unwrap();
    client
        .receive_msg(refine_req, core::marker::PhantomData::<()>::default())
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse refine answer: {:#?}", err);
            panic!()
        });
    log::info!("Acknowledgement of refinements received");
}

fn success_system<In, Out>(
    mut exit: EventWriter<AppExit>,
    mut client: ResMut<rpc::Client<In, Out>>,
    mut vm: ResMut<ui::VM>,
    cfg: Res<AppConfig>,
) where
    In: std::io::Read + std::marker::Sync + std::marker::Send + 'static,
    Out: std::io::Write + std::marker::Sync + std::marker::Send + 'static,
{
    on_success(client.as_mut(), vm.as_mut());
    if let Some(path) = &cfg.file {
        save_code_on_exit(path, vm.as_ref());
    }
    exit.send(AppExit)
}

fn goal_print<In, Out>(mut ctx: data::Context, mut client: rpc::Client<In, Out>, path: String)
where
    In: std::io::Read,
    Out: std::io::Write,
{
    log::info!("Asking for graph goal");
    let goal_req = client.send_msg("goal", ()).unwrap();
    let goal: SolveGraph = client
        .receive_msg(goal_req, parser::Parser::<SolveGraph>::new(ctx.clone()))
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse goal answer: {:#?}", err);
            panic!()
        });
    log::info!("Goal received");

    log::info!("Printing");
    let mut file = File::create(path).unwrap();
    graph::viz(&mut file, &goal, &mut ctx).unwrap();

    // Notify server we're finished
    log::info!("Sending printed notification");
    let refine_req = client.send_msg("printed", ()).unwrap();
    client
        .receive_msg(refine_req, core::marker::PhantomData::<()>::default())
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse printed answer: {:#?}", err);
            panic!()
        });
    log::info!("Acknowledgement of printed received");
}

fn goal_norm<In, Out>(mut ctx: data::Context, mut client: rpc::Client<In, Out>)
where
    In: std::io::Read,
    Out: std::io::Write,
{
    log::info!("Asking for morphism to normalize");
    let goal_req = client.send_msg("goal", ()).unwrap();
    let goals: Vec<data::Morphism> = client
        .receive_msg(
            goal_req,
            parser::Parser::<data::Morphism>::new_vec(ctx.clone()),
        )
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse goal answer: {:#?}", err);
            panic!()
        });
    log::info!("{} goals received", goals.len());

    let result: Vec<Vec<anyterm::AnyTerm>> = goals
        .into_iter()
        .map(|goal| normalize::morphism(&mut ctx, goal))
        .map(|(mph, eq)| vec![mph.term(), eq.term()])
        .collect();
    log::info!("Sending normalized morphism");
    let norm_req = client.send_msg("normalized", result).unwrap();
    client
        .receive_msg(norm_req, core::marker::PhantomData::<()>::default())
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse normalized answer: {:#?}", err);
            panic!()
        });
    log::info!("Acknowledgement of normalization received");
}

fn goal_solve<In, Out>(mut ctx: data::Context, mut client: rpc::Client<In, Out>, level: usize)
where
    In: std::io::Read,
    Out: std::io::Write,
{
    log::info!("Asking for goal");
    let goal_req = client.send_msg("goal", ()).unwrap();
    let (goal, sigma): (vm::Graph, _) = client
        .receive_msg(
            goal_req,
            parser::Parser::<vm::GraphParsed>::new(ctx.clone()),
        )
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse goal answer: {:#?}", err);
            panic!()
        })
        .prepare(&mut ctx);
    log::info!("Goal received");

    let mut vm = ui::VM::new(ctx, goal, sigma, Vec::new());

    log::info!("Asking for pair of morphisms to equate");
    let tosolve_req = client.send_msg("tosolve", ()).unwrap();
    let mut tosolve: Vec<data::Morphism> = client
        .receive_msg(
            tosolve_req,
            parser::Parser::<data::Morphism>::new_vec(vm.ctx.clone()),
        )
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse goal answer: {:#?}", err);
            panic!()
        });
    if tosolve.len() != 2 {
        log::error!("Received {} morphisms, expected 2", tosolve.len());
        panic!()
    }

    let right = tosolve.pop().unwrap();
    let left = tosolve.pop().unwrap();
    if right.cat(&vm.ctx) != left.cat(&vm.ctx) {
        log::error!("The two morphism do not have the same category");
        panic!()
    } else if right.src(&vm.ctx) != left.src(&vm.ctx) {
        log::error!("The two morphism do not have the same domain");
        panic!()
    } else if right.dst(&vm.ctx) != left.dst(&vm.ctx) {
        log::error!("The two morphism do not have the same codomain");
        panic!()
    }
    log::info!("Received valid pair of morphisms");

    let (left_src, left_id, left_dst) = vm.insert_mph(left.clone());
    let (right_src, right_id, right_dst) = vm.insert_mph(right.clone());
    assert_eq!(left_src, right_src, "The two goals should be parallel");
    assert_eq!(left_dst, right_dst, "The two goals should be parallel");

    log::info!("Creating goal face");
    let ex = vm.ctx.new_existential();
    let goal_face = graph::Face {
        start: left_src,
        end: left_dst,
        left: vec![left_id],
        right: vec![right_id],
        eq: eq!(vm.ctx, (?ex) : left == right),
        label: Default::default(),
    };
    let goal_id = vm.graph.faces.len();
    vm.graph.faces.push(goal_face);

    log::info!("Normalizing graph");
    let esizes = vm.graph.edges.iter().map(|v| v.len()).collect::<Vec<_>>();
    for node in 0..esizes.len() {
        for edge in 0..esizes[node] {
            vm.split_norm(node, edge);
        }
    }

    log::debug!("Graph to solve: {:#?}", vm.graph);

    log::info!("Trying to solve face {}", goal_id);
    let mask = vm
        .graph
        .faces
        .iter()
        .enumerate()
        .map(|(i, _)| i != goal_id)
        .collect();
    let sol = autofill::solve(&mut vm.ctx, &vm.graph, &mask, goal_id, level);
    match sol {
        None => {
            log::info!("Failed to solve");
            let solve_req = client.send_msg("unsolvable", ()).unwrap();
            client
                .receive_msg(solve_req, core::marker::PhantomData::<()>::default())
                .unwrap_or_else(|err| {
                    log::warn!("Couldn't parse unsolvable answer: {:#?}", err);
                    panic!()
                });
        }
        Some(sigma) => {
            log::info!("Solving succeeded");
            let eq = vm.graph.faces[goal_id].eq.clone().subst(&vm.ctx, &sigma);
            let solve_req = client.send_msg("solved", vec![eq.term()]).unwrap();
            client
                .receive_msg(solve_req, core::marker::PhantomData::<()>::default())
                .unwrap_or_else(|err| {
                    log::warn!("Couldn't parse solved answer: {:#?}", err);
                    panic!()
                })
        }
    }

    log::info!("Solving finished")
}

fn embed(normalize: bool, autosolve: Option<usize>, print: Option<String>, state: Option<String>) {
    simplelog::WriteLogger::init(
        simplelog::LevelFilter::max(),
        simplelog::ConfigBuilder::new()
            .add_filter_ignore("wgpu_hal".to_string())
            .add_filter_ignore("wgpu_core".to_string())
            .add_filter_ignore("objects".to_string())
            .add_filter_ignore("winit".to_string())
            .add_filter_ignore("gilrs".to_string())
            .add_filter_ignore("naga".to_string())
            .add_filter_ignore("mio".to_string())
            .build(),
        File::create("diagrams-engine.log").unwrap(),
    )
    .unwrap();

    log::info!("Creating context");
    let ctx = data::Context::new();
    let mut client = rpc::Client::new(std::io::stdin(), std::io::stdout());
    log::info!("Asking for hypothesises");
    let hyps_req = client.send_msg("hyps", ()).unwrap();
    log::info!("Waiting for hypothesises");
    client
        .receive_msg(
            hyps_req,
            parser::ParserVec::new(core::marker::PhantomData::<(u64, String)>::default()),
        )
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse hyps answer: {:#?}", err);
            panic!()
        })
        .into_iter()
        .for_each(|(id, name)| {
            log::debug!("New hypothesis: {} => \"{}\"", id, name);
            ctx.new_term_mv(id, name)
        });

    if normalize {
        goal_norm(ctx, client)
    } else if autosolve.is_some() {
        goal_solve(ctx, client, autosolve.unwrap())
    } else if print.is_some() {
        goal_print(ctx, client, print.unwrap())
    } else {
        goal_graph(ctx, client, state)
    }
}

#[derive(Parser, Debug)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Test {},
    Ui {},
    Embed {
        #[arg(long)]
        normalize: bool,
        #[arg(long)]
        autosolve: Option<usize>,
        #[arg(long)]
        print: Option<String>,
        #[arg(long)]
        state: Option<String>,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Test {} => test_main(),
        Commands::Ui {} => test_ui(),
        Commands::Embed {
            normalize,
            autosolve,
            print,
            state,
        } => embed(normalize, autosolve, print, state),
    }
}
