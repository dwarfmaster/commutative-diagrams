pub mod anyterm;
pub mod autofill;
pub mod data;
pub mod dsl;
pub mod graph;
pub mod normalize;
pub mod parser;
pub mod pretty;
pub mod rpc;
pub mod substitution;
pub mod ui;
pub mod unification;
pub mod vm;

use anyterm::IsTerm;
use data::ProofObject;
use data::{ActualCategory, ActualEquality, ActualFunctor, ActualMorphism, ActualObject};
use data::{CategoryData, EqualityData, FunctorData, MorphismData, ObjectData};
use dsl::{cat, eq, funct, mph, obj};
use graph::Graph;
use substitution::Substitutable;

use std::fs::File;
use std::vec::Vec;

use clap::{Parser, Subcommand};
use rmp_serde::encode;

use bevy::app::AppExit;
use bevy::prelude::*;
use bevy_egui::{egui, EguiContext, EguiPlugin};

type SolveGraph = Graph<(), (), ()>;

fn messagepack_to_file<NL, EL, FL>(path: &str, gr: &Graph<NL, EL, FL>) {
    let mut file = File::create(path).unwrap();
    encode::write(&mut file, gr).unwrap();
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
    let vm = vm::VM::new(ctx, gr);
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(EguiPlugin)
        .insert_resource(vm)
        .add_system(goal_ui_system)
        .run();
}

fn test_main(packfile: &str) {
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

    messagepack_to_file(packfile, &gr);
}

fn goal_graph<In, Out>(ctx: data::Context, mut client: rpc::Client<In, Out>)
where
    In: std::io::Read + std::marker::Sync + std::marker::Send + 'static,
    Out: std::io::Write + std::marker::Sync + std::marker::Send + 'static,
{
    log::info!("Asking for graph goal");
    let goal_req = client.send_msg("goal", ()).unwrap();
    let goal: vm::Graph = client
        .receive_msg(goal_req, parser::Parser::<vm::Graph>::new(ctx.clone()))
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse goal answer: {:#?}", err);
            panic!()
        });
    log::info!("Goal received");

    // Run the ui
    log::info!("Running the ui");
    let vm = vm::VM::new(ctx, goal);
    App::new()
        .add_plugins(DefaultPlugins.build().disable::<bevy::log::LogPlugin>())
        .add_plugin(EguiPlugin)
        .add_state(vm::EndStatus::Running)
        .insert_resource(vm)
        .insert_resource(client)
        .add_system_set(SystemSet::on_update(vm::EndStatus::Running).with_system(goal_ui_system))
        .add_system_set(
            SystemSet::on_enter(vm::EndStatus::Success).with_system(success_system::<In, Out>),
        )
        .add_system_set(
            SystemSet::on_enter(vm::EndStatus::Failure).with_system(failure_system::<In, Out>),
        )
        .run();
}

fn goal_ui_system(
    mut egui_context: ResMut<EguiContext>,
    mut vm: ResMut<vm::VM>,
    mut state: ResMut<State<vm::EndStatus>>,
) {
    egui::SidePanel::left("Code").show(egui_context.ctx_mut(), |ui| ui::code(ui, vm.as_mut()));
    egui::SidePanel::right("Faces")
        .show(egui_context.ctx_mut(), |ui| ui::faces(ui, &mut vm.as_mut()));

    egui::CentralPanel::default().show(egui_context.ctx_mut(), |ui| {
        ui.add(ui::graph(&mut vm.as_mut()))
    });

    match vm.end_status {
        vm::EndStatus::Success => state.set(vm::EndStatus::Success).unwrap(),
        vm::EndStatus::Failure => state.set(vm::EndStatus::Failure).unwrap(),
        _ => (),
    }
}

fn failure_system<In, Out>(mut exit: EventWriter<AppExit>, mut client: ResMut<rpc::Client<In, Out>>)
where
    In: std::io::Read + std::marker::Sync + std::marker::Send + 'static,
    Out: std::io::Write + std::marker::Sync + std::marker::Send + 'static,
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

    // Exit the app
    exit.send(AppExit)
}

fn success_system<In, Out>(
    mut exit: EventWriter<AppExit>,
    mut client: ResMut<rpc::Client<In, Out>>,
    vm: ResMut<vm::VM>,
) where
    In: std::io::Read + std::marker::Sync + std::marker::Send + 'static,
    Out: std::io::Write + std::marker::Sync + std::marker::Send + 'static,
{
    log::info!("Entering success state");
    let client = client.as_mut();

    // Result is a vector of existentials and their instantiation
    let result = &vm.refinements;
    log::info!("Sending refinements");
    let refine_req = client.send_msg("refine", result).unwrap();
    client
        .receive_msg(refine_req, core::marker::PhantomData::<()>::default())
        .unwrap_or_else(|err| {
            log::warn!("Couldn't parse refine answer: {:#?}", err);
            panic!()
        });
    log::info!("Acknowledgement of refinements received");

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
    // TODO once the refactor of the vm to use instruction is finished, makes goal_solve use
    // the VM instead of calling the underlying functions directly
    todo!()

    // log::info!("Asking for goal");
    // let goal_req = client.send_msg("goal", ()).unwrap();
    // let mut goal: SolveGraph = client
    //     .receive_msg(goal_req, parser::Parser::<SolveGraph>::new(ctx.clone()))
    //     .unwrap_or_else(|err| {
    //         log::warn!("Couldn't parse goal answer: {:#?}", err);
    //         panic!()
    //     });
    // log::info!("Goal received");
    //
    // log::info!("Asking for pair of morphisms to equate");
    // let tosolve_req = client.send_msg("tosolve", ()).unwrap();
    // let mut tosolve: Vec<data::Morphism> = client
    //     .receive_msg(
    //         tosolve_req,
    //         parser::Parser::<data::Morphism>::new_vec(ctx.clone()),
    //     )
    //     .unwrap_or_else(|err| {
    //         log::warn!("Couldn't parse goal answer: {:#?}", err);
    //         panic!()
    //     });
    // if tosolve.len() != 2 {
    //     log::error!("Received {} morphisms, expected 2", tosolve.len());
    //     panic!()
    // }
    //
    // let right = tosolve.pop().unwrap();
    // let left = tosolve.pop().unwrap();
    // if right.cat(&ctx) != left.cat(&ctx) {
    //     log::error!("The two morphism do not have the same category");
    //     panic!()
    // } else if right.src(&ctx) != left.src(&ctx) {
    //     log::error!("The two morphism do not have the same domain");
    //     panic!()
    // } else if right.dst(&ctx) != left.dst(&ctx) {
    //     log::error!("The two morphism do not have the same codomain");
    //     panic!()
    // }
    // log::info!("Received valid pair of morphisms");
    //
    // let (left_src, left_id, left_dst) = tactics::insert_mph(&mut ctx, &mut goal, left.clone());
    // let (right_src, right_id, right_dst) = tactics::insert_mph(&mut ctx, &mut goal, right.clone());
    // assert_eq!(left_src, right_src, "The two goals should be parallel");
    // assert_eq!(left_dst, right_dst, "The two goals should be parallel");
    //
    // log::info!("Creating goal face");
    // let ex = ctx.new_existential();
    // let goal_face = graph::Face {
    //     start: left_src,
    //     end: left_dst,
    //     left: vec![left_id],
    //     right: vec![right_id],
    //     eq: eq!(ctx, (?ex) : left == right),
    //     label: Default::default(),
    // };
    // let goal_id = goal.faces.len();
    // goal.faces.push(goal_face);
    //
    // log::info!("Normalizing graph");
    // let esizes = goal.edges.iter().map(|v| v.len()).collect::<Vec<_>>();
    // for node in 0..esizes.len() {
    //     for edge in 0..esizes[node] {
    //         tactics::split_norm(&mut ctx, &mut goal, node, edge);
    //     }
    // }
    //
    // log::debug!("Graph to solve: {:#?}", goal);
    //
    // log::info!("Trying to solve face {}", goal_id);
    // let sol = autofill::solve(&mut ctx, &goal, goal_id, level);
    // match sol {
    //     None => {
    //         log::info!("Failed to solve");
    //         let solve_req = client.send_msg("unsolvable", ()).unwrap();
    //         client
    //             .receive_msg(solve_req, core::marker::PhantomData::<()>::default())
    //             .unwrap_or_else(|err| {
    //                 log::warn!("Couldn't parse unsolvable answer: {:#?}", err);
    //                 panic!()
    //             });
    //     }
    //     Some(sigma) => {
    //         log::info!("Solving succeeded");
    //         let eq = goal.faces[goal_id].eq.clone().subst(&ctx, &sigma);
    //         let solve_req = client.send_msg("solved", vec![eq.term()]).unwrap();
    //         client
    //             .receive_msg(solve_req, core::marker::PhantomData::<()>::default())
    //             .unwrap_or_else(|err| {
    //                 log::warn!("Couldn't parse solved answer: {:#?}", err);
    //                 panic!()
    //             })
    //     }
    // }
    //
    // log::info!("Solving finished")
}

fn embed(normalize: bool, autosolve: Option<usize>, print: Option<String>) {
    simplelog::WriteLogger::init(
        simplelog::LevelFilter::Debug,
        simplelog::ConfigBuilder::new()
            .set_max_level(simplelog::LevelFilter::max())
            .add_filter_ignore("wgpu_hal".to_string())
            .add_filter_ignore("wgpu_core".to_string())
            .add_filter_ignore("objects".to_string())
            .add_filter_ignore("winit".to_string())
            .add_filter_ignore("gilrs".to_string())
            .add_filter_ignore("naga".to_string())
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
        goal_graph(ctx, client)
    }
}

#[derive(Parser, Debug)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Test {
        #[arg(short, long)]
        packfile: Option<String>,
    },
    Ui {},
    Embed {
        #[arg(long)]
        normalize: bool,
        #[arg(long)]
        autosolve: Option<usize>,
        #[arg(long)]
        print: Option<String>,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Test { packfile } => {
            let packfile = packfile.unwrap_or("gr.mp".to_string());
            test_main(&packfile)
        }
        Commands::Ui {} => test_ui(),
        Commands::Embed {
            normalize,
            autosolve,
            print,
        } => embed(normalize, autosolve, print),
    }
}
