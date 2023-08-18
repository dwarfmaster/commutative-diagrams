pub mod autofill;
pub mod data;
pub mod graph;
pub mod lemmas;
pub mod normalizer;
pub mod realizer;
pub mod remote;
pub mod ui;
pub mod vm;

use remote::Remote;
use ui::ActionResult;

use std::fs::File;
use std::io::{Read, Write};

use clap::{Parser, Subcommand};

use bevy::app::AppExit;
use bevy::prelude::*;
use bevy_egui::{egui, EguiContexts, EguiPlugin, EguiSettings};

type RPC = remote::RPC<std::io::Stdin, std::io::Stdout>;
type VM = ui::VM<RPC>;

#[derive(Resource)]
struct AppConfig {
    file: Option<String>,
}

// Return true if the code has succeeded, ie no ui should be started
fn init_vm_code(vm: &mut VM, path: &str) -> bool {
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
            on_success(&mut vm.ctx.remote);
            true
        }
        vm::EndStatus::Failure => {
            on_failure(&mut vm.ctx.remote);
            true
        }
        _ => false,
    }
}

fn goal_graph(client: RPC, state: Option<String>) {
    // Open the vm
    let mut vm = vm::VM::start(client);

    // Open the file
    if let Some(path) = &state {
        log::info!("Reading code from file");
        if init_vm_code(&mut vm, path) {
            log::info!("Ending without opening the ui");
            return;
        }
    }

    // Run the ui if necessary
    log::info!("Running the ui");
    let mut app = App::new();
    app.add_plugins(DefaultPlugins.build().disable::<bevy::log::LogPlugin>())
        .add_plugins(EguiPlugin)
        .add_state::<vm::EndStatus>()
        .insert_resource(vm)
        .insert_resource(AppConfig { file: state })
        .add_systems(Update, goal_ui_system)
        .add_systems(OnEnter(vm::EndStatus::Success), success_system)
        .add_systems(OnEnter(vm::EndStatus::Failure), failure_system);
    app.run();
}

fn goal_ui_system(
    mut egui_context: EguiContexts,
    mut settings: ResMut<EguiSettings>,
    mut vm: ResMut<VM>,
    mut state: ResMut<NextState<vm::EndStatus>>,
) {
    // Do one layout step
    {
        let vm = vm.as_mut();
        let fixed = |id| vm.dragged_object == Some(id);
        vm.layout.apply_forces(&vm.config, &vm.graph, &fixed);
        vm.layout.update(&vm.config);

        if let Some(lem) = vm.selected_lemma {
            if vm.lemmas[lem].pattern.is_some() {
                let dragged = vm.lemmas[lem].graphical_state.dragged;
                let fixed = |id| dragged == Some(id);
                let lem = &mut vm.lemmas[lem];
                lem.graphical_state.layout.apply_forces(
                    &vm.config,
                    lem.pattern.as_ref().unwrap(),
                    &fixed,
                );
                lem.graphical_state.layout.update(&vm.config);
            }
        }
        {
            use crate::ui::InteractiveAction::*;
            match &vm.current_action {
                Some((_, LemmaApplication(state))) => {
                    if vm.selected_lemma != Some(state.lemma) {
                        let fixed = |id| state.dragged == Some(id);
                        vm.lemmas[state.lemma].graphical_state.layout.apply_forces(
                            &vm.config,
                            &state.graph,
                            &fixed,
                        );
                        vm.lemmas[state.lemma]
                            .graphical_state
                            .layout
                            .update(&vm.config);
                    }
                }
                Some((_, Merge(..))) => (),
                Some((_, Insert(..))) => (),
                None => (),
            }
        }
    }

    ui::lemmas_window(egui_context.ctx_mut(), &mut vm.as_mut());
    ui::code(egui_context.ctx_mut(), &mut vm.as_mut());
    if let Some((last, mut interactive)) = vm.current_action.take() {
        let r = interactive.display(&mut vm, egui_context.ctx_mut());
        vm.current_action = Some((last, interactive));
        if r == ActionResult::Stop {
            vm.stop_interactive();
        } else if r == ActionResult::Commit {
            vm.commit_interactive();
        }
    }
    egui::SidePanel::left("Lemmas").show(egui_context.ctx_mut(), |ui| {
        ui::lemmas_menu(ui, vm.as_mut())
    });

    egui::CentralPanel::default().show(egui_context.ctx_mut(), |ui| {
        ui::toolbar(ui, &mut vm.as_mut());
        ui.add(ui::graph_vm(&mut vm.as_mut()))
    });

    match vm.end_status {
        vm::EndStatus::Success => state.set(vm::EndStatus::Success),
        vm::EndStatus::Failure => state.set(vm::EndStatus::Failure),
        _ => (),
    }

    if let Some(ppp) = vm.ppp {
        settings.scale_factor = ppp as f64;
    }
}

fn save_code_on_exit(path: &str, vm: &VM) {
    let path = std::path::Path::new(path);
    let prefix = path.parent().unwrap();
    std::fs::create_dir_all(prefix).unwrap();
    let mut file = File::create(path).unwrap();
    file.write_all(&vm.code[0..vm.run_until].as_bytes())
        .unwrap();
}

fn on_failure(client: &mut RPC) {
    log::info!("Entering failed state");
    client.finish(false).unwrap_or_else(|err| {
        log::warn!("Couldn't parse finish(false) answer: {:#?}", err);
        panic!()
    });
}

fn failure_system(mut exit: EventWriter<AppExit>, mut vm: ResMut<VM>, cfg: Res<AppConfig>) {
    on_failure(&mut vm.as_mut().ctx.remote);
    if let Some(path) = &cfg.file {
        save_code_on_exit(path, vm.as_ref());
    }
    exit.send(AppExit)
}

fn on_success(client: &mut RPC) {
    log::info!("Entering success state");
    client.finish(true).unwrap_or_else(|err| {
        log::warn!("Couldn't parse finish(true) answer: {:#?}", err);
        panic!()
    });
}

fn success_system(mut exit: EventWriter<AppExit>, mut vm: ResMut<VM>, cfg: Res<AppConfig>) {
    on_success(&mut vm.as_mut().ctx.remote);
    if let Some(path) = &cfg.file {
        save_code_on_exit(path, vm.as_ref());
    }
    exit.send(AppExit)
}

fn embed(state: Option<String>) {
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
    log_panics::init();

    let client = RPC::new(std::io::stdin(), std::io::stdout());
    goal_graph(client, state)
}

#[derive(Parser, Debug)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Embed {
        #[arg(long)]
        state: Option<String>,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Embed { state } => embed(state),
    }
}
