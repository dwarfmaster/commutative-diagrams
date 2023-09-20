use commutative_diagrams_engine_lib::{remote,ui,vm};

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

enum State {
    File(String),
    Script(String),
    None,
}

#[derive(Resource)]
struct AppConfig {
    state: State,
}

fn init_vm_code_from_file(vm: &mut VM, path: &str, edit: bool) -> bool {
    let file = File::open(path);
    if file.is_err() {
        return false;
    }
    let mut file = file.unwrap();

    if file.read_to_string(&mut vm.code).is_err() {
        return false;
    }
    init_vm_code(vm, edit)
}

// Return true if the code has succeeded, ie no ui should be started
fn init_vm_code(vm: &mut VM, edit: bool) -> bool {
    if edit {
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

fn goal_graph(client: RPC, state: State, edit: bool) {
    // Open the vm
    let mut vm = vm::VM::start(client);

    // Open the file
    match &state {
        State::File(path) => {
            log::info!("Reading code from file");
            if init_vm_code_from_file(&mut vm, path, edit) {
                log::info!("Ending without opening the ui");
                return;
            }
        }
        State::Script(script) => {
            vm.code = script.to_string();
            if init_vm_code(&mut vm, edit) {
                log::info!("Ending without opening the ui");
                return;
            }
        }
        State::None => (),
    }

    // Run the ui if necessary
    log::info!("Running the ui");
    let mut app = App::new();
    app.add_plugins(DefaultPlugins.build().disable::<bevy::log::LogPlugin>())
        .add_plugins(EguiPlugin)
        .add_state::<vm::EndStatus>()
        .insert_resource(vm)
        .insert_resource(AppConfig { state })
        .add_systems(
            Update,
            goal_ui_system.run_if(in_state(vm::EndStatus::Running)),
        )
        .add_systems(
            Update,
            success_system.run_if(in_state(vm::EndStatus::Success)),
        )
        .add_systems(
            Update,
            failure_system.run_if(in_state(vm::EndStatus::Failure)),
        );
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
        let fixed = |id| vm.dragged_object == Some(id) || vm.graph.pinned(id);
        vm.layout.apply_forces(&vm.config, &vm.graph, &fixed);
        vm.layout.update(&vm.config);

        if let Some(lem) = vm.selected_lemma {
            if vm.lemmas[lem].pattern.is_some() {
                let dragged = vm.lemmas[lem].graphical_state.dragged;
                let lem = &mut vm.lemmas[lem];
                let fixed = |id| dragged == Some(id) || lem.pattern.as_ref().unwrap().pinned(id);
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
                        let fixed = |id| state.dragged == Some(id) || state.graph.pinned(id);
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
    for line in vm.code[vm.run_until..].split("\n") {
        if line.is_empty() {
            continue;
        }
        if line.chars().nth(0) == Some('#') {
            file.write_all(b"\n").unwrap();
        } else {
            file.write_all(b"\n# ").unwrap();
        }
        file.write_all(line.as_bytes()).unwrap();
    }
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
    match &cfg.state {
        State::File(path) => {
            save_code_on_exit(path, vm.as_ref());
        }
        State::Script(_) => (),
        State::None => (),
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

fn success_system(
    mut egui_context: EguiContexts,
    mut exit: EventWriter<AppExit>,
    mut vm: ResMut<VM>,
    cfg: Res<AppConfig>,
) {
    match &cfg.state {
        State::File(path) => {
            save_code_on_exit(path, vm.as_ref());
            on_success(&mut vm.as_mut().ctx.remote);
            exit.send(AppExit)
        }
        State::Script(_) => {
            if ui::exit(egui_context.ctx_mut(), vm.as_mut()) {
                on_success(&mut vm.as_mut().ctx.remote);
                exit.send(AppExit)
            }
        }
        State::None => {
            on_success(&mut vm.as_mut().ctx.remote);
            exit.send(AppExit);
        }
    }
}

fn embed(state: State, edit: bool) {
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
    goal_graph(client, state, edit)
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
        #[arg(long)]
        edit: bool,
    },
    Execute {
        #[arg(long)]
        script: String,
        #[arg(long)]
        edit: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Embed { state, edit } => {
            embed(state.map(|f| State::File(f)).unwrap_or(State::None), edit)
        }
        Commands::Execute { script, edit } => embed(State::Script(script), edit),
    }
}
