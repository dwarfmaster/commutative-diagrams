use clap::{Parser, Subcommand};
use commutative_diagrams_engine_lib::runtime::Runtime;
use commutative_diagrams_engine_lib::{remote, ui, vm};
use futures::lock::Mutex;
use remote::Remote;
use std::fs::File;
use std::future::Future;
use std::io::{Read, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

type RPC = remote::RPC<std::io::Stdin, std::io::Stdout>;
type VM = ui::VM<RPC>;

enum State {
    File(String),
    Script(String),
    None,
}

struct VMRuntime {
    vm: Arc<Mutex<VM>>,
    running: Arc<AtomicBool>,
    description: String,
    tokio: Option<tokio::runtime::Runtime>,
}

impl Runtime for VMRuntime {
    type Rem = RPC;

    fn running<'a>(&'a self) -> Option<&'a str> {
        let is_running = self.running.load(Ordering::SeqCst);
        if is_running {
            Some(&self.description)
        } else {
            None
        }
    }

    fn run<Fut: Future + Send, F>(&mut self, desc: &str, f: F)
    where
        F: FnOnce(&VM) -> Fut + Send + 'static,
    {
        self.running.store(true, Ordering::SeqCst);
        self.description = desc.to_string();

        let vm = self.vm.clone();
        let running = self.running.clone();
        self.tokio.as_ref().unwrap().spawn(async move {
            let vm = vm.lock().await;
            let _ = f(&vm).await;
            running.store(false, Ordering::SeqCst);
        });
    }
}

struct App {
    state: State,
    copying: bool,
    runtime: VMRuntime,
}

impl App {
    pub fn new(_cc: &eframe::CreationContext<'_>, state: State, vm: VM) -> Self {
        let rt = tokio::runtime::Runtime::new().expect("Create the tokio runtime");
        Self {
            state,
            copying: false,
            runtime: VMRuntime {
                description: String::new(),
                running: Arc::new(AtomicBool::new(false)),
                vm: Arc::new(Mutex::new(vm)),
                tokio: Some(rt),
            },
        }
    }

    pub fn finish(&mut self) {
        self.runtime
            .tokio
            .take()
            .unwrap()
            .shutdown_timeout(Duration::from_secs_f32(1.0));
    }

    pub fn success(&mut self) -> bool {
        self.finish();
        let vm = &mut self
            .runtime
            .vm
            .try_lock()
            .expect("No other references should be left");
        match &self.state {
            State::File(path) => {
                save_code_on_exit(path, &vm);
                on_success(&mut vm.ctx.remote);
                true
            }
            State::Script(_) => {
                if self.copying {
                    on_success(&mut vm.ctx.remote);
                    return true;
                }
                self.copying = true;
                false
            }
            State::None => {
                on_success(&mut vm.ctx.remote);
                true
            }
        }
    }

    pub fn failure(&mut self) -> bool {
        self.finish();
        let vm = &mut self.runtime.vm.try_lock().expect("No other references should be left");
        on_failure(&mut vm.ctx.remote);
        match &self.state {
            State::File(path) => {
                save_code_on_exit(path, &vm);
            }
            State::Script(_) => (),
            State::None => (),
        }
        true
    }
}
impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        if self.copying {
            let vm = &mut self.runtime.vm.try_lock().expect("No other references should be left");
            if ui::exit(ctx, vm) {
                on_success(&mut vm.ctx.remote);
                frame.close();
            }
        } else {
            // TODO remove mut once refactoring has reached this point
            let end = {
                let vm = &mut self.runtime.vm.try_lock().unwrap();
                ui::main(ctx, vm);
                vm.end_status.clone()
            };
            match end {
                vm::EndStatus::Success => {
                    if self.success() {
                        frame.close();
                    }
                }
                vm::EndStatus::Failure => {
                    if self.failure() {
                        frame.close();
                    }
                }
                _ => (),
            }
        }
        ctx.request_repaint_after(std::time::Duration::from_secs_f32(0.033));
    }
}

fn init_vm_code_from_file(vm: &mut VM, path: &str, edit: bool) -> bool {
    let file = File::open(path);
    if file.is_err() {
        return false;
    }
    let mut file = file.unwrap();

    if file.read_to_string(&mut vm.code.code).is_err() {
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
            vm.code.code = script.to_string();
            if init_vm_code(&mut vm, edit) {
                log::info!("Ending without opening the ui");
                return;
            }
        }
        State::None => (),
    }

    // Run the ui if necessary
    log::info!("Running the ui");
    let native_options = eframe::NativeOptions {
        renderer: eframe::Renderer::Wgpu,
        ..eframe::NativeOptions::default()
    };
    eframe::run_native(
        "ComDiag",
        native_options,
        Box::new(|cc| Box::new(App::new(cc, state, vm))),
    )
    .expect("Failed to run eframe");
}

fn save_code_on_exit(path: &str, vm: &VM) {
    let path = std::path::Path::new(path);
    let prefix = path.parent().unwrap();
    std::fs::create_dir_all(prefix).unwrap();
    let mut file = File::create(path).unwrap();
    file.write_all(&vm.code.code[0..vm.code.run_until].as_bytes())
        .unwrap();
    for line in vm.code.code[vm.code.run_until..].split("\n") {
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

fn on_success(client: &mut RPC) {
    log::info!("Entering success state");
    client.finish(true).unwrap_or_else(|err| {
        log::warn!("Couldn't parse finish(true) answer: {:#?}", err);
        panic!()
    });
}

fn embed(state: State, edit: bool) {
    simplelog::WriteLogger::init(
        simplelog::LevelFilter::max(),
        simplelog::ConfigBuilder::new()
            .add_filter_ignore("wgpu_hal".to_string())
            .add_filter_ignore("wgpu_core".to_string())
            .add_filter_ignore("objects".to_string())
            .add_filter_ignore("winit".to_string())
            .add_filter_ignore("naga".to_string())
            .add_filter_ignore("mio".to_string())
            .add_filter_ignore("eframe".to_string())
            .add_filter_ignore("egui_winit".to_string())
            .add_filter_ignore("async_io".to_string())
            .add_filter_ignore("polling".to_string())
            .add_filter_ignore("arboard".to_string())
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
