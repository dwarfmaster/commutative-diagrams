use commutative_diagrams_engine_lib::{remote, ui};
use eframe::WebRunner;
use std::time::Duration;
use wasm_bindgen::prelude::*;

mod io;
mod js;

// Handle to the web app from javascript
#[derive(Clone)]
#[wasm_bindgen]
pub struct WebHandle {
    runner: WebRunner,
}

#[wasm_bindgen]
impl WebHandle {
    #[allow(clippy::new_without_default)]
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        // Redirect log messages to console.log
        eframe::WebLogger::init(log::LevelFilter::Trace).ok();
        Self {
            runner: WebRunner::new(),
        }
    }

    #[wasm_bindgen]
    pub async fn start(&self, canvas_id: &str, vfile: &str) -> Result<(), wasm_bindgen::JsValue> {
        let url = format!("ws://127.0.0.1:8000/wss/{}", vfile);
        let ws = io::WSSync::new(&url).await;
        self.runner
            .start(
                canvas_id,
                eframe::WebOptions::default(),
                Box::new(move |_cc| {
                    Box::new(EguiApp::new(ws))
                }),
            )
            .await
    }

    #[wasm_bindgen]
    pub fn destroy(&self) {
        self.runner.destroy()
    }

    #[wasm_bindgen]
    pub fn has_panicked(&self) -> bool {
        self.runner.has_panicked()
    }

    #[wasm_bindgen]
    pub fn panic_message(&self) -> Option<String> {
        self.runner.panic_summary().map(|s| s.message())
    }

    #[wasm_bindgen]
    pub fn panic_callstack(&self) -> Option<String> {
        self.runner.panic_summary().map(|s| s.callstack())
    }
}

type RPC = remote::RPC<io::WSSync, io::WSSync>;
type VM = ui::VM<RPC>;

struct EguiApp {
    vm: VM,
}

impl EguiApp {
    pub fn new(ws: io::WSSync) -> Self {
        let rpc = remote::RPC::new(ws.clone(), ws);
        Self { vm: VM::start(rpc) }
    }
}

impl eframe::App for EguiApp {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        ui::main(ctx, &mut self.vm);
        ctx.request_repaint_after(Duration::from_millis(33));
    }
}
