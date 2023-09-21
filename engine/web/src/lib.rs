use commutative_diagrams_engine_lib::{data, graph, remote, ui};
use eframe::WebRunner;
use std::time::Duration;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    pub fn alert(s: &str);
}

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
    pub async fn start(&self, canvas_id: &str) -> Result<(), wasm_bindgen::JsValue> {
        self.runner
            .start(
                canvas_id,
                eframe::WebOptions::default(),
                Box::new(|_cc| Box::new(EguiApp::new())),
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

type RPC = remote::Mock;
type VM = ui::VM<RPC>;

struct EguiApp {
    vm: VM,
}

impl EguiApp {
    pub fn new() -> Self {
        use data::EvarStatus::Grounded;
        use data::Feature;
        use graph::{FaceParsed, GraphParsed};
        let mut ctx = RPC::new();
        let cat = ctx.new_term("C".to_string(), None, Grounded);
        ctx.add_feat(cat, Feature::Category);
        let x = ctx.new_term("x".to_string(), None, Grounded);
        ctx.add_feat(x, Feature::Object { cat });
        let m = ctx.new_term("m".to_string(), None, Grounded);
        ctx.add_feat(
            m,
            Feature::Morphism {
                cat,
                src: x,
                dst: x,
            },
        );
        let mm = ctx.new_term("m o m".to_string(), None, Grounded);
        ctx.add_feat(
            mm,
            Feature::Morphism {
                cat,
                src: x,
                dst: x,
            },
        );
        ctx.add_feat(
            mm,
            Feature::ComposeMph {
                cat,
                src: x,
                mid: x,
                dst: x,
                m1: m,
                m2: m,
            },
        );
        let eq = ctx.new_term("H".to_string(), None, Grounded);
        ctx.add_feat(
            eq,
            Feature::Equality {
                cat,
                src: x,
                dst: x,
                left: m,
                right: mm,
            },
        );

        let face = FaceParsed {
            start: 0,
            end: 2,
            left: vec![2],
            right: vec![1, 0],
            eq,
            label: Default::default(),
        };
        let gr = GraphParsed {
            nodes: vec![
                (x, cat, Default::default()),
                (x, cat, Default::default()),
                (x, cat, Default::default()),
            ],
            edges: vec![
                vec![
                    (1, Default::default(), m, ()),
                    (1, Default::default(), m, ()),
                    (2, Default::default(), m, ()),
                ],
                vec![(2, Default::default(), m, ())],
                vec![],
            ],
            faces: vec![face],
        };
        ctx.set_graph(gr);
        Self { vm: VM::start(ctx) }
    }
}

impl eframe::App for EguiApp {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        ui::main(ctx, &mut self.vm);
        ctx.request_repaint_after(Duration::from_millis(33));
    }
}
