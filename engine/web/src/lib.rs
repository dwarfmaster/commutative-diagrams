use commutative_diagrams_engine_lib::{remote, ui, data, graph};
use eframe::egui;
use eframe::WebRunner;
use ui::ActionResult;
use wasm_bindgen::prelude::*;
use std::time::Duration;

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
        Self {
            vm: VM::start(ctx),
        }
    }
}

impl eframe::App for EguiApp {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        let fixed = |id| self.vm.dragged_object == Some(id) || self.vm.graph.pinned(id);
        self.vm.layout.apply_forces(&self.vm.config, &self.vm.graph, &fixed);
        self.vm.layout.update(&self.vm.config);

        ui::lemmas_window(ctx, &mut self.vm);
        ui::code(ctx, &mut self.vm);
        if let Some((last, mut interactive)) = self.vm.current_action.take() {
            let r = interactive.display(&mut self.vm, ctx);
            self.vm.current_action = Some((last, interactive));
            if r == ActionResult::Stop {
                self.vm.stop_interactive();
            } else if r == ActionResult::Commit {
                self.vm.commit_interactive();
            }
        }
        egui::SidePanel::left("Lemmas").show(ctx, |ui| ui::lemmas_menu(ui, &mut self.vm));

        egui::CentralPanel::default().show(ctx, |ui| {
            ui::toolbar(ui, &mut self.vm);
            ui.add(ui::graph_vm(&mut self.vm))
        });

        ctx.request_repaint_after(Duration::from_millis(33));
    }
}
