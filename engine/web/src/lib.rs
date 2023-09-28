use commutative_diagrams_engine_lib::{data, graph, remote, ui};
use eframe::WebRunner;
use std::time::Duration;
use wasm_bindgen::prelude::*;
use web_sys::{MessageEvent, WebSocket};

macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}
#[wasm_bindgen]
extern "C" {
    pub fn alert(s: &str);
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);
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
    pub async fn start(&self, canvas_id: &str, vfile: &str) -> Result<(), wasm_bindgen::JsValue> {
        let url = format!("ws://127.0.0.1:8000/wss/{}", vfile);
        self.runner
            .start(
                canvas_id,
                eframe::WebOptions::default(),
                Box::new(move |_cc| {
                    let ws = WebSocket::new(&url)
                        .unwrap_or_else(|err| {
                            console_log!("Error: {:?}", err);
                            panic!();
                        });
                    ws.set_binary_type(web_sys::BinaryType::Arraybuffer);
                    console_log!("Connected to {}", ws.url());
                    let cloned_ws = ws.clone();
                    let onmessage_callback =
                        Closure::<dyn FnMut(_)>::new(move |e: MessageEvent| {
                            // Handle difference Text/Binary,...
                            if let Ok(abuf) = e.data().dyn_into::<js_sys::ArrayBuffer>() {
                                console_log!("message event, received arraybuffer: {:?}", abuf);
                                let array = js_sys::Uint8Array::new(&abuf);
                                let len = array.byte_length() as usize;
                                console_log!(
                                    "Arraybuffer received {}bytes: {:?}",
                                    len,
                                    array.to_vec()
                                );
                                // here you can for example use Serde Deserialize decode the message
                                // for demo purposes we switch back to Blob-type and send off another binary message
                                cloned_ws.set_binary_type(web_sys::BinaryType::Blob);
                                match cloned_ws.send_with_u8_array(&[5, 6, 7, 8]) {
                                    Ok(_) => console_log!("binary message successfully sent"),
                                    Err(err) => console_log!("error sending message: {:?}", err),
                                }
                            } else if let Ok(blob) = e.data().dyn_into::<web_sys::Blob>() {
                                console_log!("message event, received blob: {:?}", blob);
                                // better alternative to juggling with FileReader is to use https://crates.io/crates/gloo-file
                                let fr = web_sys::FileReader::new().unwrap();
                                let fr_c = fr.clone();
                                // create onLoadEnd callback
                                let onloadend_cb = Closure::<dyn FnMut(_)>::new(
                                    move |_e: web_sys::ProgressEvent| {
                                        let array =
                                            js_sys::Uint8Array::new(&fr_c.result().unwrap());
                                        let len = array.byte_length() as usize;
                                        console_log!(
                                            "Blob received {}bytes: {:?}",
                                            len,
                                            array.to_vec()
                                        );
                                        // here you can for example use the received image/png data
                                    },
                                );
                                fr.set_onloadend(Some(onloadend_cb.as_ref().unchecked_ref()));
                                fr.read_as_array_buffer(&blob).expect("blob not readable");
                                onloadend_cb.forget();
                            } else if let Ok(txt) = e.data().dyn_into::<js_sys::JsString>() {
                                console_log!("message event, received Text: {:?}", txt);
                            } else {
                                console_log!("message event, received Unknown: {:?}", e.data());
                            }
                        });
                    ws.set_onmessage(Some(onmessage_callback.as_ref().unchecked_ref()));
                    onmessage_callback.forget();
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

type RPC = remote::Mock;
type VM = ui::VM<RPC>;

struct EguiApp {
    vm: VM,
    ws: WebSocket,
}

impl EguiApp {
    pub fn new(ws: WebSocket) -> Self {
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
            ws,
        }
    }
}

impl eframe::App for EguiApp {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        ui::main(ctx, &mut self.vm);
        ctx.request_repaint_after(Duration::from_millis(33));
    }
}
