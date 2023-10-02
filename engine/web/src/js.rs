use wasm_bindgen::prelude::*;

macro_rules! console_log {
    ($($t:tt)*) => (crate::js::log(&format_args!($($t)*).to_string()))
}
pub(crate) use console_log;
#[wasm_bindgen]
extern "C" {
    pub fn alert(s: &str);
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);
}
