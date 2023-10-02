use crate::js;
use std::collections::VecDeque;
use std::io::{Read, Write};
use std::sync::{Arc, Mutex};
use wasm_bindgen::prelude::{Closure, JsCast};
use web_sys::{MessageEvent, WebSocket};
use futures::channel::oneshot::{channel,Sender,Receiver};

#[derive(Clone)]
pub struct WSSync {
    ws: WebSocket,
    queue: Arc<Mutex<VecDeque<u8>>>,
}

impl WSSync {
    pub async fn new(url: &str) -> Self {
        let ws = WebSocket::new(url).unwrap_or_else(|err| {
            js::console_log!("Couldn't connect to {}: {:?}", url, err);
            panic!();
        });
        ws.set_binary_type(web_sys::BinaryType::Arraybuffer);
        let queue = Arc::new(Mutex::new(VecDeque::new()));
        let rqueue = queue.clone();
        let onmessage_callback = Closure::<dyn FnMut(_)>::new(move |e: MessageEvent| {
            // Handle difference between Text/Binary
            if let Ok(buf) = e.data().dyn_into::<js_sys::ArrayBuffer>() {
                let array = js_sys::Uint8Array::new(&buf);
                array.for_each(&mut |v, _, _| rqueue.clone().lock().unwrap().push_back(v));
            } else if let Ok(buf) = e.data().dyn_into::<js_sys::JsString>() {
                let array = js_sys::Uint8Array::new(&buf);
                array.for_each(&mut |v, _, _| rqueue.clone().lock().unwrap().push_back(v));
            } else {
                js::console_log!("Received unknown message event: {:?}", e.data());
            }
        });
        ws.set_onmessage(Some(onmessage_callback.as_ref().unchecked_ref()));
        onmessage_callback.forget();

        let (tx,rx) : (Sender<()>, Receiver<()>) = channel();
        let onopen_callback = Closure::once(move || {
            tx.send(()).expect("Send on channel")
        });
        ws.set_onopen(Some(onopen_callback.as_ref().unchecked_ref()));
        onopen_callback.forget();

        js::console_log!("Waiting for websocket to open");
        let () = rx.await.expect("Receive on channel");
        js::console_log!("Websocket opened");
        WSSync { ws, queue }
    }
}

impl Read for WSSync {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        if self.ws.ready_state() == WebSocket::CONNECTING {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "websocket still connecting",
            ));
        } else if self.ws.ready_state() != WebSocket::OPEN {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "websocket closed",
            ));
        }

        let mut queue = self.queue.lock().unwrap();
        js::console_log!("Reading {} bytes from queue of length {}", buf.len(), queue.len());
        let to_drain = buf.len().min(queue.len());
        let d = queue.drain(0..to_drain);
        let mut copied = 0;
        for i in d {
            buf[copied] = i;
            copied += 1;
        }
        Ok(copied)
    }
}

impl Write for WSSync {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if self.ws.ready_state() == WebSocket::CONNECTING {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "websocket still connecting",
            ));
        } else if self.ws.ready_state() != WebSocket::OPEN {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "websocket closed",
            ));
        }

        let len = buf.len();
        self.ws.send_with_u8_array(buf).map_err(|err| {
            std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Couldn't send: {:?}", err),
            )
        })?;
        Ok(len)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        // Nothing to do
        Ok(())
    }
}
