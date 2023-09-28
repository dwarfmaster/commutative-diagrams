use futures::future;
use futures_util::sink::SinkExt;
use futures_util::{FutureExt, StreamExt, TryFutureExt, TryStreamExt};
use tokio_util::bytes::Bytes;
use warp::http::Uri;
use warp::Filter;

#[tokio::main]
async fn main() {
    let static_assets = warp::path("static").and(warp::fs::dir("../web"));
    let root = warp::path::end().map(|| warp::redirect(Uri::from_static("/static/index.html")));
    let script = warp::path("scripts").and(warp::fs::dir("../web"));
    let websocket = warp::path("wss")
        .and(warp::ws())
        .and(warp::path::param())
        .and_then(ws_handler);
    let routes = static_assets.or(root).or(script).or(websocket);
    warp::serve(routes).run(([127, 0, 0, 1], 8000)).await;
}

pub async fn ws_handler(ws: warp::ws::Ws, id: String) -> Result<impl warp::Reply, warp::Rejection> {
    Ok(ws.on_upgrade(|socket| client_connection(socket, id)))
}

#[derive(Debug)]
pub enum ConnectionError {
    IO(std::io::Error),
    WARP(warp::Error),
}

pub async fn client_connection(ws: warp::ws::WebSocket, id: String) {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut socket_path = temp_dir.path().to_path_buf();
    socket_path.push(std::path::Path::new("socket"));
    let future = tokio::net::UnixStream::connect(socket_path)
        .and_then(|unix_socket| {
            let (unix_sink, unix_stream) = tokio_util::codec::length_delimited::Builder::new()
                .native_endian()
                .new_framed(unix_socket)
                .map_err(ConnectionError::IO)
                .sink_map_err(ConnectionError::IO)
                .split();
            let (ws_sink, ws_stream) = ws
                .map(|msg| msg.map(|data| Bytes::copy_from_slice(data.as_bytes())))
                .with(|data: tokio_util::bytes::BytesMut| -> _ {
                    async move {
                        let r: Result<warp::ws::Message, warp::Error> =
                            Ok(warp::ws::Message::binary(&*data));
                        r
                    }
                })
                .map_err(ConnectionError::WARP)
                .sink_map_err(ConnectionError::WARP)
                .split();
            let ws_to_unix = ws_stream.forward(unix_sink).map(std::mem::drop);
            let unix_to_ws = unix_stream.forward(ws_sink).map(std::mem::drop);
            future::join(ws_to_unix, unix_to_ws).map(|_| -> Result<(), _> { Ok(()) })
        })
        .unwrap_or_else(|err| {
            log::debug!("Error on connection for {}: {:?}", id, err);
        });
    future.await
}
