use warp::http::Uri;
use warp::Filter;
// use futures_util::StreamExt;
use futures_util::sink::SinkExt;
use futures_util::FutureExt;

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

pub async fn client_connection(mut ws: warp::ws::WebSocket, id: String) {
    // let (ws_snd, mut ws_rcv) = ws.split();
    ws.send(warp::ws::Message::text(format!("Hello {}\n", id)))
        .map(|_result| ())
        .await
}
