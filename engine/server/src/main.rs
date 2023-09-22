use warp::http::Uri;
use warp::Filter;

#[tokio::main]
async fn main() {
    let static_assets = warp::path("static").and(warp::fs::dir("../web"));
    let root = warp::path::end().map(|| warp::redirect(Uri::from_static("/static/index.html")));
    let script = warp::path("scripts").and(warp::fs::dir("../web"));
    let routes = static_assets.or(root).or(script);
    warp::serve(routes).run(([127, 0, 0, 1], 8000)).await;
}
