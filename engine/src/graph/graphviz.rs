use crate::data::Context;
use crate::graph::mces::Span;
use crate::graph::Graph;

pub fn viz(graph: &Graph, _ctx: &mut Context) {
    println!("digraph {{");

    // TODO pretty-print objects terms
    for (i, _) in graph.nodes.iter().enumerate() {
        println!("    n{} [label=\"o{}\"];", i, i);
    }

    // TODO pretty-print morphism terms
    for s in 0..graph.nodes.len() {
        for (d, _) in graph.edges[s].iter() {
            println!("    n{} -> n{} [label=\"m{}_{}\"];", s, d, s, d);
        }
    }
    println!("}}");
}

pub fn span_viz(left: &Graph, right: &Graph, span: &Span) {
    let nodes_left_seen: Vec<bool> = left.nodes.iter().map(|_| false).collect();
    let edges_left_seen: Vec<Vec<bool>> = left
        .edges
        .iter()
        .map(|v| v.iter().map(|_| false).collect())
        .collect();
    let nodes_right_seen: Vec<bool> = right.nodes.iter().map(|_| false).collect();

    println!("digraph {{");

    println!("}}");
}
