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
    let mut nodes_left_seen: Vec<Option<usize>> = left.nodes.iter().map(|_| None).collect();
    let mut edges_left_seen: Vec<Vec<bool>> = left
        .edges
        .iter()
        .map(|v| v.iter().map(|_| false).collect())
        .collect();
    let mut nodes_right_seen: Vec<Option<usize>> = right.nodes.iter().map(|_| None).collect();
    let mut edges_right_seen: Vec<Vec<bool>> = right
        .edges
        .iter()
        .map(|v| v.iter().map(|_| false).collect())
        .collect();

    let color_common = "green";
    let color_left = "blue";
    let color_right = "red";

    println!("digraph {{");

    // Print common part
    for (c, (l, r)) in span.nodes.iter().enumerate() {
        nodes_left_seen[*l] = Some(c);
        nodes_right_seen[*r] = Some(c);
        println!(
            "    c{} [ color=\"{}\" label=\"c_{}_{}\" ];",
            c, color_common, l, r
        );
    }
    for s in 0..span.nodes.len() {
        for (l, r) in span.edges[s].iter() {
            let d = nodes_left_seen[left.edges[span.nodes[s].0][*l].0].unwrap();
            edges_left_seen[span.nodes[s].0][*l] = true;
            edges_right_seen[span.nodes[s].1][*r] = true;
            println!(
                "    c{} -> c{} [ color=\"{}\" label=\"e_{}_{}\" ];",
                s, d, color_common, *l, *r
            );
        }
    }

    // Print left part
    for (i, _o) in left.nodes.iter().enumerate() {
        if nodes_left_seen[i].is_some() {
            continue;
        }
        println!("    l{} [ color=\"{}\" label=\"l_{}\" ];", i, color_left, i);
    }
    for s in 0..left.nodes.len() {
        let src = nodes_left_seen[s]
            .map(|s| format!("c{}", s))
            .unwrap_or(format!("l{}", s));
        for (e, (d, _m)) in left.edges[s].iter().enumerate() {
            if edges_left_seen[s][e] {
                continue;
            }
            let dst = nodes_left_seen[*d]
                .map(|d| format!("c{}", d))
                .unwrap_or(format!("l{}", d));
            println!(
                "    {} -> {} [ color=\"{}\" label=\"l_{}\" ];",
                src, dst, color_left, e
            );
        }
    }

    // Print right part
    for (i, _o) in right.nodes.iter().enumerate() {
        if nodes_right_seen[i].is_some() {
            continue;
        }
        println!(
            "    r{} [ color=\"{}\" label=\"r_{}\" ];",
            i, color_right, i
        );
    }
    for s in 0..right.nodes.len() {
        let src = nodes_right_seen[s]
            .map(|s| format!("c{}", s))
            .unwrap_or(format!("r{}", s));
        for (e, (d, _m)) in right.edges[s].iter().enumerate() {
            if edges_right_seen[s][e] {
                continue;
            }
            let dst = nodes_right_seen[*d]
                .map(|d| format!("c{}", d))
                .unwrap_or(format!("r{}", d));
            println!(
                "    {} -> {} [ color=\"{}\" label=\"r_{}\" ];",
                src, dst, color_right, e
            );
        }
    }

    println!("}}");
}
