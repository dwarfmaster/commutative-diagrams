use crate::data::Context;
use crate::graph::mces::Span;
use crate::graph::Graph;

pub fn viz<F, NL, EL, FL>(
    fmt: &mut F,
    graph: &Graph<NL, EL, FL>,
    ctx: &mut Context,
) -> Result<(), std::io::Error>
where
    F: std::io::Write,
{
    writeln!(fmt, "digraph {{")?;

    for (i, o) in graph.nodes.iter().enumerate() {
        writeln!(fmt, "    n{} [label=\"{}\"];", i, o.0.render(ctx, 100))?;
    }

    for s in 0..graph.nodes.len() {
        for (d, _, e) in graph.edges[s].iter() {
            writeln!(
                fmt,
                "    n{} -> n{} [label=\"{}\"];",
                s,
                d,
                e.render(ctx, 100)
            )?;
        }
    }
    writeln!(fmt, "}}")
}

pub fn span_viz<F, NL, EL, FL>(
    fmt: &mut F,
    ctx: &mut Context,
    left: &Graph<NL, EL, FL>,
    right: &Graph<NL, EL, FL>,
    span: &Span,
) -> Result<(), std::io::Error>
where
    F: std::io::Write,
{
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

    writeln!(fmt, "digraph {{")?;

    // Print common part
    for (c, (l, r)) in span.nodes.iter().enumerate() {
        nodes_left_seen[*l] = Some(c);
        nodes_right_seen[*r] = Some(c);
        writeln!(
            fmt,
            "    c{} [ color=\"{}\" label=\"{}\" ];",
            c,
            color_common,
            left.nodes[*l].0.render(ctx, 100)
        )?;
    }
    for s in 0..span.nodes.len() {
        for (l, r) in span.edges[s].iter() {
            let ld = left.edges[span.nodes[s].0][*l].0.clone();
            let e = left.edges[span.nodes[s].0][*l].2.clone();
            let d = nodes_left_seen[ld].unwrap();
            edges_left_seen[span.nodes[s].0][*l] = true;
            edges_right_seen[span.nodes[s].1][*r] = true;
            writeln!(
                fmt,
                "    c{} -> c{} [ color=\"{}\" label=\"{}\" ];",
                s,
                d,
                color_common,
                e.render(ctx, 100)
            )?;
        }
    }

    // Print left part
    for (i, o) in left.nodes.iter().enumerate() {
        if nodes_left_seen[i].is_some() {
            continue;
        }
        writeln!(
            fmt,
            "    l{} [ color=\"{}\" label=\"{}\" ];",
            i,
            color_left,
            o.0.render(ctx, 100)
        )?;
    }
    for s in 0..left.nodes.len() {
        let src = nodes_left_seen[s]
            .map(|s| format!("c{}", s))
            .unwrap_or(format!("l{}", s));
        for (e, (d, _, m)) in left.edges[s].iter().enumerate() {
            if edges_left_seen[s][e] {
                continue;
            }
            let dst = nodes_left_seen[*d]
                .map(|d| format!("c{}", d))
                .unwrap_or(format!("l{}", d));
            writeln!(
                fmt,
                "    {} -> {} [ color=\"{}\" label=\"{}\" ];",
                src,
                dst,
                color_left,
                m.render(ctx, 100)
            )?;
        }
    }

    // Print right part
    for (i, o) in right.nodes.iter().enumerate() {
        if nodes_right_seen[i].is_some() {
            continue;
        }
        writeln!(
            fmt,
            "    r{} [ color=\"{}\" label=\"{}\" ];",
            i,
            color_right,
            o.0.render(ctx, 100)
        )?;
    }
    for s in 0..right.nodes.len() {
        let src = nodes_right_seen[s]
            .map(|s| format!("c{}", s))
            .unwrap_or(format!("r{}", s));
        for (e, (d, _, m)) in right.edges[s].iter().enumerate() {
            if edges_right_seen[s][e] {
                continue;
            }
            let dst = nodes_right_seen[*d]
                .map(|d| format!("c{}", d))
                .unwrap_or(format!("r{}", d));
            writeln!(
                fmt,
                "    {} -> {} [ color=\"{}\" label=\"{}\" ];",
                src,
                dst,
                color_right,
                m.render(ctx, 100)
            )?;
        }
    }

    writeln!(fmt, "}}")
}
