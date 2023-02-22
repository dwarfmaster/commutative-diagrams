// Render to graphviz using unique identifiers as labels, export to json, read
// json, and use it to layout the graph

use crate::vm::VM;
use egui::Pos2;

use itertools::Itertools;
use std::io::Write;
use std::process::{Command, Stdio};

impl VM {
    pub fn layout(&mut self) {
        let graph = &mut self.graph;

        // Spawn the  graphviz process
        log::trace!("Spawning graphviz");
        let mut graphviz = Command::new("dot")
            .arg("-Tjson")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .map_err(|err| {
                log::debug!("Failed to start graphivz: {}", err);
                err
            })
            .expect("Failed to start graphviz");

        // Write graph to dot input
        // It is ok to write synchronously since dot reads its whole input
        // before starting writing to stdout. If it turns out not to be the
        // case, the following code must be started on a thread while the main
        // process reads on stdout. See example here:
        //   https://doc.rust-lang.org/std/process/struct.Stdio.html#method.piped
        log::trace!("Writing the graph to stdin");
        {
            let mut stdin = graphviz.stdin.take().expect("Failed to open stdin");
            write!(stdin, "digraph {{\n").unwrap();
            for n in 0..graph.nodes.len() {
                let lbl = &graph.nodes[n].1;
                if !lbl.hidden {
                    write!(
                        stdin,
                        "  n{} [label=\"{}:{}\", shape=plain];\n",
                        n, n, lbl.label
                    )
                    .unwrap();
                }
            }
            for src in 0..graph.nodes.len() {
                for mph in 0..graph.edges[src].len() {
                    let dst = graph.edges[src][mph].0;
                    let lbl = &graph.edges[src][mph].1;
                    if !lbl.hidden {
                        write!(
                            stdin,
                            "  n{} -> n{} [label=\"{}:{}:{}\", arrowhead=none];\n",
                            src, dst, src, mph, lbl.label
                        )
                        .unwrap();
                    }
                }
            }
            write!(stdin, "}}").unwrap();
        }

        // Run graphviz and read its output as json
        log::trace!("Reading graphviz output");
        let stdout = graphviz.stdout.take().expect("Failed to bind stdout");
        let json: serde_json::Value = serde_json::from_reader(stdout)
            .map_err(|err| {
                log::debug!("Coudln't parse graphivz output: {}", err);
                err
            })
            .expect("Couldn't parse graphviz output as json");

        // Find the coordinates of dots
        log::trace!("Place nodes");
        if !json["objects"].is_null() {
            for object in json["objects"]
                .as_array()
                .expect(".objects should be a list")
            {
                let id = object["label"]
                    .as_str()
                    .expect(".objects[].label should be a string")
                    .split(":")
                    .next()
                    .expect("The label should not be empty")
                    .parse::<usize>()
                    .expect("The first part of the label should be the id");
                let pos = object_pos(object);
                graph.nodes[id].1.pos = pos;
            }
        }

        // Find the coordinates of edges
        log::trace!("Place edges");
        if !json["edges"].is_null() {
            for edge in json["edges"].as_array().expect(".edges should be a list") {
                let (src_id, mph_id) = edge["label"]
                    .as_str()
                    .expect(".edges[].label should be a string")
                    .split(":")
                    .take(2)
                    .map(|s| {
                        s.parse::<usize>()
                            .expect("The beggining of the label should be an integer")
                    })
                    .collect_tuple()
                    .expect("The label should have two :-separated fields at least");
                let coordinates: Vec<Pos2> = edge["pos"]
                    .as_str()
                    .expect(".edges[].pos should be a string")
                    .split(' ')
                    .map(parse_pos)
                    .collect();
                graph.edges[src_id][mph_id].1.shape = split_bspline(coordinates);
                graph.edges[src_id][mph_id].1.label_pos = label_pos(edge);
            }
        }
    }
}

fn parse_pos(str: &str) -> Pos2 {
    let coordinates: Vec<&str> = str.split(',').collect();
    assert_eq!(coordinates.len(), 2, "Position should have two coordinates");
    egui::Pos2::new(
        coordinates[0].parse::<f32>().unwrap(),
        coordinates[1].parse::<f32>().unwrap(),
    )
}

// Find position of object, by looking at ldraw or falling back to pos
fn object_pos(obj: &serde_json::Value) -> Pos2 {
    let pos = parse_pos(
        obj["pos"]
            .as_str()
            .expect(".objects[].pos should be a string"),
    );
    ldraw_t_pos(obj).unwrap_or(pos)
}

// Find position of label, by looking at ldraw or falling back to lp
fn label_pos(obj: &serde_json::Value) -> Pos2 {
    let pos = parse_pos(obj["lp"].as_str().expect(".edges[].lp should be a string"));
    ldraw_t_pos(obj).unwrap_or(pos)
}

// Look for position of T drawing operation in _ldraw_
fn ldraw_t_pos(obj: &serde_json::Value) -> Option<Pos2> {
    obj["_ldraw_"]
        .as_array()
        .iter()
        .flat_map(|v| v.iter())
        .find_map(|cmd| {
            if cmd["op"].as_str() != Some("T") {
                return None;
            }
            let pos = cmd["pt"].as_array()?;
            if pos.len() != 2 {
                return None;
            }
            let x = pos[0].as_f64()? as f32;
            let y = pos[1].as_f64()? as f32;
            Some(Pos2::new(x, y))
        })
}

// Split a b-spline in a list a cubic bezier curves
fn split_bspline(controls: Vec<Pos2>) -> Vec<[Pos2; 4]> {
    assert!(controls.len() >= 4, "There should be at least 4 points");
    assert_eq!(
        controls.len() % 3,
        1,
        "The number of points should be of the form 4+3n"
    );
    let ncurves = 1 + (controls.len() - 4) / 3;
    let mut result: Vec<[Pos2; 4]> = Vec::with_capacity(ncurves);
    for c in 0..ncurves {
        let bezier = [
            controls[3 * c],
            controls[3 * c + 1],
            controls[3 * c + 2],
            controls[3 * c + 3],
        ];
        result.push(bezier);
    }
    result
}
