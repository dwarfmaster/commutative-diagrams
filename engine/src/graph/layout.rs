// Render to graphviz using unique identifiers as labels, export to json, read
// json, and use it to layout the graph

use crate::graph::Graph;
use egui::Pos2;
use lens_rs::*;

use std::collections::HashMap;
use std::io::Write;
use std::process::{Command, Stdio};

impl<NL, EL, FL> Graph<NL, EL, FL> {
    pub fn layout<Pos: Copy, Name: Copy, EPos: Copy, EName: Copy>(
        &mut self,
        node_pos: Pos,
        node_name: Name,
        edge_path: EPos,
        edge_name: EName,
    ) where
        NL: LensMut<Pos, Pos2> + Lens<Name, String>,
        EL: LensMut<EPos, Vec<[Pos2; 4]>> + Lens<EName, String>,
    {
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

        // Keep a hashmap of labels to nodes id/edge id
        let mut nodes: HashMap<String, usize> = HashMap::new();
        let mut edges: HashMap<String, (usize, usize)> = HashMap::new();

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
            for n in 0..self.nodes.len() {
                let lbl = self.nodes[n].1.view_ref(node_name);
                nodes.insert(lbl.to_string(), n);
                write!(stdin, "  {} [label=\"{}\" shape=plain];\n", lbl, lbl).unwrap();
            }
            for src in 0..self.nodes.len() {
                for mph in 0..self.edges[src].len() {
                    let dst = self.edges[src][mph].0;
                    let src_lbl = self.nodes[src].1.view_ref(node_name);
                    let dst_lbl = self.nodes[dst].1.view_ref(node_name);
                    let lbl = self.edges[src][mph].1.view_ref(edge_name);
                    edges.insert(lbl.to_string(), (src, mph));
                    write!(stdin, "  {} -> {} [label=\"{}\"];\n", src_lbl, dst_lbl, lbl).unwrap();
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
        for object in json["objects"]
            .as_array()
            .expect(".objects should be a list")
        {
            let name = object["label"]
                .as_str()
                .expect(".objects[].label should be a string");
            let id = nodes.get(name).expect("The label should have been seen");
            let pos = object_pos(object);
            *self.nodes[*id].1.view_mut(node_pos) = pos;
        }

        // Find the coordinates of edges
        log::trace!("Place edges");
        for edge in json["edges"].as_array().expect(".edges should be a list") {
            let name = edge["label"]
                .as_str()
                .expect(".edges[].label should be a string");
            let (src_id, mph_id) = edges.get(name).expect("The label should have been seen");
            let coordinates: Vec<Pos2> = edge["pos"]
                .as_str()
                .expect(".edges[].pos should be a string")
                .split(' ')
                .skip(1) // The first element e,... is not a control point
                .map(parse_pos)
                .collect();
            *self.edges[*src_id][*mph_id].1.view_mut(edge_path) = split_bspline(coordinates);
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

// Look for object position in _ldraw_, falling back to pos if it fails
fn object_pos(obj: &serde_json::Value) -> Pos2 {
    let pos = parse_pos(
        obj["pos"]
            .as_str()
            .expect(".objects[].pos should be a string"),
    );
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
        .unwrap_or(pos)
}

// Split a b-spline in a list a cubic bezier curves
fn split_bspline(controls: Vec<Pos2>) -> Vec<[Pos2; 4]> {
    assert!(controls.len() >= 4, "There should be at least 4 points");
    assert_eq!(
        controls.len() % 3,
        1,
        "The number of points should be of the from 4+3n"
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
