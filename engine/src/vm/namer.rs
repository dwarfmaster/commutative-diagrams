use crate::vm::graph::GraphId;
use crate::vm::vm::VM;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric1};
use nom::combinator::{eof, value};
use nom::multi::many0_count;
use nom::sequence::delimited;
use nom::IResult;

// Nom parser recognizing a valid name
fn valid_name(input: &str) -> IResult<&str, ()> {
    value(
        (),
        delimited(alpha1, many0_count(alt((alphanumeric1, tag("_")))), eof),
    )(input)
}

fn is_valid_name(name: &String) -> bool {
    valid_name(name).is_ok()
}

impl VM {
    pub fn unset_name(&mut self, id: GraphId) {
        use GraphId::*;
        let prev = match id {
            Node(n) => self.graph.nodes[n].1.name.take(),
            Morphism(src, mph) => self.graph.edges[src][mph].1.name.take(),
            Face(f) => self.graph.faces[f].label.name.take(),
        };
        if let Some(prev) = prev {
            self.names.remove(&prev);
        }
    }

    // If name is already used by another element, returns false without
    // changing anything. If name is already used by the given id, returns true
    // without changing anything. Otherwise set the element with this graphid to
    // name and update the names map accordingly. Also checks if the name is
    // syntactically correct, does nothing and return false if it isn't.
    pub fn rename(&mut self, id: GraphId, name: &String) -> bool {
        use GraphId::*;
        if !is_valid_name(name) {
            return false;
        }
        if let Some(oid) = self.names.get(name) {
            return *oid == id;
        }
        self.unset_name(id);
        self.names.insert(name.clone(), id);
        let name = name.clone();
        match id {
            Node(n) => self.graph.nodes[n].1.name = Some(name),
            Morphism(src, mph) => self.graph.edges[src][mph].1.name = Some(name),
            Face(f) => self.graph.faces[f].label.name = Some(name),
        }
        true
    }

    // Try to set the names from labels: check if the labels are correct names,
    // and if they are use them as names. Only renames element that are not yet
    // named.
    pub fn autoname(&mut self) {
        for n in 0..self.graph.nodes.len() {
            if self.graph.nodes[n].1.name.is_none() {
                let lbl = self.graph.nodes[n].1.label.clone();
                self.rename(GraphId::Node(n), &lbl);
            }
        }
        for src in 0..self.graph.edges.len() {
            for mph in 0..self.graph.edges[src].len() {
                if self.graph.edges[src][mph].1.name.is_none() {
                    let lbl = self.graph.edges[src][mph].1.label.clone();
                    self.rename(GraphId::Morphism(src, mph), &lbl);
                }
            }
        }
        for fce in 0..self.graph.faces.len() {
            if self.graph.faces[fce].label.name.is_none() {
                let lbl = self.graph.faces[fce].label.label.clone();
                self.rename(GraphId::Face(fce), &lbl);
            }
        }
    }
}
