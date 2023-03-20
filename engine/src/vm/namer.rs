use crate::data::{ActualEquality, ActualProofObject};
use crate::vm::graph::GraphId;
use crate::vm::vm::VM;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric1};
use nom::combinator::{eof, value};
use nom::multi::many0_count;
use nom::sequence::delimited;
use nom::IResult;
use std::ops::Deref;

type Ins = crate::vm::asm::Instruction;

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
    // Set name without any checks, assuming the previous name is empty
    fn set_name(&mut self, id: GraphId, name: String) {
        use GraphId::*;
        self.names.insert(name.clone(), id);
        match id {
            Node(n) => self.graph.nodes[n].1.name = name,
            Morphism(src, mph) => self.graph.edges[src][mph].1.name = name,
            Face(fce) => self.graph.faces[fce].label.name = name,
        }
    }

    pub fn get_name(&self, id: GraphId) -> String {
        use GraphId::*;
        match id {
            Node(n) => self.graph.nodes[n].1.name.clone(),
            Morphism(src, mph) => self.graph.edges[src][mph].1.name.clone(),
            Face(f) => self.graph.faces[f].label.name.clone(),
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
        let prev = self.get_name(id);
        let ins = match id {
            Node(nd) => Ins::RenameNode(nd, prev, name.clone()),
            Morphism(src, mph) => Ins::RenameMorphism(src, mph, prev, name.clone()),
            Face(fce) => Ins::RenameFace(fce, prev, name.clone()),
        };
        self.register_instruction(ins);
        true
    }

    pub fn autoname_node(&mut self, nd: usize) {
        assert!(self.graph.nodes[nd].1.name.is_empty());
        let lbl = &self.graph.nodes[nd].1.label;
        if is_valid_name(lbl) && self.names.get(lbl).is_none() {
            self.set_name(GraphId::Node(nd), lbl.clone());
            return;
        }

        // Try to name it from the category
        let cat_name = self.graph.nodes[nd]
            .0
            .cat(&self.ctx)
            .render(&mut self.ctx, 100)
            .to_lowercase();
        let base_name = if is_valid_name(&cat_name) {
            cat_name
        } else {
            "x".to_string()
        };
        let mut name = base_name.clone();
        let mut count = 0;
        while self.names.get(&name).is_some() {
            name = format!("{}_{}", base_name, count);
            count += 1;
        }
        self.set_name(GraphId::Node(nd), name);
    }

    pub fn autoname_morphism(&mut self, src: usize, mph: usize) {
        assert!(self.graph.edges[src][mph].1.name.is_empty());
        let lbl = &self.graph.edges[src][mph].1.label;
        if is_valid_name(lbl) && self.names.get(lbl).is_none() {
            self.set_name(GraphId::Morphism(src, mph), lbl.clone());
            return;
        }

        // Try to use src and dst to name it
        let src_name = if is_valid_name(&self.graph.nodes[src].1.label) {
            self.graph.nodes[src].1.label.clone()
        } else {
            "".to_string()
        };
        let dst = self.graph.edges[src][mph].0;
        let dst_name = if is_valid_name(&self.graph.nodes[dst].1.label) {
            self.graph.nodes[dst].1.label.clone()
        } else {
            "".to_string()
        };
        let base_name = format!("m{}{}", src_name, dst_name);
        let mut name = base_name.clone();
        let mut count = 0;
        while self.names.get(&name).is_some() {
            name = format!("{}_{}", base_name, count);
            count += 1;
        }
        self.set_name(GraphId::Morphism(src, mph), name);
    }

    pub fn autoname_face(&mut self, fce: usize, parent: Option<usize>) {
        assert!(self.graph.faces[fce].label.name.is_empty());

        // If the name of the parent is not empty (ie we're the first children of the parent),
        // we copy the name
        if let Some(parent) = parent {
            if !self.graph.faces[parent].label.name.is_empty() {
                let parent_name = self.graph.faces[parent].label.name.clone();
                self.set_name(GraphId::Face(parent), "".to_string());
                self.set_name(GraphId::Face(fce), parent_name);
                return;
            }
        }

        let lbl = &self.graph.faces[fce].label.label;
        if is_valid_name(lbl) && self.names.get(lbl).is_none() {
            self.set_name(GraphId::Face(fce), lbl.clone());
            return;
        }

        // If it is only an atomic name it Goal{count}
        if let ActualEquality::Atomic(data) = self.graph.faces[fce].eq.deref() {
            if let ActualProofObject::Existential(_) = data.pobj.deref() {
                let mut name = "Goal0".to_string();
                let mut count = 0;
                while self.names.get(&name).is_some() {
                    count += 1;
                    name = format!("Goal{}", count);
                }
                self.set_name(GraphId::Face(fce), name);
                return;
            }
        }

        // Otherwise, use an arbitrary name
        let mut name = "p0".to_string();
        let mut count = 0;
        while self.names.get(&name).is_some() {
            count += 1;
            name = format!("p{}", count);
        }
        self.set_name(GraphId::Face(fce), name);
    }

    // To be called only once at the start.
    pub fn autoname(&mut self) {
        for n in 0..self.graph.nodes.len() {
            self.autoname_node(n);
        }
        for src in 0..self.graph.edges.len() {
            for mph in 0..self.graph.edges[src].len() {
                self.autoname_morphism(src, mph);
            }
        }
        for fce in 0..self.graph.faces.len() {
            self.autoname_face(fce, None);
        }
    }
}
