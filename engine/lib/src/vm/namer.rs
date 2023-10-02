use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::graph::{FaceStatus, Graph};
use crate::vm::store::Context;
use crate::vm::vm::{Interactive, VM};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric1};
use nom::combinator::{eof, value};
use nom::multi::many0_count;
use nom::sequence::delimited;
use nom::IResult;
use std::collections::HashMap;

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

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    // Set name without any checks, assuming the previous name is empty
    pub fn set_name(&mut self, id: GraphId, name: String) {
        use GraphId::*;
        if !name.is_empty() {
            self.graph.names.insert(name.clone(), id);
        }
        match id {
            Node(n) => self.graph.graph.nodes[n].2.name = name,
            Morphism(src, mph) => self.graph.graph.edges[src][mph].1.name = name,
            Face(fce) => self.graph.graph.faces[fce].label.name = name,
        }
    }

    pub fn get_name(&self, id: GraphId) -> String {
        use GraphId::*;
        match id {
            Node(n) => self.graph.graph.nodes[n].2.name.clone(),
            Morphism(src, mph) => self.graph.graph.edges[src][mph].1.name.clone(),
            Face(f) => self.graph.graph.faces[f].label.name.clone(),
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
        if let Some(oid) = self.graph.names.get(name) {
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

    pub fn name_compute_node(
        ctx: &mut Context<Rm>,
        graph: &Graph,
        names: &HashMap<String, GraphId>,
        nd: usize,
    ) -> String {
        let lbl = &graph.nodes[nd].2.label;
        if is_valid_name(lbl) && names.get(lbl).is_none() {
            return lbl.clone();
        }

        // Try to name it from the category
        let cat_name = ctx.get_stored_label(graph.nodes[nd].1).to_lowercase();
        let base_name = if is_valid_name(&cat_name) {
            cat_name
        } else {
            "x".to_string()
        };
        let mut name = base_name.clone();
        let mut count = 0;
        while names.get(&name).is_some() {
            name = format!("{}_{}", base_name, count);
            count += 1;
        }
        return name;
    }

    pub fn autoname_node(&mut self, nd: usize) {
        assert!(self.graph.graph.nodes[nd].2.name.is_empty());
        let name = Self::name_compute_node(&mut self.ctx, &self.graph.graph, &self.graph.names, nd);
        self.set_name(GraphId::Node(nd), name.clone());
    }

    pub fn name_compute_morphism(
        _ctx: &mut Context<Rm>,
        graph: &Graph,
        names: &HashMap<String, GraphId>,
        src: usize,
        mph: usize,
    ) -> String {
        let lbl = &graph.edges[src][mph].1.label;
        if is_valid_name(lbl) && names.get(lbl).is_none() {
            return lbl.clone();
        }

        // Try to use src and dst to name it
        let src_name = if is_valid_name(&graph.nodes[src].2.label) {
            graph.nodes[src].2.label.clone()
        } else {
            "".to_string()
        };
        let dst = graph.edges[src][mph].0;
        let dst_name = if is_valid_name(&graph.nodes[dst].2.label) {
            graph.nodes[dst].2.label.clone()
        } else {
            "".to_string()
        };
        let base_name = format!("m{}{}", src_name, dst_name);
        let mut name = base_name.clone();
        let mut count = 0;
        while names.get(&name).is_some() {
            name = format!("{}_{}", base_name, count);
            count += 1;
        }
        return name;
    }

    pub fn autoname_morphism(&mut self, src: usize, mph: usize) {
        assert!(self.graph.graph.edges[src][mph].1.name.is_empty());
        let name = Self::name_compute_morphism(
            &mut self.ctx,
            &self.graph.graph,
            &self.graph.names,
            src,
            mph,
        );
        self.set_name(GraphId::Morphism(src, mph), name);
    }

    pub fn name_compute_face(
        _ctx: &mut Context<Rm>,
        graph: &Graph,
        names: &HashMap<String, GraphId>,
        base: &str,
        fce: usize,
    ) -> String {
        let lbl = &graph.faces[fce].label.label;
        if is_valid_name(lbl) && names.get(lbl).is_none() {
            return lbl.clone();
        }

        // We name goals Goal{count}
        if graph.faces[fce].label.status == FaceStatus::Goal {
            let mut name = format!("{}-0", base);
            let mut count = 0;
            while names.get(&name).is_some() {
                count += 1;
                name = format!("{}-{}", base, count);
            }
            return name;
        }

        // Otherwise, use an arbitrary name
        let mut name = "p0".to_string();
        let mut count = 0;
        while names.get(&name).is_some() {
            count += 1;
            name = format!("p{}", count);
        }
        return name;
    }

    pub fn autoname_face(&mut self, fce: usize) {
        assert!(self.graph.graph.faces[fce].label.name.is_empty());
        let mut base = "Goal";
        let graph = &mut self.graph;

        // If the name of the parent is not empty (ie we're the first children of the parent),
        // and the parent is hidden, we copy the name. If the parent isn't hidden, we use its name
        // as base.
        if let Some(parent) = graph.graph.faces[fce].label.parent {
            if !graph.graph.faces[parent].label.name.is_empty() {
                if graph.graph.faces[parent].label.hidden {
                    let parent_name = graph.graph.faces[parent].label.name.clone();
                    self.set_name(GraphId::Face(parent), "".to_string());
                    self.set_name(GraphId::Face(fce), parent_name);
                    return;
                } else {
                    base = graph.graph.faces[parent].label.name.as_str();
                }
            }
        }

        let name = Self::name_compute_face(&mut self.ctx, &graph.graph, &graph.names, base, fce);
        self.set_name(GraphId::Face(fce), name);
    }

    // To be called only once at the start.
    pub fn autoname(&mut self) {
        for n in 0..self.graph.graph.nodes.len() {
            self.autoname_node(n);
        }
        for src in 0..self.graph.graph.edges.len() {
            for mph in 0..self.graph.graph.edges[src].len() {
                self.autoname_morphism(src, mph);
            }
        }
        for fce in 0..self.graph.graph.faces.len() {
            self.autoname_face(fce);
        }
    }
}
