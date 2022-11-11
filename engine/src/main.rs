pub mod anyterm;
pub mod data;
pub mod graph;
pub mod substitution;
// pub mod unification;

use std::vec::Vec;

fn main() {
    let mut ctx = data::Context::new();
    let po: data::ProofObject = data::ProofObject::Term(1);
    let gr: graph::Graph = graph::Graph {
        nodes: Vec::new(),
        edges: Vec::new(),
        faces: Vec::new(),
    };
    println!("{:#?}", po);
    println!("{}", gr.check(&mut ctx));
}
