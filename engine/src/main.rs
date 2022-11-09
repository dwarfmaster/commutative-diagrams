pub mod data;
pub mod graph;

use std::vec::Vec;

fn main() {
    let po: data::ProofObject = data::ProofObject {
        id: 1,
        printed: String::from("Coucou"),
    };
    let gr: graph::Graph = graph::Graph {
        nodes: Vec::new(),
        edges: Vec::new(),
        faces: Vec::new(),
    };
    println!("{:#?}", po);
    println!("{}", gr.check());
}
