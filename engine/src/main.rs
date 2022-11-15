pub mod anyterm;
pub mod data;
pub mod dsl;
pub mod graph;
pub mod parser;
pub mod substitution;
// pub mod unification;

use data::ProofObject;
use data::{ActualCategory, ActualFunctor, ActualMorphism, ActualObject};
use data::{CategoryData, FunctorData, MorphismData, ObjectData};
use dsl::{cat, funct, mph, obj};

use std::vec::Vec;

fn serialize() {
    let ctx = data::Context::new();
    let cat = cat!(ctx, :0);
    let src_cat = cat!(ctx, :1);
    let a = obj!(ctx, (:2) in cat);
    let b = obj!(ctx, (:3) in cat);
    let c = obj!(ctx, (:4) in src_cat);
    let f = funct!(ctx, (:5) : src_cat => cat);
    let c = obj!(ctx, f _0 c);
    let m = mph!(ctx, ((?0) : a -> b) >> ((?1) : b -> c));
    let eq = ctx.mk(data::ActualEquality::Refl(m));
    assert!(eq.check(&ctx));
    let term = anyterm::AnyTerm::Eq(eq);
    let json = serde_json::to_string(&term).unwrap();
    println!("{}", json)
}

fn deserialize() {
    let str = r#"{"equality":{"refl":{"comp":[{"atomic":{"pobj":{"Existential":0},"category":{"atomic":{"pobj":{"Term":0}}},"src":{"atomic":{"pobj":{"Term":2},"category":{"atomic":{"pobj":{"Term":0}}}}},"dst":{"atomic":{"pobj":{"Term":3},"category":{"atomic":{"pobj":{"Term":0}}}}}}},{"atomic":{"pobj":{"Existential":1},"category":{"atomic":{"pobj":{"Term":0}}},"src":{"atomic":{"pobj":{"Term":3},"category":{"atomic":{"pobj":{"Term":0}}}}},"dst":{"funct":[{"atomic":{"pobj":{"Term":5},"src":{"atomic":{"pobj":{"Term":1}}},"dst":{"atomic":{"pobj":{"Term":0}}}}},{"atomic":{"pobj":{"Term":4},"category":{"atomic":{"pobj":{"Term":1}}}}}]}}}]}}}"#;
    let ctx = data::Context::new();
    let res = ctx.parse(str);
    match res {
        Ok(term) => println!("{:#?}", term),
        Err(err) => println!("Couldn't parse: {}", err),
    }
}

fn main() {
    let ctx = data::Context::new();
    let po: data::ProofObject = data::ProofObject::Term(1);
    let gr: graph::Graph = graph::Graph {
        nodes: Vec::new(),
        edges: Vec::new(),
        faces: Vec::new(),
    };
    println!("{:#?}", po);
    println!("{}", gr.check(&ctx));
    println!("##########");
    serialize();
    println!("##########");
    deserialize();
}
