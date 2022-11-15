pub mod anyterm;
pub mod data;
pub mod graph;
pub mod parser;
pub mod substitution;
pub mod unification;

use std::vec::Vec;

fn serialize() {
    let mut ctx = data::Context::new();
    ctx.new_term(0, "C");
    ctx.new_term(1, "I");
    ctx.new_term(2, "a");
    ctx.new_term(3, "b");
    ctx.new_term(4, "c");
    ctx.new_term(5, "F");
    let cat = ctx.mk(data::ActualCategory::Atomic(data::CategoryData {
        pobj: data::ProofObject::Term(0),
    }));
    let src_cat = ctx.mk(data::ActualCategory::Atomic(data::CategoryData {
        pobj: data::ProofObject::Term(1),
    }));
    let a = ctx.mk(data::ActualObject::Atomic(data::ObjectData {
        pobj: data::ProofObject::Term(2),
        category: cat.clone(),
    }));
    let b = ctx.mk(data::ActualObject::Atomic(data::ObjectData {
        pobj: data::ProofObject::Term(3),
        category: cat.clone(),
    }));
    let c = ctx.mk(data::ActualObject::Atomic(data::ObjectData {
        pobj: data::ProofObject::Term(4),
        category: src_cat.clone(),
    }));
    let f = ctx.mk(data::ActualFunctor::Atomic(data::FunctorData {
        pobj: data::ProofObject::Term(5),
        src: src_cat.clone(),
        dst: cat.clone(),
    }));
    let c = ctx.mk(data::ActualObject::Funct(f, c));
    let m1 = ctx.mk(data::ActualMorphism::Atomic(data::MorphismData {
        pobj: data::ProofObject::Existential(0),
        category: cat.clone(),
        src: a.clone(),
        dst: b.clone(),
    }));
    let m2 = ctx.mk(data::ActualMorphism::Atomic(data::MorphismData {
        pobj: data::ProofObject::Existential(1),
        category: cat.clone(),
        src: b.clone(),
        dst: c.clone(),
    }));
    let m = ctx.comp(m1.clone(), m2.clone());
    let eq = ctx.mk(data::ActualEquality::Refl(m));
    assert!(eq.check(&mut ctx));
    let term = anyterm::AnyTerm::Eq(eq);
    let json = serde_json::to_string(&term).unwrap();
    println!("{}", json)
}

fn deserialize() {
    let str = r#"{"equality":{"refl":{"comp":[{"atomic":{"pobj":{"Existential":0},"category":{"atomic":{"pobj":{"Term":0}}},"src":{"atomic":{"pobj":{"Term":2},"category":{"atomic":{"pobj":{"Term":0}}}}},"dst":{"atomic":{"pobj":{"Term":3},"category":{"atomic":{"pobj":{"Term":0}}}}}}},{"atomic":{"pobj":{"Existential":1},"category":{"atomic":{"pobj":{"Term":0}}},"src":{"atomic":{"pobj":{"Term":3},"category":{"atomic":{"pobj":{"Term":0}}}}},"dst":{"funct":[{"atomic":{"pobj":{"Term":5},"src":{"atomic":{"pobj":{"Term":1}}},"dst":{"atomic":{"pobj":{"Term":0}}}}},{"atomic":{"pobj":{"Term":4},"category":{"atomic":{"pobj":{"Term":1}}}}}]}}}]}}}"#;
    let ctx = data::Context::new();
    let (_, res) = ctx.parse(str);
    match res {
        Ok(term) => println!("{:#?}", term),
        Err(err) => println!("Couldn't parse: {}", err),
    }
}

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
    println!("##########");
    serialize();
    println!("##########");
    deserialize();
}
