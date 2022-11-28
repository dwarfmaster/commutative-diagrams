pub mod anyterm;
pub mod data;
pub mod dsl;
pub mod graph;
pub mod parser;
pub mod substitution;
pub mod unification;

use data::ProofObject;
use data::{ActualCategory, ActualFunctor, ActualMorphism, ActualObject};
use data::{CategoryData, FunctorData, MorphismData, ObjectData};
use dsl::{cat, funct, mph, obj};

use std::vec::Vec;

fn main() {
    let mut ctx = data::Context::new();
    let cat = cat!(ctx, :0);
    let src_cat = cat!(ctx, :1);
    let a = obj!(ctx, (:2) in cat);
    let b = obj!(ctx, (:3) in cat);
    let c = obj!(ctx, (:4) in src_cat);
    let f = funct!(ctx, (:5) : src_cat => cat);
    let c = obj!(ctx, f _0 c);
    let m1 = mph!(ctx, (?0) : a -> b);
    let m2 = mph!(ctx, (?1) : b -> c);

    let gr: graph::Graph = graph::Graph {
        nodes: vec![a, b, c],
        edges: vec![vec![(1, m1)], vec![(2, m2)], Vec::new()],
        faces: Vec::new(),
    };

    graph::viz(&gr, &mut ctx);
}
