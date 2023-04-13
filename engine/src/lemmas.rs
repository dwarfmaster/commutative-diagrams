use crate::data::{ActualCategory, Category};
use crate::data::{ActualEquality, Equality};
use crate::data::{ActualFunctor, Functor};
use crate::data::{ActualMorphism, Morphism};
use crate::data::{ActualObject, Object};
use crate::data::{ActualProofObject, ProofObject};
use crate::graph::Graph;
use std::ops::Deref;

pub struct Lemma<GR, NL, EL, FL> {
    pub name: String,
    pub pattern: Graph<NL, EL, FL>,
    pub existentials: Vec<u64>,
    pub graphical_state: GR,
}

fn extract_atomic_exs(exs: &mut Vec<u64>, po: &ProofObject) {
    use ActualProofObject::*;
    match po.deref() {
        Term(_) => (),
        Existential(ex) => exs.push(*ex),
        Cat(cat) => extract_cat_exs(exs, cat),
        Funct(f) => extract_funct_exs(exs, f),
        Obj(o) => extract_obj_exs(exs, o),
        Mph(m) => extract_mph_exs(exs, m),
        Eq(e) => extract_eq_exs(exs, e),
        Composed(_, _, pos) => pos.iter().for_each(|po| extract_atomic_exs(exs, po)),
    }
}

fn extract_cat_exs(exs: &mut Vec<u64>, cat: &Category) {
    use ActualCategory::*;
    match cat.deref() {
        Atomic(data) => extract_atomic_exs(exs, &data.pobj),
    }
}

fn extract_funct_exs(exs: &mut Vec<u64>, funct: &Functor) {
    use ActualFunctor::*;
    match funct.deref() {
        Atomic(data) => extract_atomic_exs(exs, &data.pobj),
    }
}

fn extract_obj_exs(exs: &mut Vec<u64>, obj: &Object) {
    use ActualObject::*;
    match obj.deref() {
        Atomic(data) => extract_atomic_exs(exs, &data.pobj),
        Funct(f, o) => {
            extract_funct_exs(exs, f);
            extract_obj_exs(exs, o);
        }
    }
}

fn extract_mph_exs(exs: &mut Vec<u64>, mph: &Morphism) {
    use ActualMorphism::*;
    match mph.deref() {
        Atomic(data) => extract_atomic_exs(exs, &data.pobj),
        Identity(o) => extract_obj_exs(exs, o),
        Comp(m1, m2) => {
            extract_mph_exs(exs, m1);
            extract_mph_exs(exs, m2);
        }
        Funct(f, m) => {
            extract_funct_exs(exs, f);
            extract_mph_exs(exs, m);
        }
    }
}

fn extract_eq_exs(exs: &mut Vec<u64>, eq: &Equality) {
    use ActualEquality::*;
    match eq.deref() {
        Atomic(data) => extract_atomic_exs(exs, &data.pobj),
        Refl(m) => extract_mph_exs(exs, m),
        Concat(p1, p2) => {
            extract_eq_exs(exs, p1);
            extract_eq_exs(exs, p2);
        }
        Inv(p) => extract_eq_exs(exs, p),
        Compose(p1, p2) => {
            extract_eq_exs(exs, p1);
            extract_eq_exs(exs, p2);
        }
        Assoc(m1, m2, m3) => {
            extract_mph_exs(exs, m1);
            extract_mph_exs(exs, m2);
            extract_mph_exs(exs, m3);
        }
        LeftId(m) => extract_mph_exs(exs, m),
        RightId(m) => extract_mph_exs(exs, m),
        RAp(p, m) => {
            extract_eq_exs(exs, p);
            extract_mph_exs(exs, m);
        }
        LAp(m, p) => {
            extract_mph_exs(exs, m);
            extract_eq_exs(exs, p);
        }
        FunctId(f, o) => {
            extract_funct_exs(exs, f);
            extract_obj_exs(exs, o);
        }
        FunctComp(f, m1, m2) => {
            extract_funct_exs(exs, f);
            extract_mph_exs(exs, m1);
            extract_mph_exs(exs, m2);
        }
        FunctCtx(f, p) => {
            extract_funct_exs(exs, f);
            extract_eq_exs(exs, p);
        }
    }
}

fn extract_existentials<NL, EL, FL>(gr: &Graph<NL, EL, FL>) -> Vec<u64> {
    let mut exs = Vec::new();
    gr.nodes
        .iter()
        .for_each(|nd| extract_obj_exs(&mut exs, &nd.0));
    gr.edges.iter().for_each(|edges| {
        edges
            .iter()
            .for_each(|edge| extract_mph_exs(&mut exs, &edge.2))
    });
    gr.faces
        .iter()
        .for_each(|face| extract_eq_exs(&mut exs, &face.eq));
    exs
}

impl<GR: Default, NL, EL, FL> Lemma<GR, NL, EL, FL> {
    pub fn new(name: String, pattern: Graph<NL, EL, FL>) -> Self {
        let exs = extract_existentials(&pattern);
        Self {
            name,
            pattern,
            existentials: exs,
            graphical_state: Default::default(),
        }
    }
}
