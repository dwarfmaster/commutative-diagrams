use crate::graph::eq::{Eq, Morphism};
use crate::remote::TermEngine;

pub fn hook<R: TermEngine>(mph: &Morphism, _ctx: &mut R, eq: Eq, opts: &mut Vec<Eq>) {
    if mph.src == eq.inp.dst {
        let mut neq = eq.clone();
        neq.rap(mph);
        opts.push(neq);
    }
}
