use super::super::vm::{Interactive, VM};
use crate::data::EvarStatus;
use crate::graph::eq::Eq;
use crate::normalizer::{normalize_eq, normalize_morphism};
use crate::realizer::realize_eq;
use crate::remote::Remote;
use crate::vm::Context;

// Check if the equality is simple, and furthermore checks if the internal object
// is an evar. Otherwise returns None. Returns Ok if the evar is directly used and
// Err if it is inversed.
fn eq_is_evar<Rm: Remote>(ctx: &mut Context<Rm>, eq: &Eq) -> Option<Result<u64, u64>> {
    if let Some(e) = eq.is_simple() {
        match e {
            Ok(a) => {
                if ctx.get_stored_status(a) == EvarStatus::Evar {
                    return Some(Ok(a));
                }
            }
            Err(a) => {
                if ctx.get_stored_status(a) == EvarStatus::Evar {
                    return Some(Err(a));
                }
            }
        }
    }
    None
}

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    pub fn unify_eq(&mut self, cat: u64, eq1: &Eq, eq2: &Eq) -> bool {
        self.unify_eq_unsafe(cat, eq1, eq2, true)
    }

    pub fn unify_eq_unsafe(&mut self, cat: u64, eq1: &Eq, eq2: &Eq, safe: bool) -> bool {
        let (atomic, eq) = if let Some(e) = eq_is_evar(&mut self.ctx, eq1) {
            match e {
                Ok(a) => (a, eq2.clone()),
                Err(a) => {
                    let mut eq = eq2.clone();
                    eq.inv();
                    (a, eq)
                }
            }
        } else if let Some(e) = eq_is_evar(&mut self.ctx, eq2) {
            match e {
                Ok(a) => (a, eq1.clone()),
                Err(a) => {
                    let mut eq = eq1.clone();
                    eq.inv();
                    (a, eq)
                }
            }
        } else {
            return false;
        };

        if let Some((_, _, left, right)) = self.ctx.is_eq(atomic, cat) {
            let eq = realize_eq(&mut self.ctx, left, right, &eq);
            self.change_state();
            let r = self
                .ctx
                .remote
                .unify(std::iter::once((atomic, eq)))
                .unwrap();
            if r && safe {
                // Make sure morphisms are atomic after unification
                self.ensure_morphisms_invariant()
            }
            r
        } else {
            false
        }
    }

    pub fn ensure_morphisms_invariant(&mut self) {
        for src in 0..self.graph.graph.nodes.len() {
            let cat = self.graph.graph.nodes[src].1;
            for mph in 0..self.graph.graph.edges[src].len() {
                normalize_morphism(&mut self.ctx, cat, &mut self.graph.graph.edges[src][mph].3);
            }
        }
        for fce in 0..self.graph.graph.faces.len() {
            normalize_eq(&mut self.ctx, &mut self.graph.graph.faces[fce].eq);
        }
    }
}
