use super::super::vm::{Interactive, VM};
use crate::graph::eq::Eq;
use crate::realizer::realize_eq;
use crate::remote::Remote;

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    pub fn unify_eq(&mut self, cat: u64, eq1: &Eq, eq2: &Eq) -> bool {
        let (atomic, eq) = if let Some(e) = eq1.is_simple() {
            match e {
                Ok(a) => (a, eq2.clone()),
                Err(a) => {
                    let mut eq = eq2.clone();
                    eq.inv();
                    (a, eq)
                }
            }
        } else if let Some(e) = eq2.is_simple() {
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
            self.ctx
                .remote
                .unify(std::iter::once((atomic, eq)))
                .unwrap()
        } else {
            false
        }
    }
}
