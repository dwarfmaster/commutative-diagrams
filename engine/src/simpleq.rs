use crate::data::{ActualEquality, ActualMorphism, Context, Equality};
use std::ops::Deref;

// The workflow is wrong here
// TODO: RAp(RAp(eq, m1), m2) --> RAp(eq, (m1 o m2))
// TODO: Same with LAp
// TODO: simplify equality followed by inv of itself
struct EqIterator<'a> {
    state: Vec<Equality>,
    ctx: &'a Context,
    inv: bool,
}

impl<'a> EqIterator<'a> {
    pub fn new(eq: Equality, inv: bool, ctx: &'a Context) -> Self {
        Self {
            state: vec![eq],
            inv,
            ctx,
        }
    }
}

impl<'a> Iterator for EqIterator<'a> {
    type Item = Equality;

    fn next(&mut self) -> Option<Self::Item> {
        let mut nxt = self.state.pop()?;
        while let ActualEquality::Concat(p1, p2) = nxt.deref() {
            self.state.push(p2.clone());
            nxt = self.ctx.simpl_eq_inv_to_concat(self.inv, p1.clone());
        }
        Some(nxt)
    }
}

impl Context {
    fn simpl_eq_inv_to_concat(&self, inv: bool, eq: Equality) -> Equality {
        use ActualEquality::*;
        match eq.deref() {
            Concat(m1, m2) => {
                if inv {
                    self.mk(Concat(m2.clone(), m1.clone()))
                } else {
                    eq
                }
            }
            Refl(_) => eq,
            Inv(eq) => self.simpl_eq_inv(!inv, eq.clone()),
            Compose(eq1, eq2) => {
                let eq1 = self.simpl_eq_inv(!inv, eq1.clone());
                let eq2 = self.simpl_eq_inv(!inv, eq2.clone());
                match (eq1.deref(), eq2.deref()) {
                    (Refl(m), _) => self.simpl_eq_inv(inv, self.mk(LAp(m.clone(), eq2))),
                    (_, Refl(m)) => self.simpl_eq_inv(inv, self.mk(RAp(eq1, m.clone()))),
                    _ => self.mk(Compose(eq1, eq2)),
                }
            }
            RAp(eq, m) => {
                if let ActualMorphism::Identity(_) = m.deref() {
                    let idl = self.mk(RightId(eq.left(self)));
                    let idr = self.mk(Inv(self.mk(RightId(eq.right(self)))));
                    return self.mk(Concat(idl, self.mk(Concat(eq.clone(), idr))));
                }
                let eq = self.simpl_eq_inv(inv, eq.clone());
                match eq.deref() {
                    Refl(m1) => self.mk(Refl(self.mk(ActualMorphism::Comp(m1.clone(), m.clone())))),
                    Concat(eq1, eq2) => self.mk(Concat(
                        self.mk(RAp(eq1.clone(), m.clone())),
                        self.mk(RAp(eq2.clone(), m.clone())),
                    )),
                    _ => self.mk(RAp(eq, m.clone())),
                }
            }
            LAp(m, eq) => {
                if let ActualMorphism::Identity(_) = m.deref() {
                    let idl = self.mk(LeftId(eq.left(self)));
                    let idr = self.mk(Inv(self.mk(LeftId(eq.right(self)))));
                    return self.mk(Concat(idl, self.mk(Concat(eq.clone(), idr))));
                }
                let eq = self.simpl_eq_inv(inv, eq.clone());
                match eq.deref() {
                    Refl(m2) => self.mk(Refl(self.mk(ActualMorphism::Comp(m.clone(), m2.clone())))),
                    Concat(eq1, eq2) => self.mk(Concat(
                        self.mk(LAp(m.clone(), eq1.clone())),
                        self.mk(LAp(m.clone(), eq2.clone())),
                    )),
                    _ => self.mk(LAp(m.clone(), eq)),
                }
            }
            FunctCtx(f, eq) => {
                let eq = self.simpl_eq_inv(inv, eq.clone());
                match eq.deref() {
                    Refl(m) => self.mk(Refl(self.mk(ActualMorphism::Funct(f.clone(), m.clone())))),
                    _ => self.mk(FunctCtx(f.clone(), eq)),
                }
            }
            _ => {
                if inv {
                    self.mk(Inv(eq))
                } else {
                    eq
                }
            }
        }
    }

    fn simpl_eq_inv(&self, inv: bool, eq: Equality) -> Equality {
        let m = eq.left(self);
        EqIterator::new(eq, inv, self)
            .filter(|eq| {
                if let ActualEquality::Refl(_) = eq.deref() {
                    false
                } else {
                    true
                }
            })
            .reduce(|eq1, eq2| self.mk(ActualEquality::Concat(eq1, eq2)))
            .unwrap_or(self.mk(ActualEquality::Refl(m)))
    }

    pub fn simpl_eq(&self, eq: Equality) -> Equality {
        self.simpl_eq_inv(false, eq)
    }
}
