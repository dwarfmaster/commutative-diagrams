use crate::data::{ActualEquality, ActualMorphism, Context, Equality};
use std::ops::Deref;

impl ActualEquality {
    fn simpl_inv(&self, ctx: &Context, inv: bool) -> Equality {
        use ActualEquality::*;
        match self {
            Refl(m) => ctx.mk(Refl(m.clone())),
            Concat(eq1, eq2) => {
                let eq1 = eq1.simpl_inv(ctx, inv);
                let eq2 = eq2.simpl_inv(ctx, inv);
                match (eq1.deref(), eq2.deref()) {
                    (Refl(m), Refl(_)) => ctx.mk(Refl(m.clone())),
                    (Refl(_), _) => eq2,
                    (_, Refl(_)) => eq1,
                    _ => {
                        if inv {
                            ctx.mk(Concat(eq2, eq1))
                        } else {
                            ctx.mk(Concat(eq1, eq2))
                        }
                    }
                }
            }
            Inv(eq) => eq.simpl_inv(ctx, !inv),
            Compose(eq1, eq2) => {
                let eq1 = eq1.simpl_inv(ctx, inv);
                let eq2 = eq2.simpl_inv(ctx, inv);
                match (eq1.deref(), eq2.deref()) {
                    (Refl(m1), Refl(m2)) => {
                        ctx.mk(Refl(ctx.mk(ActualMorphism::Comp(m1.clone(), m2.clone()))))
                    }
                    (Refl(m), _) => ctx.mk(LAp(m.clone(), eq2)),
                    (_, Refl(m)) => ctx.mk(RAp(eq1, m.clone())),
                    _ => ctx.mk(Compose(eq1, eq2)),
                }
            }
            RAp(eq, m) => {
                let eq = eq.simpl_inv(ctx, inv);
                match eq.deref() {
                    Refl(m1) => ctx.mk(Refl(ctx.mk(ActualMorphism::Comp(m1.clone(), m.clone())))),
                    _ => ctx.mk(RAp(eq, m.clone())),
                }
            }
            LAp(m, eq) => {
                let eq = eq.simpl_inv(ctx, inv);
                match eq.deref() {
                    Refl(m2) => ctx.mk(Refl(ctx.mk(ActualMorphism::Comp(m.clone(), m2.clone())))),
                    _ => ctx.mk(LAp(m.clone(), eq)),
                }
            }
            FunctCtx(f, eq) => {
                let eq = eq.simpl_inv(ctx, inv);
                match eq.deref() {
                    Refl(m) => ctx.mk(Refl(ctx.mk(ActualMorphism::Funct(f.clone(), m.clone())))),
                    _ => ctx.mk(FunctCtx(f.clone(), eq)),
                }
            }
            _ => {
                if inv {
                    ctx.mk(Inv(ctx.mk(self.clone())))
                } else {
                    ctx.mk(self.clone())
                }
            }
        }
    }

    pub fn simpl(&self, ctx: &Context) -> Equality {
        self.simpl_inv(ctx, false)
    }
}
