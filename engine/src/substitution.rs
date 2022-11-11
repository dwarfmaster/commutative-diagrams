use crate::anyterm::AnyTerm;
use crate::data::ProofObject::{Existential, Term};
use crate::data::{ActualCategory, ActualEquality, ActualFunctor, ActualMorphism, ActualObject};
use crate::data::{Category, Context, Equality, Functor, Morphism, Object};
use core::ops::Deref;

/// A substitution for a graph, with replacement for existential of all types
/// For performance reasons a substitution is a sequence of substitution, with each following one
/// also applying on the previous one
pub type Substitution = Vec<(u64, AnyTerm)>;

pub fn check_subst(sigma: &Substitution, ctx: &mut Context) -> bool {
    sigma.iter().all(|(_, term)| term.check(ctx))
}

fn find_applicable(sigma: &Substitution, start: usize, e: u64) -> Option<(usize, AnyTerm)> {
    sigma
        .iter()
        .enumerate()
        .skip(start)
        .find(|(_, (ematch, _))| e == *ematch)
        .map(|(i, (_, v))| (i, v.clone()))
}

pub trait Substitutable {
    fn subst_from(self, ctx: &mut Context, sigma: &Substitution, start: usize) -> Self
    where
        Self: Sized;
    fn subst(self, ctx: &mut Context, sigma: &Substitution) -> Self
    where
        Self: Sized,
    {
        self.subst_from(ctx, sigma, 0)
    }
}

impl Substitutable for AnyTerm {
    fn subst_from(self, ctx: &mut Context, sigma: &Substitution, start: usize) -> Self {
        use AnyTerm::*;
        match self {
            Cat(cat) => Cat(cat.subst_from(ctx, sigma, start)),
            Funct(f) => Funct(f.subst_from(ctx, sigma, start)),
            Obj(o) => Obj(o.subst_from(ctx, sigma, start)),
            Mph(m) => Mph(m.subst_from(ctx, sigma, start)),
            Eq(eq) => Eq(eq.subst_from(ctx, sigma, start)),
        }
    }
}

impl Substitutable for Category {
    fn subst_from(self, ctx: &mut Context, sigma: &Substitution, start: usize) -> Category {
        use ActualCategory::*;
        match self.deref() {
            Atomic(data) => match data.pobj {
                Term(_) => self,
                Existential(e) => find_applicable(sigma, start, e)
                    .map(|(i, v)| v.expect_cat(ctx).subst_from(ctx, sigma, i + 1))
                    .unwrap_or(self),
            },
        }
    }
}

impl Substitutable for Functor {
    fn subst_from(self, ctx: &mut Context, sigma: &Substitution, start: usize) -> Self {
        use ActualFunctor::*;
        match self.deref() {
            Atomic(data) => match data.pobj {
                Term(_) => self,
                Existential(e) => find_applicable(sigma, start, e)
                    .map(|(i, v)| {
                        v.expect_funct(ctx, data.src.clone(), data.dst.clone())
                            .subst_from(ctx, sigma, i + 1)
                    })
                    .unwrap_or(self),
            },
        }
    }
}

impl Substitutable for Object {
    fn subst_from(self, ctx: &mut Context, sigma: &Substitution, start: usize) -> Self {
        use ActualObject::*;
        match self.deref() {
            Atomic(data) => match data.pobj {
                Term(_) => self,
                Existential(e) => find_applicable(sigma, start, e)
                    .map(|(i, v)| {
                        v.expect_obj(ctx, data.category.clone())
                            .subst_from(ctx, sigma, i + 1)
                    })
                    .unwrap_or(self),
            },
            Funct(f, o) => {
                let f = f.clone().subst_from(ctx, sigma, start);
                let o = o.clone().subst_from(ctx, sigma, start);
                ctx.mk(Funct(f, o))
            }
        }
    }
}

impl Substitutable for Morphism {
    fn subst_from(self, ctx: &mut Context, sigma: &Substitution, start: usize) -> Self {
        use ActualMorphism::*;
        match self.deref() {
            Atomic(data) => match data.pobj {
                Term(_) => self,
                Existential(e) => find_applicable(sigma, start, e)
                    .map(|(i, v)| {
                        v.expect_mph(
                            ctx,
                            data.category.clone(),
                            data.src.clone(),
                            data.dst.clone(),
                        )
                        .subst_from(ctx, sigma, i + 1)
                    })
                    .unwrap_or(self),
            },
            Identity(o) => {
                let o = o.clone().subst_from(ctx, sigma, start);
                ctx.mk(Identity(o))
            }
            Comp(m1, m2) => {
                let m1 = m1.clone().subst_from(ctx, sigma, start);
                let m2 = m2.clone().subst_from(ctx, sigma, start);
                ctx.mk(Comp(m1, m2))
            }
            Funct(f, m) => {
                let f = f.clone().subst_from(ctx, sigma, start);
                let m = m.clone().subst_from(ctx, sigma, start);
                ctx.mk(Funct(f, m))
            }
        }
    }
}

impl Substitutable for Equality {
    fn subst_from(self, ctx: &mut Context, sigma: &Substitution, start: usize) -> Self {
        use ActualEquality::*;
        match self.deref() {
            Atomic(data) => match data.pobj {
                Term(_) => self,
                Existential(e) => find_applicable(sigma, start, e)
                    .map(|(i, v)| {
                        v.expect_eq(
                            ctx,
                            data.category.clone(),
                            data.src.clone(),
                            data.dst.clone(),
                            data.left.clone(),
                            data.right.clone(),
                        )
                        .subst_from(ctx, sigma, i + 1)
                    })
                    .unwrap_or(self),
            },
            Refl(m) => {
                let m = m.clone().subst_from(ctx, sigma, start);
                ctx.mk(Refl(m))
            }
            Concat(eq1, eq2) => {
                let eq1 = eq1.clone().subst_from(ctx, sigma, start);
                let eq2 = eq2.clone().subst_from(ctx, sigma, start);
                ctx.mk(Concat(eq1, eq2))
            }
            Inv(eq) => {
                let eq = eq.clone().subst_from(ctx, sigma, start);
                ctx.mk(Inv(eq))
            }
            Compose(eq1, eq2) => {
                let eq1 = eq1.clone().subst_from(ctx, sigma, start);
                let eq2 = eq2.clone().subst_from(ctx, sigma, start);
                ctx.mk(Compose(eq1, eq2))
            }
            Assoc(m1, m2, m3) => {
                let m1 = m1.clone().subst_from(ctx, sigma, start);
                let m2 = m2.clone().subst_from(ctx, sigma, start);
                let m3 = m3.clone().subst_from(ctx, sigma, start);
                ctx.mk(Assoc(m1, m2, m3))
            }
            LeftId(m) => {
                let m = m.clone().subst_from(ctx, sigma, start);
                ctx.mk(LeftId(m))
            }
            RightId(m) => {
                let m = m.clone().subst_from(ctx, sigma, start);
                ctx.mk(RightId(m))
            }
            RAp(eq, m) => {
                let eq = eq.clone().subst_from(ctx, sigma, start);
                let m = m.clone().subst_from(ctx, sigma, start);
                ctx.mk(RAp(eq, m))
            }
            LAp(m, eq) => {
                let m = m.clone().subst_from(ctx, sigma, start);
                let eq = eq.clone().subst_from(ctx, sigma, start);
                ctx.mk(LAp(m, eq))
            }
            FunctId(f, o) => {
                let f = f.clone().subst_from(ctx, sigma, start);
                let o = o.clone().subst_from(ctx, sigma, start);
                ctx.mk(FunctId(f, o))
            }
            FunctComp(f, m1, m2) => {
                let f = f.clone().subst_from(ctx, sigma, start);
                let m1 = m1.clone().subst_from(ctx, sigma, start);
                let m2 = m2.clone().subst_from(ctx, sigma, start);
                ctx.mk(FunctComp(f, m1, m2))
            }
            FunctCtx(f, eq) => {
                let f = f.clone().subst_from(ctx, sigma, start);
                let eq = eq.clone().subst_from(ctx, sigma, start);
                ctx.mk(FunctCtx(f, eq))
            }
        }
    }
}
