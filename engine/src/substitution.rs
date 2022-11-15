use crate::anyterm::AnyTerm;
use crate::data::ProofObject::{Existential, Term};
use crate::data::{ActualCategory, ActualEquality, ActualFunctor, ActualMorphism, ActualObject};
use crate::data::{Category, Context, Equality, Functor, Morphism, Object};
use crate::data::{EqualityData, FunctorData, MorphismData, ObjectData};
use core::ops::Deref;

/// A substitution for a graph, with replacement for existential of all types
/// For performance reasons a substitution is a sequence of substitution, with each following one
/// also applying on the previous one
pub type Substitution = Vec<(u64, AnyTerm)>;

pub fn check_subst(sigma: &Substitution, ctx: &mut Context) -> bool {
    sigma.iter().all(|(_, term)| term.check(ctx))
}

fn find_applicable(sigma: &[(u64, AnyTerm)], e: u64) -> Option<(usize, AnyTerm)> {
    sigma
        .iter()
        .enumerate()
        .find(|(_, (ematch, _))| e == *ematch)
        .map(|(i, (_, v))| (i, v.clone()))
}

pub trait Substitutable {
    fn subst_slice(self, ctx: &mut Context, sigma: &[(u64, AnyTerm)]) -> Self
    where
        Self: Sized;
    fn subst(self, ctx: &mut Context, sigma: &Substitution) -> Self
    where
        Self: Sized,
    {
        self.subst_slice(ctx, sigma)
    }
}

impl Substitutable for AnyTerm {
    fn subst_slice(self, ctx: &mut Context, sigma: &[(u64, AnyTerm)]) -> Self {
        use AnyTerm::*;
        match self {
            Cat(cat) => Cat(cat.subst_slice(ctx, sigma)),
            Funct(f) => Funct(f.subst_slice(ctx, sigma)),
            Obj(o) => Obj(o.subst_slice(ctx, sigma)),
            Mph(m) => Mph(m.subst_slice(ctx, sigma)),
            Eq(eq) => Eq(eq.subst_slice(ctx, sigma)),
        }
    }
}

impl Substitutable for Category {
    fn subst_slice(self, ctx: &mut Context, sigma: &[(u64, AnyTerm)]) -> Category {
        use ActualCategory::*;
        match self.deref() {
            Atomic(data) => match data.pobj {
                Term(_) => self,
                Existential(e) => find_applicable(sigma, e)
                    .map(|(i, v)| v.subst_slice(ctx, &sigma[i + 1..]).expect_cat(ctx))
                    .unwrap_or(self),
            },
        }
    }
}

impl Substitutable for Functor {
    fn subst_slice(self, ctx: &mut Context, sigma: &[(u64, AnyTerm)]) -> Self {
        use ActualFunctor::*;
        match self.deref() {
            Atomic(data) => {
                let src = data.src.clone().subst_slice(ctx, sigma);
                let dst = data.dst.clone().subst_slice(ctx, sigma);
                match data.pobj {
                    Term(e) => ctx.mk(Atomic(FunctorData {
                        pobj: Term(e),
                        src,
                        dst,
                    })),
                    Existential(e) => find_applicable(sigma, e)
                        .map(|(i, v)| {
                            v.subst_slice(ctx, &sigma[i + 1..])
                                .expect_funct(ctx, src, dst)
                        })
                        .unwrap_or(self),
                }
            }
        }
    }
}

impl Substitutable for Object {
    fn subst_slice(self, ctx: &mut Context, sigma: &[(u64, AnyTerm)]) -> Self {
        use ActualObject::*;
        match self.deref() {
            Atomic(data) => {
                let category = data.category.clone().subst_slice(ctx, sigma);
                match data.pobj {
                    Term(e) => ctx.mk(Atomic(ObjectData {
                        pobj: Term(e),
                        category,
                    })),
                    Existential(e) => find_applicable(sigma, e)
                        .map(|(i, v)| {
                            v.subst_slice(ctx, &sigma[i + 1..])
                                .expect_obj(ctx, category)
                        })
                        .unwrap_or(self),
                }
            }
            Funct(f, o) => {
                let f = f.clone().subst_slice(ctx, sigma);
                let o = o.clone().subst_slice(ctx, sigma);
                ctx.mk(Funct(f, o))
            }
        }
    }
}

impl Substitutable for Morphism {
    fn subst_slice(self, ctx: &mut Context, sigma: &[(u64, AnyTerm)]) -> Self {
        use ActualMorphism::*;
        match self.deref() {
            Atomic(data) => {
                let category = data.category.clone().subst_slice(ctx, sigma);
                let src = data.src.clone().subst_slice(ctx, sigma);
                let dst = data.dst.clone().subst_slice(ctx, sigma);
                match data.pobj {
                    Term(e) => ctx.mk(Atomic(MorphismData {
                        pobj: Term(e),
                        category,
                        src,
                        dst,
                    })),
                    Existential(e) => find_applicable(sigma, e)
                        .map(|(i, v)| {
                            v.subst_slice(ctx, &sigma[i + 1..])
                                .expect_mph(ctx, category, src, dst)
                        })
                        .unwrap_or(self),
                }
            }
            Identity(o) => {
                let o = o.clone().subst_slice(ctx, sigma);
                ctx.mk(Identity(o))
            }
            Comp(m1, m2) => {
                let m1 = m1.clone().subst_slice(ctx, sigma);
                let m2 = m2.clone().subst_slice(ctx, sigma);
                ctx.mk(Comp(m1, m2))
            }
            Funct(f, m) => {
                let f = f.clone().subst_slice(ctx, sigma);
                let m = m.clone().subst_slice(ctx, sigma);
                ctx.mk(Funct(f, m))
            }
        }
    }
}

impl Substitutable for Equality {
    fn subst_slice(self, ctx: &mut Context, sigma: &[(u64, AnyTerm)]) -> Self {
        use ActualEquality::*;
        match self.deref() {
            Atomic(data) => {
                let category = data.category.clone().subst_slice(ctx, sigma);
                let src = data.src.clone().subst_slice(ctx, sigma);
                let dst = data.dst.clone().subst_slice(ctx, sigma);
                let left = data.left.clone().subst_slice(ctx, sigma);
                let right = data.right.clone().subst_slice(ctx, sigma);
                match data.pobj {
                    Term(e) => ctx.mk(Atomic(EqualityData {
                        pobj: Term(e),
                        category,
                        src,
                        dst,
                        left,
                        right,
                    })),
                    Existential(e) => find_applicable(sigma, e)
                        .map(|(i, v)| {
                            v.subst_slice(ctx, &sigma[i + 1..])
                                .expect_eq(ctx, category, src, dst, left, right)
                        })
                        .unwrap_or(self),
                }
            }
            Refl(m) => {
                let m = m.clone().subst_slice(ctx, sigma);
                ctx.mk(Refl(m))
            }
            Concat(eq1, eq2) => {
                let eq1 = eq1.clone().subst_slice(ctx, sigma);
                let eq2 = eq2.clone().subst_slice(ctx, sigma);
                ctx.mk(Concat(eq1, eq2))
            }
            Inv(eq) => {
                let eq = eq.clone().subst_slice(ctx, sigma);
                ctx.mk(Inv(eq))
            }
            Compose(eq1, eq2) => {
                let eq1 = eq1.clone().subst_slice(ctx, sigma);
                let eq2 = eq2.clone().subst_slice(ctx, sigma);
                ctx.mk(Compose(eq1, eq2))
            }
            Assoc(m1, m2, m3) => {
                let m1 = m1.clone().subst_slice(ctx, sigma);
                let m2 = m2.clone().subst_slice(ctx, sigma);
                let m3 = m3.clone().subst_slice(ctx, sigma);
                ctx.mk(Assoc(m1, m2, m3))
            }
            LeftId(m) => {
                let m = m.clone().subst_slice(ctx, sigma);
                ctx.mk(LeftId(m))
            }
            RightId(m) => {
                let m = m.clone().subst_slice(ctx, sigma);
                ctx.mk(RightId(m))
            }
            RAp(eq, m) => {
                let eq = eq.clone().subst_slice(ctx, sigma);
                let m = m.clone().subst_slice(ctx, sigma);
                ctx.mk(RAp(eq, m))
            }
            LAp(m, eq) => {
                let m = m.clone().subst_slice(ctx, sigma);
                let eq = eq.clone().subst_slice(ctx, sigma);
                ctx.mk(LAp(m, eq))
            }
            FunctId(f, o) => {
                let f = f.clone().subst_slice(ctx, sigma);
                let o = o.clone().subst_slice(ctx, sigma);
                ctx.mk(FunctId(f, o))
            }
            FunctComp(f, m1, m2) => {
                let f = f.clone().subst_slice(ctx, sigma);
                let m1 = m1.clone().subst_slice(ctx, sigma);
                let m2 = m2.clone().subst_slice(ctx, sigma);
                ctx.mk(FunctComp(f, m1, m2))
            }
            FunctCtx(f, eq) => {
                let f = f.clone().subst_slice(ctx, sigma);
                let eq = eq.clone().subst_slice(ctx, sigma);
                ctx.mk(FunctCtx(f, eq))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::anyterm::AnyTerm;
    use crate::data::{ActualCategory, ActualMorphism, ActualObject};
    use crate::data::{CategoryData, MorphismData, ObjectData};
    use crate::data::{Context, ProofObject};
    use crate::dsl::{cat, mph, obj};
    use crate::substitution::Substitutable;

    #[test]
    pub fn simple_subst() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let a = obj!(ctx, (:1) in cat);
        let b = obj!(ctx, (:2) in cat);
        let c = obj!(ctx, (:3) in cat);
        let c_ex = obj!(ctx, (?0) in cat);
        let d = obj!(ctx, (:4) in cat);
        let m1 = mph!(ctx, (:5) : a -> b);
        let m2 = mph!(ctx, (:6) : b -> c);
        let m2_ex = mph!(ctx, (?1) : b -> c_ex);
        let m3 = mph!(ctx, (:7) : c -> d);
        let m3_ex = mph!(ctx, (:7) : c_ex -> d);
        let m = mph!(ctx, m1 >> m2 >> m3);
        let m_ex = mph!(ctx, m1 >> m2_ex >> m3_ex);

        let subst = vec![(0, AnyTerm::Obj(c.clone())), (1, AnyTerm::Mph(m2.clone()))];
        assert_eq!(
            m_ex.clone().subst(&mut ctx, &subst),
            m.clone(),
            "Substitution failed"
        );

        let subst2 = vec![(1, AnyTerm::Mph(m2.clone())), (0, AnyTerm::Obj(c.clone()))];
        assert_eq!(
            m_ex.clone().subst(&mut ctx, &subst2),
            m.clone(),
            "Substitution 2 failed"
        );
    }
}
