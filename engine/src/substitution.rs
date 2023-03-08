use crate::anyterm::AnyTerm;
use crate::data::{ActualCategory, ActualEquality, ActualFunctor, ActualMorphism, ActualObject};
use crate::data::{ActualProofObject, Context, ProofObject};
use crate::data::{Category, Equality, Functor, Morphism, Object};
use crate::data::{CategoryData, EqualityData, FunctorData, MorphismData, ObjectData};
use core::ops::Deref;

/// A substitution for a graph, with replacement for existential of all types
/// For performance reasons a substitution is a sequence of substitution, with each following one
/// also applying on the previous one
pub type Substitution = Vec<(u64, AnyTerm)>;

pub fn check_subst(sigma: &Substitution, ctx: &Context) -> bool {
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
    fn subst_slice(self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Self
    where
        Self: Sized;
    fn subst(self, ctx: &Context, sigma: &Substitution) -> Self
    where
        Self: Sized,
    {
        self.subst_slice(ctx, sigma)
    }
}

pub trait SubstitutableInPlace {
    fn subst_slice_in_place(&mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]);
    fn subst_in_place(&mut self, ctx: &Context, sigma: &Substitution) {
        self.subst_slice_in_place(ctx, sigma)
    }
}

impl Substitutable for AnyTerm {
    fn subst_slice(self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Self {
        use AnyTerm::*;
        match self {
            Cat(cat) => Cat(cat.subst_slice(ctx, sigma)),
            Funct(f) => Funct(f.subst_slice(ctx, sigma)),
            Obj(o) => Obj(o.subst_slice(ctx, sigma)),
            Mph(m) => Mph(m.subst_slice(ctx, sigma)),
            Eq(eq) => Eq(eq.subst_slice(ctx, sigma)),
            Pobj(obj) => Pobj(obj.subst_slice(ctx, sigma)),
            TypedCat(cat) => TypedCat(cat.subst_slice(ctx, sigma)),
            TypedFunct(f) => TypedFunct(f.subst_slice(ctx, sigma)),
            TypedObj(o) => TypedObj(o.subst_slice(ctx, sigma)),
            TypedMph(m) => TypedMph(m.subst_slice(ctx, sigma)),
            TypedEq(eq) => TypedEq(eq.subst_slice(ctx, sigma)),
        }
    }
}

impl SubstitutableInPlace for AnyTerm {
    fn subst_slice_in_place(&mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]) {
        // Value used to prevent uninitialized reference, will be discarded by the end
        let def = AnyTerm::Cat(ctx.def_cat());
        let new = std::mem::replace(self, def).subst_slice(ctx, sigma);
        *self = new;
    }
}

impl Substitutable for ProofObject {
    fn subst_slice(self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Self {
        use ActualProofObject::*;
        match self.deref() {
            Term(_) => self,
            Existential(e) => find_applicable(sigma, *e)
                .map(|(i, v)| v.subst_slice(ctx, &sigma[i + 1..]).to_proof_object(ctx))
                .unwrap_or(self),
            Cat(c) => ctx.mk(Cat(c.clone().subst_slice(ctx, sigma))),
            Funct(f) => ctx.mk(Funct(f.clone().subst_slice(ctx, sigma))),
            Obj(o) => ctx.mk(Obj(o.clone().subst_slice(ctx, sigma))),
            Mph(m) => ctx.mk(Mph(m.clone().subst_slice(ctx, sigma))),
            Eq(eq) => ctx.mk(Eq(eq.clone().subst_slice(ctx, sigma))),
            Composed(id, name, subs) => ctx.mk(Composed(
                *id,
                name.clone(),
                subs.into_iter()
                    .map(|s| s.clone().subst_slice(ctx, sigma))
                    .collect(),
            )),
        }
    }
}

impl SubstitutableInPlace for ProofObject {
    fn subst_slice_in_place(&mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]) {
        let new = std::mem::replace(self, ctx.def()).subst_slice(ctx, sigma);
        *self = new;
    }
}

impl Substitutable for Category {
    fn subst_slice(self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Category {
        use ActualCategory::*;
        match self.deref() {
            Atomic(data) => {
                let pobj = data.pobj.clone().subst_slice(ctx, sigma);
                if let ActualProofObject::Cat(c) = pobj.deref() {
                    c.clone()
                } else {
                    ctx.mk(Atomic(CategoryData { pobj }))
                }
            }
        }
    }
}

impl SubstitutableInPlace for Category {
    fn subst_slice_in_place(&mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]) {
        let new = std::mem::replace(self, ctx.def()).subst_slice(ctx, sigma);
        *self = new;
    }
}

impl Substitutable for Functor {
    fn subst_slice(self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Self {
        use ActualFunctor::*;
        match self.deref() {
            Atomic(data) => {
                let src = data.src.clone().subst_slice(ctx, sigma);
                let dst = data.dst.clone().subst_slice(ctx, sigma);
                let pobj = data.pobj.clone().subst_slice(ctx, sigma);
                if let ActualProofObject::Funct(f) = pobj.deref() {
                    f.clone()
                } else {
                    ctx.mk(Atomic(FunctorData { pobj, src, dst }))
                }
            }
        }
    }
}

impl SubstitutableInPlace for Functor {
    fn subst_slice_in_place(&mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]) {
        let new = std::mem::replace(self, ctx.def()).subst_slice(ctx, sigma);
        *self = new;
    }
}

impl Substitutable for Object {
    fn subst_slice(self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Self {
        use ActualObject::*;
        match self.deref() {
            Atomic(data) => {
                let category = data.category.clone().subst_slice(ctx, sigma);
                let pobj = data.pobj.clone().subst_slice(ctx, sigma);
                if let ActualProofObject::Obj(o) = pobj.deref() {
                    o.clone()
                } else {
                    ctx.mk(Atomic(ObjectData { category, pobj }))
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

impl SubstitutableInPlace for Object {
    fn subst_slice_in_place(&mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]) {
        let new = std::mem::replace(self, ctx.def()).subst_slice(ctx, sigma);
        *self = new;
    }
}

impl Substitutable for Morphism {
    fn subst_slice(self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Self {
        use ActualMorphism::*;
        match self.deref() {
            Atomic(data) => {
                let category = data.category.clone().subst_slice(ctx, sigma);
                let src = data.src.clone().subst_slice(ctx, sigma);
                let dst = data.dst.clone().subst_slice(ctx, sigma);
                let pobj = data.pobj.clone().subst_slice(ctx, sigma);
                if let ActualProofObject::Mph(m) = pobj.deref() {
                    m.clone()
                } else {
                    ctx.mk(Atomic(MorphismData {
                        category,
                        src,
                        dst,
                        pobj,
                    }))
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

impl SubstitutableInPlace for Morphism {
    fn subst_slice_in_place(&mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]) {
        let new = std::mem::replace(self, ctx.def()).subst_slice(ctx, sigma);
        *self = new;
    }
}

impl Substitutable for Equality {
    fn subst_slice(self, ctx: &Context, sigma: &[(u64, AnyTerm)]) -> Self {
        use ActualEquality::*;
        match self.deref() {
            Atomic(data) => {
                let category = data.category.clone().subst_slice(ctx, sigma);
                let src = data.src.clone().subst_slice(ctx, sigma);
                let dst = data.dst.clone().subst_slice(ctx, sigma);
                let left = data.left.clone().subst_slice(ctx, sigma);
                let right = data.right.clone().subst_slice(ctx, sigma);
                let pobj = data.pobj.clone().subst_slice(ctx, sigma);
                if let ActualProofObject::Eq(eq) = pobj.deref() {
                    eq.clone()
                } else {
                    ctx.mk(Atomic(EqualityData {
                        category,
                        src,
                        dst,
                        left,
                        right,
                        pobj,
                    }))
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

impl SubstitutableInPlace for Equality {
    fn subst_slice_in_place(&mut self, ctx: &Context, sigma: &[(u64, AnyTerm)]) {
        let new = std::mem::replace(self, ctx.def()).subst_slice(ctx, sigma);
        *self = new;
    }
}

#[cfg(test)]
mod tests {
    use crate::anyterm::AnyTerm;
    use crate::data::Context;
    use crate::dsl::{cat, mph, obj};
    use crate::substitution::Substitutable;

    #[test]
    pub fn simple_subst() {
        let ctx = Context::new();
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
            m_ex.clone().subst(&ctx, &subst),
            m.clone(),
            "Substitution failed"
        );

        let subst2 = vec![(1, AnyTerm::Mph(m2.clone())), (0, AnyTerm::Obj(c.clone()))];
        assert_eq!(
            m_ex.clone().subst(&ctx, &subst2),
            m.clone(),
            "Substitution 2 failed"
        );
    }
}
