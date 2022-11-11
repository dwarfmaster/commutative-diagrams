use crate::data::{ActualCategory, ActualEquality, ActualFunctor, ActualMorphism, ActualObject};
use crate::data::{Category, Context, Equality, Functor, Morphism, Object};
use core::ops::Deref;

/// Represent an arbitrary term
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AnyTerm {
    Cat(Category),
    Funct(Functor),
    Obj(Object),
    Mph(Morphism),
    Eq(Equality),
}

impl AnyTerm {
    pub fn check(&self, ctx: &mut Context) -> bool {
        use AnyTerm::*;
        match self {
            Cat(cat) => cat.check(ctx),
            Funct(f) => f.check(ctx),
            Obj(o) => o.check(ctx),
            Mph(m) => m.check(ctx),
            Eq(eq) => eq.check(ctx),
        }
    }

    fn pobj_as_var<T: crate::data::IsPOBacked>(pobj: T) -> Option<u64> {
        use crate::data::ProofObject::*;
        match pobj.pobj() {
            Term(_) => None,
            Existential(e) => Some(e.clone()),
        }
    }

    pub fn as_var(&self) -> Option<u64> {
        use AnyTerm::*;
        match self {
            Cat(c) => match c.deref() {
                ActualCategory::Atomic(data) => AnyTerm::pobj_as_var(data.clone()),
            },
            Funct(f) => match f.deref() {
                ActualFunctor::Atomic(data) => AnyTerm::pobj_as_var(data.clone()),
            },
            Obj(o) => match o.deref() {
                ActualObject::Atomic(data) => AnyTerm::pobj_as_var(data.clone()),
                _ => None,
            },
            Mph(m) => match m.deref() {
                ActualMorphism::Atomic(data) => AnyTerm::pobj_as_var(data.clone()),
                _ => None,
            },
            Eq(e) => match e.deref() {
                ActualEquality::Atomic(data) => AnyTerm::pobj_as_var(data.clone()),
                _ => None,
            },
        }
    }

    pub fn expect_cat(&self, _ctx: &mut Context) -> Category {
        match self {
            AnyTerm::Cat(cat) => cat.clone(),
            _ => panic!("Tried to substitute something else for a category"),
        }
    }

    pub fn expect_funct(&self, ctx: &mut Context, src: Category, dst: Category) -> Functor {
        match self {
            AnyTerm::Funct(f) => {
                assert_eq!(
                    f.src(ctx),
                    src,
                    "Tried to substitute a functor of wrong source"
                );
                assert_eq!(
                    f.dst(ctx),
                    dst,
                    "Tried to substitute a functor of wrong destination"
                );
                f.clone()
            }
            _ => panic!("Tried to substiture something else for a functor"),
        }
    }

    pub fn expect_obj(&self, ctx: &mut Context, cat: Category) -> Object {
        match self {
            AnyTerm::Obj(o) => {
                assert_eq!(
                    o.cat(ctx),
                    cat,
                    "Tried to substitute an object of wrong category"
                );
                o.clone()
            }
            _ => panic!("Tried to substiture something else for an object"),
        }
    }

    pub fn expect_mph(
        &self,
        ctx: &mut Context,
        cat: Category,
        src: Object,
        dst: Object,
    ) -> Morphism {
        match self {
            AnyTerm::Mph(m) => {
                assert_eq!(
                    m.cat(ctx),
                    cat,
                    "Tried to substitute a morphism of wrong category"
                );
                assert_eq!(
                    m.src(ctx),
                    src,
                    "Tried to substitute a morphism of wrong source"
                );
                assert_eq!(
                    m.dst(ctx),
                    dst,
                    "Tried to substitute a morphism of wrong destination"
                );
                m.clone()
            }
            _ => panic!("Tried to substiture something else for a morphism"),
        }
    }

    pub fn expect_eq(
        &self,
        ctx: &mut Context,
        cat: Category,
        src: Object,
        dst: Object,
        left: Morphism,
        right: Morphism,
    ) -> Equality {
        match self {
            AnyTerm::Eq(eq) => {
                assert_eq!(
                    eq.cat(ctx),
                    cat,
                    "Tried to substitute an equality of wrong category"
                );
                assert_eq!(
                    eq.src(ctx),
                    src,
                    "Tried to substitute an equality of wrong source"
                );
                assert_eq!(
                    eq.dst(ctx),
                    dst,
                    "Tried to substitute an equality of wrong destination"
                );
                assert_eq!(
                    eq.left(ctx),
                    left,
                    "Tried to substitute an equality of wrong left hand side"
                );
                assert_eq!(
                    eq.right(ctx),
                    right,
                    "Tried to substitute an equality of wrong right hand side"
                );
                eq.clone()
            }
            _ => panic!("Tried to substiture something else for an equality"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TermIterator {
    term: AnyTerm,
    child: usize,
}

pub trait IsTerm {
    fn term(self) -> AnyTerm;
    fn subterms(self) -> TermIterator
    where
        Self: Sized,
    {
        TermIterator {
            term: self.term(),
            child: 0,
        }
    }
}

impl IsTerm for AnyTerm {
    fn term(self) -> AnyTerm {
        self
    }
}
impl IsTerm for Category {
    fn term(self) -> AnyTerm {
        AnyTerm::Cat(self)
    }
}
impl IsTerm for Functor {
    fn term(self) -> AnyTerm {
        AnyTerm::Funct(self)
    }
}
impl IsTerm for Object {
    fn term(self) -> AnyTerm {
        AnyTerm::Obj(self)
    }
}
impl IsTerm for Morphism {
    fn term(self) -> AnyTerm {
        AnyTerm::Mph(self)
    }
}
impl IsTerm for Equality {
    fn term(self) -> AnyTerm {
        AnyTerm::Eq(self)
    }
}

impl Iterator for TermIterator {
    type Item = AnyTerm;
    fn next(&mut self) -> Option<AnyTerm> {
        use AnyTerm::*;
        let child = self.child;
        self.child += 1;
        match self.term.clone() {
            Cat(_) => None,
            Funct(_) => None,
            Obj(o) => {
                use ActualObject::*;
                match (o.deref(), child) {
                    (Funct(f, _), 0) => Some(f.clone().term()),
                    (Funct(_, o), 1) => Some(o.clone().term()),
                    _ => None,
                }
            }
            Mph(m) => {
                use ActualMorphism::*;
                match (m.deref(), child) {
                    (Identity(o), 0) => Some(o.clone().term()),
                    (Comp(m1, _), 0) => Some(m1.clone().term()),
                    (Comp(_, m2), 1) => Some(m2.clone().term()),
                    (Funct(f, _), 0) => Some(f.clone().term()),
                    (Funct(_, m), 1) => Some(m.clone().term()),
                    _ => None,
                }
            }
            Eq(e) => {
                use ActualEquality::*;
                match (e.deref(), child) {
                    (Refl(m), 0) => Some(m.clone().term()),
                    (Concat(e1, _), 0) => Some(e1.clone().term()),
                    (Concat(_, e2), 1) => Some(e2.clone().term()),
                    (Inv(e), 0) => Some(e.clone().term()),
                    (Compose(e1, _), 0) => Some(e1.clone().term()),
                    (Compose(_, e2), 1) => Some(e2.clone().term()),
                    (Assoc(m1, _, _), 0) => Some(m1.clone().term()),
                    (Assoc(_, m2, _), 1) => Some(m2.clone().term()),
                    (Assoc(_, _, m3), 2) => Some(m3.clone().term()),
                    (LeftId(m), 0) => Some(m.clone().term()),
                    (RightId(m), 0) => Some(m.clone().term()),
                    (RAp(eq, _), 0) => Some(eq.clone().term()),
                    (RAp(_, m), 1) => Some(m.clone().term()),
                    (LAp(m, _), 0) => Some(m.clone().term()),
                    (LAp(_, eq), 1) => Some(eq.clone().term()),
                    (FunctId(f, _), 0) => Some(f.clone().term()),
                    (FunctId(_, o), 1) => Some(o.clone().term()),
                    (FunctComp(f, _, _), 0) => Some(f.clone().term()),
                    (FunctComp(_, m1, _), 1) => Some(m1.clone().term()),
                    (FunctComp(_, _, m2), 2) => Some(m2.clone().term()),
                    (FunctCtx(f, _), 0) => Some(f.clone().term()),
                    (FunctCtx(_, eq), 1) => Some(eq.clone().term()),
                    _ => None,
                }
            }
        }
    }
}
