use crate::data::{ActualCategory, ActualEquality, ActualFunctor, ActualMorphism, ActualObject};
use crate::data::{Category, Context, Equality, Functor, Morphism, Object, ProofObject};
use core::ops::Deref;
use serde::{Serialize, Serializer};

/// Represent an arbitrary term
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AnyTerm {
    Cat(Category),
    Funct(Functor),
    Obj(Object),
    Mph(Morphism),
    Eq(Equality),
    TypedCat(Category),
    TypedFunct(Functor),
    TypedObj(Object),
    TypedMph(Morphism),
    TypedEq(Equality),
}

impl AnyTerm {
    pub fn check(&self, ctx: &Context) -> bool {
        use AnyTerm::*;
        match self {
            Cat(cat) => cat.check(ctx),
            Funct(f) => f.check(ctx),
            Obj(o) => o.check(ctx),
            Mph(m) => m.check(ctx),
            Eq(eq) => eq.check(ctx),
            TypedCat(cat) => cat.check(ctx),
            TypedFunct(f) => f.check(ctx),
            TypedObj(o) => o.check(ctx),
            TypedMph(m) => m.check(ctx),
            TypedEq(eq) => eq.check(ctx),
        }
    }

    pub fn to_typed(self) -> Self {
        use AnyTerm::*;
        match self {
            Cat(cat) => TypedCat(cat),
            Funct(f) => TypedFunct(f),
            Obj(o) => TypedObj(o),
            Mph(m) => TypedMph(m),
            Eq(eq) => TypedEq(eq),
            _ => self,
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
            _ => None,
        }
    }

    pub fn expect_cat(&self, _ctx: &Context) -> Category {
        match self {
            AnyTerm::Cat(cat) => cat.clone(),
            _ => panic!("Tried to substitute something else for a category"),
        }
    }

    pub fn expect_funct(&self, ctx: &Context, src: Category, dst: Category) -> Functor {
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

    pub fn expect_obj(&self, ctx: &Context, cat: Category) -> Object {
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

    pub fn expect_mph(&self, ctx: &Context, cat: Category, src: Object, dst: Object) -> Morphism {
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
        ctx: &Context,
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

pub struct TermIterator {
    term: AnyTerm,
    child: usize,
    ctx: Context,
}

pub trait IsTerm {
    fn term(self) -> AnyTerm;
    fn typed(self) -> AnyTerm
    where
        Self: Sized,
    {
        self.term().to_typed()
    }
    fn subterms(self, ctx: Context) -> TermIterator;
    /// Checks if the two terms have the same head.
    /// Behaviour unspecified if one of the two is a variable.
    fn same_head(&self, other: &Self) -> bool;
}

impl IsTerm for AnyTerm {
    fn term(self) -> AnyTerm {
        self
    }

    fn subterms(self, ctx: Context) -> TermIterator {
        TermIterator {
            term: self,
            child: 0,
            ctx,
        }
    }

    fn same_head(&self, other: &Self) -> bool {
        use AnyTerm::*;
        match (self, other) {
            (Cat(c1), Cat(c2)) => c1.same_head(c2),
            (Funct(f1), Funct(f2)) => f1.same_head(f2),
            (Obj(o1), Obj(o2)) => o1.same_head(o2),
            (Mph(m1), Mph(m2)) => m1.same_head(m2),
            (Eq(e1), Eq(e2)) => e1.same_head(e2),
            (TypedCat(..), TypedCat(..)) => true,
            (TypedFunct(..), TypedFunct(..)) => true,
            (TypedObj(..), TypedObj(..)) => true,
            (TypedMph(..), TypedMph(..)) => true,
            (TypedEq(..), TypedEq(..)) => true,
            _ => false,
        }
    }
}

fn pobj_same(p1: &ProofObject, p2: &ProofObject) -> bool {
    use ProofObject::*;
    match (p1, p2) {
        (Term(t1), Term(t2)) => t1 == t2,
        _ => false,
    }
}

impl IsTerm for Category {
    fn term(self) -> AnyTerm {
        AnyTerm::Cat(self)
    }

    fn subterms(self, ctx: Context) -> TermIterator {
        TermIterator {
            term: self.term(),
            child: 0,
            ctx,
        }
    }

    fn same_head(&self, other: &Self) -> bool {
        use ActualCategory::*;
        match (self.deref(), other.deref()) {
            (Atomic(d1), Atomic(d2)) => pobj_same(&d1.pobj, &d2.pobj),
        }
    }
}
impl IsTerm for Functor {
    fn term(self) -> AnyTerm {
        AnyTerm::Funct(self)
    }

    fn subterms(self, ctx: Context) -> TermIterator {
        TermIterator {
            term: self.term(),
            child: 0,
            ctx,
        }
    }

    fn same_head(&self, other: &Self) -> bool {
        use ActualFunctor::*;
        match (self.deref(), other.deref()) {
            (Atomic(d1), Atomic(d2)) => pobj_same(&d1.pobj, &d2.pobj),
        }
    }
}
impl IsTerm for Object {
    fn term(self) -> AnyTerm {
        AnyTerm::Obj(self)
    }

    fn subterms(self, ctx: Context) -> TermIterator {
        TermIterator {
            term: self.term(),
            child: 0,
            ctx,
        }
    }

    fn same_head(&self, other: &Self) -> bool {
        use ActualObject::*;
        match (self.deref(), other.deref()) {
            (Atomic(d1), Atomic(d2)) => pobj_same(&d1.pobj, &d2.pobj),
            (Funct(..), Funct(..)) => true,
            _ => false,
        }
    }
}
impl IsTerm for Morphism {
    fn term(self) -> AnyTerm {
        AnyTerm::Mph(self)
    }

    fn subterms(self, ctx: Context) -> TermIterator {
        TermIterator {
            term: self.term(),
            child: 0,
            ctx,
        }
    }

    fn same_head(&self, other: &Self) -> bool {
        use ActualMorphism::*;
        match (self.deref(), other.deref()) {
            (Atomic(d1), Atomic(d2)) => pobj_same(&d1.pobj, &d2.pobj),
            (Identity(..), Identity(..)) => true,
            (Comp(..), Comp(..)) => true,
            (Funct(..), Funct(..)) => true,
            _ => false,
        }
    }
}
impl IsTerm for Equality {
    fn term(self) -> AnyTerm {
        AnyTerm::Eq(self)
    }

    fn subterms(self, ctx: Context) -> TermIterator {
        TermIterator {
            term: self.term(),
            child: 0,
            ctx,
        }
    }

    fn same_head(&self, other: &Self) -> bool {
        use ActualEquality::*;
        match (self.deref(), other.deref()) {
            (Atomic(d1), Atomic(d2)) => pobj_same(&d1.pobj, &d2.pobj),
            (Refl(..), Refl(..)) => true,
            (Concat(..), Concat(..)) => true,
            (Inv(..), Inv(..)) => true,
            (Compose(..), Compose(..)) => true,
            (Assoc(..), Assoc(..)) => true,
            (LeftId(..), LeftId(..)) => true,
            (RightId(..), RightId(..)) => true,
            (RAp(..), RAp(..)) => true,
            (LAp(..), LAp(..)) => true,
            (FunctId(..), FunctId(..)) => true,
            (FunctComp(..), FunctComp(..)) => true,
            (FunctCtx(..), FunctCtx(..)) => true,
            _ => false,
        }
    }
}

impl Iterator for TermIterator {
    type Item = AnyTerm;
    fn next(&mut self) -> Option<AnyTerm> {
        use AnyTerm::*;
        let child = self.child;
        self.child += 1;
        match (self.term.clone(), child) {
            // TypedCat
            (TypedCat(c), 0) => Some(c.term()),
            // TypedFunct
            (TypedFunct(f), 0) => Some(f.src(&self.ctx).typed()),
            (TypedFunct(f), 1) => Some(f.dst(&self.ctx).typed()),
            (TypedFunct(f), 2) => Some(f.term()),
            // TypedObj
            (TypedObj(o), 0) => Some(o.cat(&self.ctx).typed()),
            (TypedObj(o), 1) => Some(o.term()),
            // TypedMph
            (TypedMph(m), 0) => Some(m.cat(&self.ctx).typed()),
            (TypedMph(m), 1) => Some(m.src(&self.ctx).typed()),
            (TypedMph(m), 2) => Some(m.dst(&self.ctx).typed()),
            (TypedMph(m), 3) => Some(m.term()),
            // TypedEq
            (TypedEq(e), 0) => Some(e.cat(&self.ctx).typed()),
            (TypedEq(e), 1) => Some(e.src(&self.ctx).typed()),
            (TypedEq(e), 2) => Some(e.dst(&self.ctx).typed()),
            (TypedEq(e), 3) => Some(e.left(&self.ctx).typed()),
            (TypedEq(e), 4) => Some(e.right(&self.ctx).typed()),
            (TypedEq(e), 5) => Some(e.term()),
            // Cat
            // Funct
            (Funct(f), _) => {
                use ActualFunctor::*;
                match (f.deref(), child) {
                    (Atomic(data), 0) if data.pobj.is_term() => Some(data.src.clone().typed()),
                    (Atomic(data), 1) if data.pobj.is_term() => Some(data.dst.clone().typed()),
                    _ => None,
                }
            }
            // Obj
            (Obj(o), _) => {
                use ActualObject::*;
                match (o.deref(), child) {
                    (Atomic(data), 0) if data.pobj.is_term() => Some(data.category.clone().typed()),
                    (Funct(f, _), 0) => Some(f.clone().typed()),
                    (Funct(_, o), 1) => Some(o.clone().typed()),
                    _ => None,
                }
            }
            // Mph
            (Mph(m), _) => {
                use ActualMorphism::*;
                match (m.deref(), child) {
                    (Atomic(data), 0) if data.pobj.is_term() => Some(data.category.clone().typed()),
                    (Atomic(data), 1) if data.pobj.is_term() => Some(data.src.clone().typed()),
                    (Atomic(data), 2) if data.pobj.is_term() => Some(data.dst.clone().typed()),
                    (Identity(o), 0) => Some(o.clone().typed()),
                    (Comp(m1, _), 0) => Some(m1.clone().typed()),
                    (Comp(_, m2), 1) => Some(m2.clone().typed()),
                    (Funct(f, _), 0) => Some(f.clone().typed()),
                    (Funct(_, m), 1) => Some(m.clone().typed()),
                    _ => None,
                }
            }
            // Eq
            (Eq(e), _) => {
                use ActualEquality::*;
                match (e.deref(), child) {
                    (Atomic(data), 0) if data.pobj.is_term() => Some(data.category.clone().typed()),
                    (Atomic(data), 1) if data.pobj.is_term() => Some(data.src.clone().typed()),
                    (Atomic(data), 2) if data.pobj.is_term() => Some(data.dst.clone().typed()),
                    (Atomic(data), 3) if data.pobj.is_term() => Some(data.left.clone().typed()),
                    (Atomic(data), 4) if data.pobj.is_term() => Some(data.right.clone().typed()),
                    (Refl(m), 0) => Some(m.clone().typed()),
                    (Concat(e1, _), 0) => Some(e1.clone().typed()),
                    (Concat(_, e2), 1) => Some(e2.clone().typed()),
                    (Inv(e), 0) => Some(e.clone().typed()),
                    (Compose(e1, _), 0) => Some(e1.clone().typed()),
                    (Compose(_, e2), 1) => Some(e2.clone().typed()),
                    (Assoc(m1, _, _), 0) => Some(m1.clone().typed()),
                    (Assoc(_, m2, _), 1) => Some(m2.clone().typed()),
                    (Assoc(_, _, m3), 2) => Some(m3.clone().typed()),
                    (LeftId(m), 0) => Some(m.clone().typed()),
                    (RightId(m), 0) => Some(m.clone().typed()),
                    (RAp(eq, _), 0) => Some(eq.clone().typed()),
                    (RAp(_, m), 1) => Some(m.clone().typed()),
                    (LAp(m, _), 0) => Some(m.clone().typed()),
                    (LAp(_, eq), 1) => Some(eq.clone().typed()),
                    (FunctId(f, _), 0) => Some(f.clone().typed()),
                    (FunctId(_, o), 1) => Some(o.clone().typed()),
                    (FunctComp(f, _, _), 0) => Some(f.clone().typed()),
                    (FunctComp(_, m1, _), 1) => Some(m1.clone().typed()),
                    (FunctComp(_, _, m2), 2) => Some(m2.clone().typed()),
                    (FunctCtx(f, _), 0) => Some(f.clone().typed()),
                    (FunctCtx(_, eq), 1) => Some(eq.clone().typed()),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

impl Serialize for AnyTerm {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use AnyTerm::*;
        match self {
            Cat(c) => s.serialize_newtype_variant("term", 0, "category", c.deref()),
            Funct(f) => s.serialize_newtype_variant("term", 1, "functor", f.deref()),
            Obj(o) => s.serialize_newtype_variant("term", 2, "object", o.deref()),
            Mph(m) => s.serialize_newtype_variant("term", 3, "morphism", m.deref()),
            Eq(e) => s.serialize_newtype_variant("term", 4, "equality", e.deref()),
            // TODO is that really what we want ?
            TypedCat(c) => s.serialize_newtype_variant("term", 0, "category", c.deref()),
            TypedFunct(f) => s.serialize_newtype_variant("term", 1, "functor", f.deref()),
            TypedObj(o) => s.serialize_newtype_variant("term", 2, "object", o.deref()),
            TypedMph(m) => s.serialize_newtype_variant("term", 3, "morphism", m.deref()),
            TypedEq(e) => s.serialize_newtype_variant("term", 4, "equality", e.deref()),
        }
    }
}
