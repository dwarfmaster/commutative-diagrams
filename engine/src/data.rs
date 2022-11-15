use core::ops::Deref;
use hashconsing::{HConsed, HConsign, HashConsign};
use serde::ser::{SerializeStruct, SerializeTupleVariant};
use serde::{Deserialize, Serialize, Serializer};
use std::collections::HashMap;

//  ____                   __    ___  _     _           _
// |  _ \ _ __ ___   ___  / _|  / _ \| |__ (_) ___  ___| |_
// | |_) | '__/ _ \ / _ \| |_  | | | | '_ \| |/ _ \/ __| __|
// |  __/| | | (_) | (_) |  _| | |_| | |_) | |  __/ (__| |_
// |_|   |_|  \___/ \___/|_|    \___/|_.__// |\___|\___|\__|
//                                       |__/
// Proof Object
#[derive(Serialize, Deserialize, Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProofObject {
    Term(u64),
    Existential(u64),
}

pub trait IsPOBacked {
    fn pobj(&self) -> ProofObject;
}
macro_rules! derive_pobacked {
    ($t:ty) => {
        impl IsPOBacked for $t {
            fn pobj(&self) -> ProofObject {
                self.pobj.clone()
            }
        }
    };
}

//   ____      _                        _
//  / ___|__ _| |_ ___  __ _  ___  _ __(_) ___  ___
// | |   / _` | __/ _ \/ _` |/ _ \| '__| |/ _ \/ __|
// | |__| (_| | ||  __/ (_| | (_) | |  | |  __/\__ \
//  \____\__,_|\__\___|\__, |\___/|_|  |_|\___||___/
//                     |___/
// Categories
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct CategoryData {
    pub pobj: ProofObject,
}
derive_pobacked!(CategoryData);
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ActualCategory {
    Atomic(CategoryData),
}
pub type Category = HConsed<ActualCategory>;

impl ActualCategory {
    pub fn check(&self, ctx: &mut Context) -> bool {
        use ActualCategory::*;
        match self {
            Atomic(data) => ctx.has_po(&data.pobj),
        }
    }
}

impl Serialize for CategoryData {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut cat = s.serialize_struct("categoryData", 1)?;
        cat.serialize_field("pobj", &self.pobj)?;
        cat.end()
    }
}
impl Serialize for ActualCategory {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use ActualCategory::*;
        match self {
            Atomic(data) => s.serialize_newtype_variant("category", 0, "atomic", data),
        }
    }
}

//  _____                 _
// |  ___|   _ _ __   ___| |_ ___  _ __ ___
// | |_ | | | | '_ \ / __| __/ _ \| '__/ __|
// |  _|| |_| | | | | (__| || (_) | |  \__ \
// |_|   \__,_|_| |_|\___|\__\___/|_|  |___/
//
// Functors
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctorData {
    pub pobj: ProofObject,
    pub src: Category,
    pub dst: Category,
}
derive_pobacked!(FunctorData);
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ActualFunctor {
    Atomic(FunctorData),
}
pub type Functor = HConsed<ActualFunctor>;

impl ActualFunctor {
    pub fn src(&self, _ctx: &mut Context) -> Category {
        use ActualFunctor::*;
        match self {
            Atomic(data) => data.src.clone(),
        }
    }
    pub fn dst(&self, _ctx: &mut Context) -> Category {
        use ActualFunctor::*;
        match self {
            Atomic(data) => data.dst.clone(),
        }
    }
    pub fn check(&self, ctx: &mut Context) -> bool {
        use ActualFunctor::*;
        match self {
            Atomic(data) => data.src.check(ctx) && data.dst.check(ctx) && ctx.has_po(&data.pobj),
        }
    }
}

impl Serialize for FunctorData {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut f = s.serialize_struct("functorData", 3)?;
        f.serialize_field("pobj", &self.pobj)?;
        f.serialize_field("src", &self.src.deref())?;
        f.serialize_field("dst", &self.dst.deref())?;
        f.end()
    }
}
impl Serialize for ActualFunctor {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use ActualFunctor::*;
        match self {
            Atomic(data) => s.serialize_newtype_variant("functor", 0, "atomic", data),
        }
    }
}

//   ___  _     _           _
//  / _ \| |__ (_) ___  ___| |_ ___
// | | | | '_ \| |/ _ \/ __| __/ __|
// | |_| | |_) | |  __/ (__| |_\__ \
//  \___/|_.__// |\___|\___|\__|___/
//           |__/
// Objects
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ObjectData {
    pub pobj: ProofObject,
    pub category: Category,
}
derive_pobacked!(ObjectData);
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ActualObject {
    Atomic(ObjectData),
    Funct(Functor, Object),
}
pub type Object = HConsed<ActualObject>;

impl ActualObject {
    pub fn cat(&self, ctx: &mut Context) -> Category {
        use ActualObject::*;
        match self {
            Atomic(data) => data.category.clone(),
            Funct(funct, _) => funct.dst(ctx),
        }
    }
    pub fn check(&self, ctx: &mut Context) -> bool {
        use ActualObject::*;
        match self {
            Atomic(data) => data.category.check(ctx) && ctx.has_po(&data.pobj),
            Funct(funct, obj) => {
                funct.check(ctx) && obj.check(ctx) && funct.src(ctx) == obj.cat(ctx)
            }
        }
    }
}

impl Serialize for ObjectData {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut o = s.serialize_struct("objectData", 2)?;
        o.serialize_field("pobj", &self.pobj)?;
        o.serialize_field("category", &self.category.deref())?;
        o.end()
    }
}
impl Serialize for ActualObject {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use ActualObject::*;
        match self {
            Atomic(data) => s.serialize_newtype_variant("object", 0, "atomic", data),
            Funct(f, o) => {
                let mut x = s.serialize_tuple_variant("object", 1, "funct", 2)?;
                x.serialize_field(f.deref())?;
                x.serialize_field(o.deref())?;
                x.end()
            }
        }
    }
}

//  __  __                  _     _
// |  \/  | ___  _ __ _ __ | |__ (_)___ _ __ ___  ___
// | |\/| |/ _ \| '__| '_ \| '_ \| / __| '_ ` _ \/ __|
// | |  | | (_) | |  | |_) | | | | \__ \ | | | | \__ \
// |_|  |_|\___/|_|  | .__/|_| |_|_|___/_| |_| |_|___/
//                   |_|
// Morphisms
// TODO isomorphisms
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MorphismData {
    pub pobj: ProofObject,
    pub category: Category,
    pub src: Object,
    pub dst: Object,
}
derive_pobacked!(MorphismData);
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ActualMorphism {
    Atomic(MorphismData),
    Identity(Object),
    Comp(Morphism, Morphism), // Comp(m1,m2) is m2 o m1
    Funct(Functor, Morphism),
}
pub type Morphism = HConsed<ActualMorphism>;

impl ActualMorphism {
    pub fn cat(&self, ctx: &mut Context) -> Category {
        use ActualMorphism::*;
        match self {
            Atomic(data) => data.category.clone(),
            Identity(obj) => obj.cat(ctx),
            Comp(m1, _) => m1.cat(ctx),
            Funct(f, _) => f.dst(ctx),
        }
    }
    pub fn src(&self, ctx: &mut Context) -> Object {
        use ActualMorphism::*;
        match self {
            Atomic(data) => data.src.clone(),
            Identity(obj) => obj.clone(),
            Comp(m1, _) => m1.src(ctx),
            Funct(f, m) => {
                let src = m.src(ctx);
                ctx.fobj(f.clone(), src)
            }
        }
    }
    pub fn dst(&self, ctx: &mut Context) -> Object {
        use ActualMorphism::*;
        match self {
            Atomic(data) => data.dst.clone(),
            Identity(obj) => obj.clone(),
            Comp(_, m2) => m2.dst(ctx),
            Funct(f, m) => {
                let dst = m.dst(ctx);
                ctx.fobj(f.clone(), dst)
            }
        }
    }
    pub fn check(&self, ctx: &mut Context) -> bool {
        use ActualMorphism::*;
        match self {
            Atomic(data) => {
                data.category.check(ctx)
                    && data.src.check(ctx)
                    && data.dst.check(ctx)
                    && data.src.cat(ctx) == data.category
                    && data.dst.cat(ctx) == data.category
                    && ctx.has_po(&data.pobj)
            }
            Identity(obj) => obj.check(ctx),
            Comp(m1, m2) => m1.check(ctx) && m2.check(ctx) && m1.dst(ctx) == m2.src(ctx),
            Funct(f, m) => f.check(ctx) && m.check(ctx) && f.src(ctx) == m.cat(ctx),
        }
    }
}

impl Serialize for MorphismData {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut m = s.serialize_struct("objectData", 4)?;
        m.serialize_field("pobj", &self.pobj)?;
        m.serialize_field("category", &self.category.deref())?;
        m.serialize_field("src", &self.src.deref())?;
        m.serialize_field("dst", &self.dst.deref())?;
        m.end()
    }
}
impl Serialize for ActualMorphism {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use ActualMorphism::*;
        match self {
            Atomic(data) => s.serialize_newtype_variant("morphism", 0, "atomic", data),
            Identity(o) => s.serialize_newtype_variant("morphism", 1, "identity", o.deref()),
            Comp(m1, m2) => {
                let mut x = s.serialize_tuple_variant("morphism", 2, "comp", 2)?;
                x.serialize_field(m1.deref())?;
                x.serialize_field(m2.deref())?;
                x.end()
            }
            Funct(f, m) => {
                let mut x = s.serialize_tuple_variant("morphism", 3, "funct", 2)?;
                x.serialize_field(f.deref())?;
                x.serialize_field(m.deref())?;
                x.end()
            }
        }
    }
}

//  _____                  _ _ _   _
// | ____|__ _ _   _  __ _| (_) |_(_) ___  ___
// |  _| / _` | | | |/ _` | | | __| |/ _ \/ __|
// | |__| (_| | |_| | (_| | | | |_| |  __/\__ \
// |_____\__, |\__,_|\__,_|_|_|\__|_|\___||___/
//          |_|
// Equalities
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct EqualityData {
    pub pobj: ProofObject,
    pub category: Category,
    pub src: Object,
    pub dst: Object,
    pub left: Morphism,
    pub right: Morphism,
}
derive_pobacked!(EqualityData);
#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ActualEquality {
    Atomic(EqualityData),
    Refl(Morphism),
    Concat(Equality, Equality),
    Inv(Equality),
    Compose(Equality, Equality),
    Assoc(Morphism, Morphism, Morphism),
    LeftId(Morphism),
    RightId(Morphism),
    RAp(Equality, Morphism),
    LAp(Morphism, Equality),
    FunctId(Functor, Object),
    FunctComp(Functor, Morphism, Morphism),
    FunctCtx(Functor, Equality),
}
pub type Equality = HConsed<ActualEquality>;

impl ActualEquality {
    pub fn cat(&self, ctx: &mut Context) -> Category {
        use ActualEquality::*;
        match self {
            Atomic(data) => data.category.clone(),
            Refl(m) => m.cat(ctx),
            Concat(eq1, _) => eq1.cat(ctx),
            Inv(eq) => eq.cat(ctx),
            Compose(eq1, _) => eq1.cat(ctx),
            Assoc(m1, _, _) => m1.cat(ctx),
            LeftId(m) => m.cat(ctx),
            RightId(m) => m.cat(ctx),
            RAp(_, m) => m.cat(ctx),
            LAp(m, _) => m.cat(ctx),
            FunctId(f, _) => f.dst(ctx),
            FunctComp(f, _, _) => f.dst(ctx),
            FunctCtx(f, _) => f.dst(ctx),
        }
    }
    pub fn src(&self, ctx: &mut Context) -> Object {
        use ActualEquality::*;
        match self {
            Atomic(data) => data.src.clone(),
            Refl(m) => m.src(ctx),
            Concat(eq1, _) => eq1.src(ctx),
            Inv(eq) => eq.src(ctx),
            Compose(eq1, _) => eq1.src(ctx),
            Assoc(m1, _, _) => m1.src(ctx),
            LeftId(m) => m.src(ctx),
            RightId(m) => m.src(ctx),
            RAp(eq, _) => eq.src(ctx),
            LAp(m, _) => m.src(ctx),
            FunctId(f, o) => ctx.fobj(f.clone(), o.clone()),
            FunctComp(f, m1, _) => {
                let src = m1.src(ctx);
                ctx.fobj(f.clone(), src)
            }
            FunctCtx(f, eq) => {
                let src = eq.src(ctx);
                ctx.fobj(f.clone(), src)
            }
        }
    }
    pub fn dst(&self, ctx: &mut Context) -> Object {
        use ActualEquality::*;
        match self {
            Atomic(data) => data.dst.clone(),
            Refl(m) => m.dst(ctx),
            Concat(eq1, _) => eq1.dst(ctx),
            Inv(eq) => eq.dst(ctx),
            Compose(_, eq2) => eq2.dst(ctx),
            Assoc(_, _, m3) => m3.dst(ctx),
            LeftId(m) => m.dst(ctx),
            RightId(m) => m.dst(ctx),
            RAp(_, m) => m.dst(ctx),
            LAp(_, eq) => eq.dst(ctx),
            FunctId(f, o) => ctx.fobj(f.clone(), o.clone()),
            FunctComp(f, _, m2) => {
                let dst = m2.dst(ctx);
                ctx.fobj(f.clone(), dst)
            }
            FunctCtx(f, eq) => {
                let dst = eq.dst(ctx);
                ctx.fobj(f.clone(), dst)
            }
        }
    }
    pub fn left(&self, ctx: &mut Context) -> Morphism {
        use ActualEquality::*;
        match self {
            Atomic(data) => data.left.clone(),
            Refl(m) => m.clone(),
            Concat(eq1, _) => eq1.left(ctx),
            Inv(eq) => eq.right(ctx),
            Compose(eq1, eq2) => {
                let l1 = eq1.left(ctx);
                let l2 = eq2.left(ctx);
                ctx.comp(l1, l2)
            }
            Assoc(m1, m2, m3) => {
                let cmp = ctx.comp(m1.clone(), m2.clone());
                ctx.comp(cmp, m3.clone())
            }
            LeftId(m) => {
                let src = m.src(ctx);
                let id = ctx.id(src);
                ctx.comp(id, m.clone())
            }
            RightId(m) => {
                let dst = m.dst(ctx);
                let id = ctx.id(dst);
                ctx.comp(m.clone(), id)
            }
            RAp(eq, m) => {
                let l = eq.left(ctx);
                ctx.comp(l, m.clone())
            }
            LAp(m, eq) => {
                let l = eq.left(ctx);
                ctx.comp(m.clone(), l)
            }
            FunctId(f, o) => {
                let id = ctx.id(o.clone());
                ctx.fmph(f.clone(), id)
            }
            FunctComp(f, m1, m2) => {
                let cmp = ctx.comp(m1.clone(), m2.clone());
                ctx.fmph(f.clone(), cmp)
            }
            FunctCtx(f, eq) => {
                let l = eq.left(ctx);
                ctx.fmph(f.clone(), l)
            }
        }
    }
    pub fn right(&self, ctx: &mut Context) -> Morphism {
        use ActualEquality::*;
        match self {
            Atomic(data) => data.right.clone(),
            Refl(m) => m.clone(),
            Concat(_, eq2) => eq2.right(ctx),
            Inv(eq) => eq.left(ctx),
            Compose(eq1, eq2) => {
                let r1 = eq1.right(ctx);
                let r2 = eq2.right(ctx);
                ctx.comp(r1, r2)
            }
            Assoc(m1, m2, m3) => {
                let cmp = ctx.comp(m2.clone(), m3.clone());
                ctx.comp(m1.clone(), cmp)
            }
            LeftId(m) => m.clone(),
            RightId(m) => m.clone(),
            RAp(eq, m) => {
                let r = eq.right(ctx);
                ctx.comp(r, m.clone())
            }
            LAp(m, eq) => {
                let r = eq.right(ctx);
                ctx.comp(m.clone(), r)
            }
            FunctId(f, o) => {
                let o = ctx.fobj(f.clone(), o.clone());
                ctx.id(o)
            }
            FunctComp(f, m1, m2) => {
                let f1 = ctx.fmph(f.clone(), m1.clone());
                let f2 = ctx.fmph(f.clone(), m2.clone());
                ctx.comp(f1, f2)
            }
            FunctCtx(f, eq) => {
                let r = eq.right(ctx);
                ctx.fmph(f.clone(), r)
            }
        }
    }
    pub fn check(&self, ctx: &mut Context) -> bool {
        use ActualEquality::*;
        match self {
            Atomic(data) => {
                data.category.check(ctx)
                    && data.src.check(ctx)
                    && data.dst.check(ctx)
                    && data.left.check(ctx)
                    && data.right.check(ctx)
                    && data.src.cat(ctx) == data.category
                    && data.dst.cat(ctx) == data.category
                    && data.left.src(ctx) == data.src
                    && data.left.dst(ctx) == data.dst
                    && data.right.src(ctx) == data.src
                    && data.right.dst(ctx) == data.dst
                    && ctx.has_po(&data.pobj)
            }
            Refl(m) => m.check(ctx),
            Concat(eq1, eq2) => eq1.check(ctx) && eq2.check(ctx) && eq1.right(ctx) == eq2.left(ctx),
            Inv(eq) => eq.check(ctx),
            Compose(eq1, eq2) => eq1.check(ctx) && eq2.check(ctx) && eq1.dst(ctx) == eq2.src(ctx),
            Assoc(m1, m2, m3) => {
                m1.check(ctx)
                    && m2.check(ctx)
                    && m3.check(ctx)
                    && m1.dst(ctx) == m2.src(ctx)
                    && m2.dst(ctx) == m3.src(ctx)
            }
            LeftId(m) => m.check(ctx),
            RightId(m) => m.check(ctx),
            RAp(eq, m) => eq.check(ctx) && m.check(ctx) && eq.dst(ctx) == m.src(ctx),
            LAp(m, eq) => m.check(ctx) && eq.check(ctx) && m.dst(ctx) == eq.src(ctx),
            FunctId(f, o) => f.check(ctx) && o.check(ctx) && f.src(ctx) == o.cat(ctx),
            FunctComp(f, m1, m2) => {
                f.check(ctx)
                    && m1.check(ctx)
                    && m2.check(ctx)
                    && f.src(ctx) == m1.cat(ctx)
                    && m1.dst(ctx) == m2.src(ctx)
            }
            FunctCtx(f, eq) => f.check(ctx) && eq.check(ctx) && f.src(ctx) == eq.cat(ctx),
        }
    }
}

impl Serialize for EqualityData {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut e = s.serialize_struct("equalityData:", 6)?;
        e.serialize_field("pobj", &self.pobj)?;
        e.serialize_field("category", &self.category.deref())?;
        e.serialize_field("src", &self.src.deref())?;
        e.serialize_field("dst", &self.dst.deref())?;
        e.serialize_field("left", &self.left.deref())?;
        e.serialize_field("right", &self.right.deref())?;
        e.end()
    }
}
impl Serialize for ActualEquality {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use ActualEquality::*;
        match self {
            Atomic(data) => s.serialize_newtype_variant("equality", 0, "atomic", data),
            Refl(m) => s.serialize_newtype_variant("equality", 1, "refl", m.deref()),
            Concat(eq1, eq2) => {
                let mut x = s.serialize_tuple_variant("equality", 2, "concat", 2)?;
                x.serialize_field(eq1.deref())?;
                x.serialize_field(eq2.deref())?;
                x.end()
            }
            Inv(eq) => s.serialize_newtype_variant("equality", 3, "inv", eq.deref()),
            Compose(eq1, eq2) => {
                let mut x = s.serialize_tuple_variant("equality", 4, "compose", 2)?;
                x.serialize_field(eq1.deref())?;
                x.serialize_field(eq2.deref())?;
                x.end()
            }
            Assoc(m1, m2, m3) => {
                let mut x = s.serialize_tuple_variant("equality", 5, "assoc", 3)?;
                x.serialize_field(m1.deref())?;
                x.serialize_field(m2.deref())?;
                x.serialize_field(m3.deref())?;
                x.end()
            }
            LeftId(m) => s.serialize_newtype_variant("equality", 6, "left_id", m.deref()),
            RightId(m) => s.serialize_newtype_variant("equality", 7, "right_id", m.deref()),
            RAp(eq, m) => {
                let mut x = s.serialize_tuple_variant("equality", 8, "rap", 2)?;
                x.serialize_field(eq.deref())?;
                x.serialize_field(m.deref())?;
                x.end()
            }
            LAp(m, eq) => {
                let mut x = s.serialize_tuple_variant("equality", 9, "lap", 2)?;
                x.serialize_field(m.deref())?;
                x.serialize_field(eq.deref())?;
                x.end()
            }
            FunctId(f, o) => {
                let mut x = s.serialize_tuple_variant("equality", 10, "funct_id", 2)?;
                x.serialize_field(f.deref())?;
                x.serialize_field(o.deref())?;
                x.end()
            }
            FunctComp(f, m1, m2) => {
                let mut x = s.serialize_tuple_variant("equality", 11, "funct_comp", 3)?;
                x.serialize_field(f.deref())?;
                x.serialize_field(m1.deref())?;
                x.serialize_field(m2.deref())?;
                x.end()
            }
            FunctCtx(f, eq) => {
                let mut x = s.serialize_tuple_variant("equality", 12, "funct_ctx", 2)?;
                x.serialize_field(f.deref())?;
                x.serialize_field(eq.deref())?;
                x.end()
            }
        }
    }
}

//   ____            _            _
//  / ___|___  _ __ | |_ _____  _| |_
// | |   / _ \| '_ \| __/ _ \ \/ / __|
// | |__| (_) | | | | ||  __/>  <| |_
//  \____\___/|_| |_|\__\___/_/\_\\__|
//
// Context
/// Stores the context in which terms must be interpreted
pub struct Context {
    /// The names of the terms proofobjects
    terms: HashMap<u64, String>,
    cat_factory: HConsign<ActualCategory>,
    funct_factory: HConsign<ActualFunctor>,
    obj_factory: HConsign<ActualObject>,
    mph_factory: HConsign<ActualMorphism>,
    eq_factory: HConsign<ActualEquality>,
}

pub trait ContextMakable {
    fn make(self, ctx: &mut Context) -> HConsed<Self>
    where
        Self: Sized;
}
impl ContextMakable for ActualCategory {
    fn make(self, ctx: &mut Context) -> HConsed<Self> {
        ctx.cat_factory.mk(self)
    }
}
impl ContextMakable for ActualFunctor {
    fn make(self, ctx: &mut Context) -> HConsed<Self> {
        ctx.funct_factory.mk(self)
    }
}
impl ContextMakable for ActualObject {
    fn make(self, ctx: &mut Context) -> HConsed<Self> {
        ctx.obj_factory.mk(self)
    }
}
impl ContextMakable for ActualMorphism {
    fn make(self, ctx: &mut Context) -> HConsed<Self> {
        ctx.mph_factory.mk(self)
    }
}
impl ContextMakable for ActualEquality {
    fn make(self, ctx: &mut Context) -> HConsed<Self> {
        ctx.eq_factory.mk(self)
    }
}

impl Context {
    pub fn new() -> Context {
        Context {
            terms: HashMap::new(),
            cat_factory: HConsign::empty(),
            funct_factory: HConsign::empty(),
            obj_factory: HConsign::empty(),
            mph_factory: HConsign::empty(),
            eq_factory: HConsign::empty(),
        }
    }

    fn has_po(&self, po: &ProofObject) -> bool {
        match po {
            ProofObject::Term(id) => self.terms.contains_key(&id),
            ProofObject::Existential(_) => true,
        }
    }

    pub fn term(&self, term: u64) -> Option<&str> {
        self.terms.get(&term).map(|s| &s[..])
    }

    pub fn new_term(&mut self, id: u64, name: &str) {
        self.terms.insert(id, name.to_string());
    }

    pub fn mk<T: ContextMakable>(&mut self, act: T) -> HConsed<T> {
        act.make(self)
    }

    // Some Helper functions
    pub fn comp(&mut self, m1: Morphism, m2: Morphism) -> Morphism {
        self.mk(ActualMorphism::Comp(m1, m2))
    }

    pub fn id(&mut self, o: Object) -> Morphism {
        self.mk(ActualMorphism::Identity(o))
    }

    pub fn fobj(&mut self, f: Functor, o: Object) -> Object {
        self.mk(ActualObject::Funct(f, o))
    }

    pub fn fmph(&mut self, f: Functor, m: Morphism) -> Morphism {
        self.mk(ActualMorphism::Funct(f, m))
    }
}

//  _____         _
// |_   _|__  ___| |_ ___
//   | |/ _ \/ __| __/ __|
//   | |  __/\__ \ |_\__ \
//   |_|\___||___/\__|___/
//
// Tests
#[test]
fn build_term() {
    let mut ctx = Context::new();
    ctx.new_term(0, "C");
    ctx.new_term(1, "a");
    ctx.new_term(2, "b");
    ctx.new_term(3, "c");
    let cat = ctx.mk(ActualCategory::Atomic(CategoryData {
        pobj: ProofObject::Term(0),
    }));
    let a = ctx.mk(ActualObject::Atomic(ObjectData {
        pobj: ProofObject::Term(1),
        category: cat.clone(),
    }));
    let b = ctx.mk(ActualObject::Atomic(ObjectData {
        pobj: ProofObject::Term(2),
        category: cat.clone(),
    }));
    let c = ctx.mk(ActualObject::Atomic(ObjectData {
        pobj: ProofObject::Term(2),
        category: cat.clone(),
    }));
    let m1 = ctx.mk(ActualMorphism::Atomic(MorphismData {
        pobj: ProofObject::Existential(0),
        category: cat.clone(),
        src: a.clone(),
        dst: b.clone(),
    }));
    let m2 = ctx.mk(ActualMorphism::Atomic(MorphismData {
        pobj: ProofObject::Existential(1),
        category: cat.clone(),
        src: b.clone(),
        dst: c.clone(),
    }));
    let m = ctx.comp(m1.clone(), m2.clone());
    assert!(m.check(&mut ctx), "m is ill typed");
    let m_ = ctx.comp(m1.clone(), m2.clone());
    assert_eq!(m.uid(), m_.uid(), "Hash-consing should get the same object");
}
