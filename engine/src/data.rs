use std::cmp::Ordering;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum ProofObject {
    Term(u64, Rc<String>),
    Existential(u64),
}
impl ProofObject {
    fn cmp_impl(&self, other: &ProofObject) -> Ordering {
        use ProofObject::*;
        match (self, other) {
            (Term(id1, _), Term(id2, _)) => id1.cmp(id2),
            (Existential(e1), Existential(e2)) => e1.cmp(e2),
            (Term(_, _), Existential(_)) => Ordering::Less,
            (Existential(_), Term(_, _)) => Ordering::Greater,
        }
    }
}
impl PartialEq for ProofObject {
    #[inline]
    fn eq(&self, other: &ProofObject) -> bool {
        use ProofObject::*;
        match (self, other) {
            (Term(id1, _), Term(id2, _)) => id1.eq(id2),
            (Existential(e1), Existential(e2)) => e1.eq(e2),
            _ => false,
        }
    }
}
impl Eq for ProofObject {}
impl PartialOrd for ProofObject {
    #[inline]
    fn partial_cmp(&self, other: &ProofObject) -> Option<Ordering> {
        Some(self.cmp_impl(other))
    }
}
impl Ord for ProofObject {
    #[inline]
    fn cmp(&self, other: &ProofObject) -> Ordering {
        self.cmp_impl(other)
    }
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
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct CategoryData {
    pub pobj: ProofObject,
}
derive_pobacked!(CategoryData);
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Category {
    Atomic(CategoryData),
}

impl Category {
    pub fn check(&self) -> bool {
        return true;
    }
}

//  _____                 _
// |  ___|   _ _ __   ___| |_ ___  _ __ ___
// | |_ | | | | '_ \ / __| __/ _ \| '__/ __|
// |  _|| |_| | | | | (__| || (_) | |  \__ \
// |_|   \__,_|_| |_|\___|\__\___/|_|  |___/
//
// Functors
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct FunctorData {
    pub pobj: ProofObject,
    pub src: Rc<Category>,
    pub dst: Rc<Category>,
}
derive_pobacked!(FunctorData);
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Functor {
    Atomic(FunctorData),
}

impl Functor {
    pub fn src(&self) -> Rc<Category> {
        use Functor::*;
        match self {
            Atomic(data) => data.src.clone(),
        }
    }
    pub fn dst(&self) -> Rc<Category> {
        use Functor::*;
        match self {
            Atomic(data) => data.dst.clone(),
        }
    }
    pub fn check(&self) -> bool {
        use Functor::*;
        match self {
            Atomic(data) => data.src.check() && data.dst.check(),
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
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct ObjectData {
    pub pobj: ProofObject,
    pub category: Rc<Category>,
}
derive_pobacked!(ObjectData);
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Object {
    Atomic(ObjectData),
    Funct(Rc<Functor>, Rc<Object>),
}

impl Object {
    pub fn cat(&self) -> Rc<Category> {
        use Object::*;
        match self {
            Atomic(data) => data.category.clone(),
            Funct(funct, _) => funct.dst(),
        }
    }
    pub fn check(&self) -> bool {
        use Object::*;
        match self {
            Atomic(data) => data.category.check(),
            Funct(funct, obj) => funct.check() && obj.check() && funct.src() == obj.cat(),
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
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct MorphismData {
    pub pobj: ProofObject,
    pub category: Rc<Category>,
    pub src: Rc<Object>,
    pub dst: Rc<Object>,
}
derive_pobacked!(MorphismData);
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Morphism {
    Atomic(MorphismData),
    Identity(Rc<Object>),
    Comp(Rc<Morphism>, Rc<Morphism>), // Comp(m1,m2) is m2 o m1
    Funct(Rc<Functor>, Rc<Morphism>),
}
fn comp(m1: &Rc<Morphism>, m2: &Rc<Morphism>) -> Rc<Morphism> {
    Rc::new(Morphism::Comp(m1.clone(), m2.clone()))
}

impl Morphism {
    pub fn cat(&self) -> Rc<Category> {
        use Morphism::*;
        match self {
            Atomic(data) => data.category.clone(),
            Identity(obj) => obj.cat(),
            Comp(m1, _) => m1.cat(),
            Funct(f, _) => f.dst(),
        }
    }
    pub fn src(&self) -> Rc<Object> {
        use Morphism::*;
        match self {
            Atomic(data) => data.src.clone(),
            Identity(obj) => obj.clone(),
            Comp(m1, _) => m1.src(),
            Funct(f, m) => Rc::new(Object::Funct(f.clone(), m.src())),
        }
    }
    pub fn dst(&self) -> Rc<Object> {
        use Morphism::*;
        match self {
            Atomic(data) => data.dst.clone(),
            Identity(obj) => obj.clone(),
            Comp(_, m2) => m2.dst(),
            Funct(f, m) => Rc::new(Object::Funct(f.clone(), m.dst())),
        }
    }
    pub fn check(&self) -> bool {
        use Morphism::*;
        match self {
            Atomic(data) => data.category.check() && data.src.check() && data.dst.check(),
            Identity(obj) => obj.check(),
            Comp(m1, m2) => m1.check() && m2.check() && m1.dst() == m2.src(),
            Funct(f, m) => f.check() && m.check() && f.src() == m.cat(),
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
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct EqualityData {
    pub pobj: ProofObject,
    pub category: Rc<Category>,
    pub src: Rc<Object>,
    pub dst: Rc<Object>,
    pub left: Rc<Morphism>,
    pub right: Rc<Morphism>,
}
derive_pobacked!(EqualityData);
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Equality {
    Atomic(EqualityData),
    Hole(Rc<Morphism>, Rc<Morphism>),
    Refl(Rc<Morphism>),
    Concat(Rc<Equality>, Rc<Equality>),
    Inv(Rc<Equality>),
    Compose(Rc<Equality>, Rc<Equality>),
    Assoc(Rc<Morphism>, Rc<Morphism>, Rc<Morphism>),
    LeftId(Rc<Morphism>),
    RightId(Rc<Morphism>),
    RAp(Rc<Equality>, Rc<Morphism>),
    LAp(Rc<Morphism>, Rc<Equality>),
    FunctId(Rc<Functor>, Rc<Object>),
    FunctComp(Rc<Functor>, Rc<Morphism>, Rc<Morphism>),
    FunctCtx(Rc<Functor>, Rc<Equality>),
}

impl Equality {
    pub fn cat(&self) -> Rc<Category> {
        use Equality::*;
        match self {
            Atomic(data) => data.category.clone(),
            Hole(l, _) => l.cat(),
            Refl(m) => m.cat(),
            Concat(eq1, _) => eq1.cat(),
            Inv(eq) => eq.cat(),
            Compose(eq1, _) => eq1.cat(),
            Assoc(m1, _, _) => m1.cat(),
            LeftId(m) => m.cat(),
            RightId(m) => m.cat(),
            RAp(_, m) => m.cat(),
            LAp(m, _) => m.cat(),
            FunctId(f, _) => f.dst(),
            FunctComp(f, _, _) => f.dst(),
            FunctCtx(f, _) => f.dst(),
        }
    }
    pub fn src(&self) -> Rc<Object> {
        use Equality::*;
        match self {
            Atomic(data) => data.src.clone(),
            Hole(l, _) => l.src(),
            Refl(m) => m.src(),
            Concat(eq1, _) => eq1.src(),
            Inv(eq) => eq.src(),
            Compose(eq1, _) => eq1.src(),
            Assoc(m1, _, _) => m1.src(),
            LeftId(m) => m.src(),
            RightId(m) => m.src(),
            RAp(eq, _) => eq.src(),
            LAp(m, _) => m.src(),
            FunctId(f, o) => Rc::new(Object::Funct(f.clone(), o.clone())),
            FunctComp(f, m1, _) => Rc::new(Object::Funct(f.clone(), m1.src())),
            FunctCtx(f, eq) => Rc::new(Object::Funct(f.clone(), eq.src())),
        }
    }
    pub fn dst(&self) -> Rc<Object> {
        use Equality::*;
        match self {
            Atomic(data) => data.dst.clone(),
            Hole(l, _) => l.dst(),
            Refl(m) => m.dst(),
            Concat(eq1, _) => eq1.dst(),
            Inv(eq) => eq.dst(),
            Compose(_, eq2) => eq2.dst(),
            Assoc(_, _, m3) => m3.dst(),
            LeftId(m) => m.dst(),
            RightId(m) => m.dst(),
            RAp(_, m) => m.dst(),
            LAp(_, eq) => eq.dst(),
            FunctId(f, o) => Rc::new(Object::Funct(f.clone(), o.clone())),
            FunctComp(f, _, m2) => Rc::new(Object::Funct(f.clone(), m2.dst())),
            FunctCtx(f, eq) => Rc::new(Object::Funct(f.clone(), eq.dst())),
        }
    }
    pub fn left(&self) -> Rc<Morphism> {
        use Equality::*;
        match self {
            Atomic(data) => data.left.clone(),
            Hole(l, _) => l.clone(),
            Refl(m) => m.clone(),
            Concat(eq1, _) => eq1.left(),
            Inv(eq) => eq.right(),
            Compose(eq1, eq2) => comp(&eq1.left(), &eq2.left()),
            Assoc(m1, m2, m3) => comp(&comp(m1, m2), m3),
            LeftId(m) => comp(&Rc::new(Morphism::Identity(m.src())), m),
            RightId(m) => comp(m, &Rc::new(Morphism::Identity(m.dst()))),
            RAp(eq, m) => comp(&eq.left(), m),
            LAp(m, eq) => comp(m, &eq.left()),
            FunctId(f, o) => Rc::new(Morphism::Funct(
                f.clone(),
                Rc::new(Morphism::Identity(o.clone())),
            )),
            FunctComp(f, m1, m2) => Rc::new(Morphism::Funct(f.clone(), comp(m1, m2))),
            FunctCtx(f, eq) => Rc::new(Morphism::Funct(f.clone(), eq.left())),
        }
    }
    pub fn right(&self) -> Rc<Morphism> {
        use Equality::*;
        match self {
            Atomic(data) => data.right.clone(),
            Hole(_, r) => r.clone(),
            Refl(m) => m.clone(),
            Concat(_, eq2) => eq2.right(),
            Inv(eq) => eq.left(),
            Compose(eq1, eq2) => comp(&eq1.right(), &eq2.right()),
            Assoc(m1, m2, m3) => comp(m1, &comp(m2, m3)),
            LeftId(m) => m.clone(),
            RightId(m) => m.clone(),
            RAp(eq, m) => comp(&eq.right(), m),
            LAp(m, eq) => comp(m, &eq.right()),
            FunctId(f, o) => Rc::new(Morphism::Identity(Rc::new(Object::Funct(
                f.clone(),
                o.clone(),
            )))),
            FunctComp(f, m1, m2) => comp(
                &Rc::new(Morphism::Funct(f.clone(), m1.clone())),
                &Rc::new(Morphism::Funct(f.clone(), m2.clone())),
            ),
            FunctCtx(f, eq) => Rc::new(Morphism::Funct(f.clone(), eq.right())),
        }
    }
    pub fn check(&self) -> bool {
        use Equality::*;
        match self {
            Atomic(data) => {
                data.category.check()
                    && data.src.check()
                    && data.dst.check()
                    && data.left.check()
                    && data.right.check()
            }
            Hole(l, r) => l.check() && r.check() && l.src() == r.src() && l.dst() == r.dst(),
            Refl(m) => m.check(),
            Concat(eq1, eq2) => eq1.check() && eq2.check() && eq1.right() == eq2.left(),
            Inv(eq) => eq.check(),
            Compose(eq1, eq2) => eq1.check() && eq2.check() && eq1.dst() == eq2.src(),
            Assoc(m1, m2, m3) => {
                m1.check()
                    && m2.check()
                    && m3.check()
                    && m1.dst() == m2.src()
                    && m2.dst() == m3.src()
            }
            LeftId(m) => m.check(),
            RightId(m) => m.check(),
            RAp(eq, m) => eq.check() && m.check() && eq.dst() == m.src(),
            LAp(m, eq) => m.check() && eq.check() && m.dst() == eq.src(),
            FunctId(f, o) => f.check() && o.check() && f.src() == o.cat(),
            FunctComp(f, m1, m2) => {
                f.check() && m1.check() && m2.check() && f.src() == m1.cat() && m1.dst() == m2.src()
            }
            FunctCtx(f, eq) => f.check() && eq.check() && f.src() == eq.cat(),
        }
    }
}
