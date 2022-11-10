use crate::data::ProofObject::{Existential, Term};
use crate::data::{Category, Functor, Morphism, Object};
use std::collections::HashMap;
use std::rc::Rc;

/// A substitution for a graph, with replacement for existential of all types
pub struct Substitution {
    pub categories: HashMap<u64, Rc<Category>>,
    pub functors: HashMap<u64, Rc<Functor>>,
    pub objects: HashMap<u64, Rc<Object>>,
    pub morphisms: HashMap<u64, Rc<Morphism>>,
}

impl Substitution {
    pub fn check(&self) -> bool {
        self.categories.iter().all(|(_, c)| c.check())
            && self.functors.iter().all(|(_, f)| f.check())
            && self.objects.iter().all(|(_, o)| o.check())
            && self.morphisms.iter().all(|(_, m)| m.check())
    }

    pub fn new() -> Substitution {
        Substitution {
            categories: HashMap::new(),
            functors: HashMap::new(),
            objects: HashMap::new(),
            morphisms: HashMap::new(),
        }
    }
}

impl Category {
    fn subst_impl(&self, sigma: &Substitution) -> Rc<Self> {
        use Category::*;
        match self {
            Atomic(data) => match data.pobj {
                Term(_, _) => Rc::new(Atomic(data.clone())),
                Existential(e) => sigma
                    .categories
                    .get(&e)
                    .unwrap_or(&Rc::new(Atomic(data.clone())))
                    .clone(),
            },
        }
    }

    #[inline]
    pub fn subst(&self, sigma: &Substitution) -> Rc<Self> {
        let res = self.subst_impl(sigma);
        assert!(res.check(), "Invalid category after substitution");
        res
    }
}

impl Functor {
    fn subst_impl(&self, sigma: &Substitution) -> Rc<Self> {
        use Functor::*;
        match self {
            Atomic(data) => {
                let old = Rc::new(Atomic(crate::data::FunctorData {
                    pobj: data.pobj.clone(),
                    src: data.src.subst_impl(sigma),
                    dst: data.dst.subst_impl(sigma),
                }));
                match data.pobj {
                    Term(_, _) => old,
                    Existential(e) => {
                        let res = sigma.functors.get(&e).unwrap_or(&old).clone();
                        assert!(
                            res.src() == old.src(),
                            "Substitution changed functor source"
                        );
                        assert!(
                            res.dst() == old.dst(),
                            "Substitution changed functer destination"
                        );
                        res
                    }
                }
            }
        }
    }

    #[inline]
    pub fn subst(&self, sigma: &Substitution) -> Rc<Self> {
        let res = self.subst_impl(sigma);
        assert!(res.check(), "Invalid functor after substitution");
        res
    }
}

impl Object {
    fn subst_impl(&self, sigma: &Substitution) -> Rc<Self> {
        use Object::*;
        match self {
            Atomic(data) => {
                let old = Rc::new(Atomic(crate::data::ObjectData {
                    pobj: data.pobj.clone(),
                    category: data.category.subst_impl(sigma),
                }));
                match data.pobj {
                    Term(_, _) => old,
                    Existential(e) => {
                        let res = sigma.objects.get(&e).unwrap_or(&old).clone();
                        assert!(
                            res.cat() == old.cat(),
                            "Substitution changed category of object"
                        );
                        res
                    }
                }
            }
            Funct(f, o) => Rc::new(Funct(f.subst_impl(sigma), o.subst_impl(sigma))),
        }
    }

    #[inline]
    pub fn subst(&self, sigma: &Substitution) -> Rc<Self> {
        let res = self.subst_impl(sigma);
        assert!(res.check(), "Invalid object after substitution");
        res
    }
}

impl Morphism {
    fn subst_impl(&self, sigma: &Substitution) -> Rc<Self> {
        use Morphism::*;
        match self {
            Atomic(data) => {
                let old = Rc::new(Atomic(crate::data::MorphismData {
                    pobj: data.pobj.clone(),
                    category: data.category.subst_impl(sigma),
                    src: data.src.subst_impl(sigma),
                    dst: data.dst.subst_impl(sigma),
                }));
                match data.pobj {
                    Term(_, _) => old,
                    Existential(e) => {
                        let res = sigma.morphisms.get(&e).unwrap_or(&old).clone();
                        assert!(
                            res.cat() == old.cat(),
                            "Substitution changed category of morphism"
                        );
                        assert!(
                            res.src() == old.src(),
                            "Substitution changed source of morphism"
                        );
                        assert!(
                            res.dst() == old.dst(),
                            "Substitution changed destionation of morphism"
                        );
                        res
                    }
                }
            }
            Identity(o) => Rc::new(Identity(o.subst_impl(sigma))),
            Comp(m1, m2) => Rc::new(Comp(m1.subst_impl(sigma), m2.subst_impl(sigma))),
            Funct(f, m) => Rc::new(Funct(f.subst_impl(sigma), m.subst_impl(sigma))),
        }
    }

    #[inline]
    pub fn subst(&self, sigma: &Substitution) -> Rc<Self> {
        let res = self.subst_impl(sigma);
        assert!(res.check(), "Invalid morphism after substitution");
        res
    }
}
