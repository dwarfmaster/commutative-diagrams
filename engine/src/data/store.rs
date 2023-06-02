use super::{Feature, Tag};
use std::collections::HashMap;

#[derive(Debug, Hash, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum EvarStatus {
    Evar,
    Partial,
    Grounded,
}

#[derive(Debug, Clone)]
pub struct Obj {
    pub label: String,
    pub name: Option<String>,
    pub status: EvarStatus,
    pub cache: QueryCache,
}

#[derive(Debug, Clone)]
pub struct Store {
    pub objects: HashMap<u64, Vec<Obj>>,
}

impl Store {
    pub fn new() -> Self {
        Store {
            objects: HashMap::new(),
        }
    }

    pub fn push_state<F>(&mut self, mut f: F)
    where
        F: FnMut(u64, &Obj) -> Obj,
    {
        self.objects.iter_mut().for_each(|(id, v)| {
            let obj = v.last().unwrap();
            v.push(f(*id, obj))
        })
    }

    pub fn pop_state(&mut self) {
        self.objects.retain(|_, v| {
            v.pop();
            !v.is_empty()
        })
    }

    pub fn register(&mut self, id: u64, label: String, name: Option<String>, status: EvarStatus) {
        self.objects
            .entry(id)
            .and_modify(|v| {
                let obj = v.last_mut().unwrap();
                obj.label = label.clone();
                obj.name = name.clone();
                obj.status = status;
            })
            .or_insert_with(|| {
                vec![Obj {
                    label,
                    name,
                    status,
                    cache: QueryCache::new(),
                }]
            });
    }

    // Id must be present in the store
    pub fn store(&mut self, id: u64, feat: Feature) {
        let obj = self.objects.get_mut(&id).unwrap().last_mut().unwrap();
        obj.cache.store(feat)
    }

    pub fn get<'a>(&'a self, id: u64) -> Option<&'a Obj> {
        self.objects.get(&id).map(|v| v.last()).flatten()
    }

    pub fn query(&self, id: u64, tag: Tag) -> Option<Vec<Feature>> {
        let obj = self.get(id)?;
        obj.cache.query(tag)
    }

    pub fn is_funct_obj(&self, id: u64, cat: u64) -> Option<(u64, u64, u64)> {
        let obj = self.get(id)?;
        obj.cache.is_funct_obj(cat)
    }

    pub fn is_identity(&self, id: u64, cat: u64) -> Option<u64> {
        let obj = self.get(id)?;
        obj.cache.is_identity(cat)
    }

    pub fn is_comp(&self, id: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        let obj = self.get(id)?;
        obj.cache.is_comp(cat)
    }

    pub fn is_funct_mph(&self, id: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        let obj = self.get(id)?;
        obj.cache.is_funct_mph(cat)
    }
}

macro_rules! store {
    ($n:expr) => {
        Option<Vec<[u64;$n]>>
    }
}

#[derive(Debug, Clone)]
pub struct QueryCache {
    categories: store!(0),
    objects: store!(1),
    morphisms: store!(3),
    functors: store!(2),
    equalities: store!(5),
    applied_funct_objs: store!(4),
    identities: store!(2),
    compose_mphs: store!(6),
    applied_funct_mphs: store!(6),
    reflexivities: store!(4),
    concats: store!(8),
    inverse_eqs: store!(6),
    compose_eqs: store!(10),
    associativities: store!(8),
    left_unitalities: store!(4),
    right_unitalities: store!(4),
    left_applications: store!(8),
    right_applications: store!(8),
    funct_identities: store!(4),
    funct_compositions: store!(8),
    applied_funct_eqs: store!(8),
}

macro_rules! get_store {
    ($st:expr, Category) => {
        $st.categories
    };
    ($st:expr, Object) => {
        $st.objects
    };
    ($st:expr, Morphism) => {
        $st.morphisms
    };
    ($st:expr, Functor) => {
        $st.functors
    };
    ($st:expr, Equality) => {
        $st.equalities
    };
    ($st:expr, AppliedFunctObj) => {
        $st.applied_funct_objs
    };
    ($st:expr, Identity) => {
        $st.identities
    };
    ($st:expr, ComposeMph) => {
        $st.compose_mphs
    };
    ($st:expr, AppliedFunctMph) => {
        $st.applied_funct_mphs
    };
    ($st:expr, Reflexivity) => {
        $st.reflexivities
    };
    ($st:expr, Concat) => {
        $st.concats
    };
    ($st:expr, InverseEq) => {
        $st.inverse_eqs
    };
    ($st:expr, ComposeEq) => {
        $st.compose_eqs
    };
    ($st:expr, Associativity) => {
        $st.associativities
    };
    ($st:expr, LeftUnitality) => {
        $st.left_unitalities
    };
    ($st:expr, RightUnitality) => {
        $st.right_unitalities
    };
    ($st:expr, LeftApplication) => {
        $st.left_applications
    };
    ($st:expr, RightApplication) => {
        $st.right_applications
    };
    ($st:expr, FunctIdentity) => {
        $st.funct_identities
    };
    ($st:expr, FunctComposition) => {
        $st.funct_compositions
    };
    ($st:expr, AppliedFunctEq) => {
        $st.applied_funct_eqs
    };
}

macro_rules! do_store {
    ($st:expr, $tag:ident, $feat:expr) => {
        get_store!($st, $tag)
            .get_or_insert_with(|| Vec::new())
            .push($feat.iter().collect::<Vec<u64>>().try_into().unwrap())
    };
}

macro_rules! do_query {
    ($st:expr, $tag:ident) => {
        get_store!($st, $tag).as_ref().map(|v| {
            v.iter()
                .map(|a| Feature::from_iter($tag, a.iter().copied()).unwrap())
                .collect()
        })
    };
}

impl QueryCache {
    fn new() -> Self {
        Self {
            categories: None,
            objects: None,
            morphisms: None,
            functors: None,
            equalities: None,
            applied_funct_objs: None,
            identities: None,
            compose_mphs: None,
            applied_funct_mphs: None,
            reflexivities: None,
            concats: None,
            inverse_eqs: None,
            compose_eqs: None,
            associativities: None,
            left_unitalities: None,
            right_unitalities: None,
            left_applications: None,
            right_applications: None,
            funct_identities: None,
            funct_compositions: None,
            applied_funct_eqs: None,
        }
    }

    fn store(&mut self, feat: Feature) {
        use Tag::*;
        match feat.tag() {
            Category => do_store!(self, Category, feat),
            Object => do_store!(self, Object, feat),
            Morphism => do_store!(self, Morphism, feat),
            Functor => do_store!(self, Functor, feat),
            Equality => do_store!(self, Equality, feat),
            AppliedFunctObj => do_store!(self, AppliedFunctObj, feat),
            Identity => do_store!(self, Identity, feat),
            ComposeMph => do_store!(self, ComposeMph, feat),
            AppliedFunctMph => do_store!(self, AppliedFunctMph, feat),
            Reflexivity => do_store!(self, Reflexivity, feat),
            Concat => do_store!(self, Concat, feat),
            InverseEq => do_store!(self, InverseEq, feat),
            ComposeEq => do_store!(self, ComposeEq, feat),
            Associativity => do_store!(self, Associativity, feat),
            LeftUnitality => do_store!(self, LeftUnitality, feat),
            RightUnitality => do_store!(self, RightUnitality, feat),
            LeftApplication => do_store!(self, LeftApplication, feat),
            RightApplication => do_store!(self, RightApplication, feat),
            FunctIdentity => do_store!(self, FunctIdentity, feat),
            FunctComposition => do_store!(self, FunctComposition, feat),
            AppliedFunctEq => do_store!(self, AppliedFunctEq, feat),
        }
    }

    fn query(&self, tag: Tag) -> Option<Vec<Feature>> {
        use Tag::*;
        match tag {
            Category => do_query!(self, Category),
            Object => do_query!(self, Object),
            Morphism => do_query!(self, Morphism),
            Functor => do_query!(self, Functor),
            Equality => do_query!(self, Equality),
            AppliedFunctObj => do_query!(self, AppliedFunctObj),
            Identity => do_query!(self, Identity),
            ComposeMph => do_query!(self, ComposeMph),
            AppliedFunctMph => do_query!(self, AppliedFunctMph),
            Reflexivity => do_query!(self, Reflexivity),
            Concat => do_query!(self, Concat),
            InverseEq => do_query!(self, InverseEq),
            ComposeEq => do_query!(self, ComposeEq),
            Associativity => do_query!(self, Associativity),
            LeftUnitality => do_query!(self, LeftUnitality),
            RightUnitality => do_query!(self, RightUnitality),
            LeftApplication => do_query!(self, LeftApplication),
            RightApplication => do_query!(self, RightApplication),
            FunctIdentity => do_query!(self, FunctIdentity),
            FunctComposition => do_query!(self, FunctComposition),
            AppliedFunctEq => do_query!(self, AppliedFunctEq),
        }
    }

    fn is_funct_obj(&self, qdcat: u64) -> Option<(u64, u64, u64)> {
        let v = self.query(Tag::AppliedFunctObj)?;
        v.into_iter().find_map(|feat| {
            if let Feature::AppliedFunctObj {
                scat,
                dcat,
                funct,
                obj,
            } = feat
            {
                if qdcat == dcat {
                    Some((scat, funct, obj))
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn is_identity(&self, qcat: u64) -> Option<u64> {
        let v = self.query(Tag::Identity)?;
        v.into_iter().find_map(|feat| {
            if let Feature::Identity { cat, obj } = feat {
                if qcat == cat {
                    Some(obj)
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn is_comp(&self, qcat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        let v = self.query(Tag::ComposeMph)?;
        v.into_iter().find_map(|feat| {
            if let Feature::ComposeMph {
                cat,
                src,
                mid,
                dst,
                m1,
                m2,
            } = feat
            {
                if qcat == cat {
                    Some((src, mid, dst, m1, m2))
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn is_funct_mph(&self, qdcat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        let v = self.query(Tag::AppliedFunctMph)?;
        v.into_iter().find_map(|feat| {
            if let Feature::AppliedFunctMph {
                scat,
                dcat,
                funct,
                src,
                dst,
                mph,
            } = feat
            {
                if qdcat == dcat {
                    Some((scat, funct, src, dst, mph))
                } else {
                    None
                }
            } else {
                None
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Feature, Tag};
    use super::{QueryCache, Store};

    #[test]
    fn cache() {
        let mut cache = QueryCache::new();
        cache.store(Feature::ComposeMph {
            cat: 0,
            src: 1,
            mid: 2,
            dst: 3,
            m1: 4,
            m2: 5,
        });
        cache.store(Feature::ComposeMph {
            cat: 6,
            src: 1,
            mid: 2,
            dst: 3,
            m1: 4,
            m2: 5,
        });
        assert_eq!(cache.query(Tag::Category), None);
        assert_eq!(cache.query(Tag::ComposeMph).map(|v| v.len()), Some(2));
        assert_eq!(cache.is_comp(0), Some((1, 2, 3, 4, 5)));
        assert_eq!(cache.is_comp(3), None);
        assert_eq!(cache.is_comp(6), Some((1, 2, 3, 4, 5)));
    }

    #[test]
    fn store() {
        type ES = super::EvarStatus;
        let mut store = Store::new();
        store.register(0, "C".to_string(), None, ES::Grounded);
        store.store(0, Feature::Category);
        store.register(1, "?x".to_string(), None, ES::Evar);
        store.store(1, Feature::Object { cat: 0 });
        assert!(store.get(0).is_some());
        assert_eq!(store.get(1).map(|o| o.status), Some(ES::Evar));
        assert_eq!(store.get(1).map(|o| &o.label[..]), Some("?x"));
        assert!(store.get(2).is_none());

        store.push_state(|_, x| x.clone());
        store.register(1, "a".to_string(), None, ES::Grounded);
        store.register(2, "1_a".to_string(), None, ES::Grounded);
        store.store(
            2,
            Feature::Morphism {
                cat: 0,
                src: 1,
                dst: 1,
            },
        );
        store.store(2, Feature::Identity { cat: 0, obj: 1 });
        assert_eq!(store.get(1).map(|o| o.status), Some(ES::Grounded));
        assert_eq!(store.is_identity(2, 0), Some(1));

        store.push_state(|_, x| x.clone());
        store.register(1, "a".to_string(), Some("a".to_string()), ES::Grounded);
        assert_eq!(
            store.get(1).map(|o| o.name.clone()),
            Some(Some("a".to_string()))
        );

        store.pop_state();
        assert_eq!(store.get(1).map(|o| o.name.clone()), Some(None));
        assert_eq!(store.is_identity(2, 0), Some(1));

        store.pop_state();
        assert_eq!(store.get(1).map(|o| o.status), Some(ES::Evar));
        assert!(store.get(2).is_none());
    }
}
