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
    pub repr: Option<u64>,
    pub cache: QueryCache,
}

#[derive(Debug, Clone)]
pub struct Store {
    pub objects: HashMap<u64, Vec<Option<Obj>>>,
}

impl Store {
    pub fn new() -> Self {
        Store {
            objects: HashMap::new(),
        }
    }

    pub fn push_state(&mut self) {
        self.objects.iter_mut().for_each(|(_, v)| v.push(None))
    }

    pub fn pop_state(&mut self) {
        self.objects.retain(|_, v| {
            v.pop();
            !v.is_empty()
        })
    }

    pub fn register<'a>(
        &'a mut self,
        id: u64,
        label: String,
        name: Option<String>,
        status: EvarStatus,
    ) -> &'a Obj {
        self.objects
            .entry(id)
            .and_modify(|v| {
                let obj = v.last_mut().unwrap();
                *obj = Some(Obj {
                    label: label.clone(),
                    name: name.clone(),
                    status,
                    repr: None,
                    cache: QueryCache::new(),
                });
            })
            .or_insert_with(|| {
                vec![Some(Obj {
                    label,
                    name,
                    status,
                    repr: None,
                    cache: QueryCache::new(),
                })]
            })
            .last()
            .unwrap()
            .as_ref()
            .unwrap()
    }

    pub fn register_repr(&mut self, id: u64, repr: u64) {
        self.objects
            .get_mut(&id)
            .unwrap()
            .last_mut()
            .unwrap()
            .as_mut()
            .unwrap()
            .repr = Some(repr);
    }

    pub fn get<'a>(&'a self, id: u64) -> Option<&'a Obj> {
        self.objects
            .get(&id)
            .and_then(|v| v.last())
            .and_then(|o| o.as_ref())
    }

    pub fn get_mut<'a>(&'a mut self, id: u64) -> Option<&'a mut Obj> {
        self.objects
            .get_mut(&id)
            .and_then(|v| v.last_mut())
            .and_then(|o| o.as_mut())
    }

    pub fn query(&self, id: u64, tag: Tag) -> Option<Vec<Feature>> {
        let obj = self.get(id)?;
        obj.cache.query(tag)
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
    pub fn new() -> Self {
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

    pub fn store_none(&mut self, tag: Tag) {
        use Tag::*;
        match tag {
            Category => get_store!(self, Category) = Some(Vec::new()),
            Object => get_store!(self, Object) = Some(Vec::new()),
            Morphism => get_store!(self, Morphism) = Some(Vec::new()),
            Functor => get_store!(self, Functor) = Some(Vec::new()),
            Equality => get_store!(self, Equality) = Some(Vec::new()),
            AppliedFunctObj => get_store!(self, AppliedFunctObj) = Some(Vec::new()),
            Identity => get_store!(self, Identity) = Some(Vec::new()),
            ComposeMph => get_store!(self, ComposeMph) = Some(Vec::new()),
            AppliedFunctMph => get_store!(self, AppliedFunctMph) = Some(Vec::new()),
            Reflexivity => get_store!(self, Reflexivity) = Some(Vec::new()),
            Concat => get_store!(self, Concat) = Some(Vec::new()),
            InverseEq => get_store!(self, InverseEq) = Some(Vec::new()),
            ComposeEq => get_store!(self, ComposeEq) = Some(Vec::new()),
            Associativity => get_store!(self, Associativity) = Some(Vec::new()),
            LeftUnitality => get_store!(self, LeftUnitality) = Some(Vec::new()),
            RightUnitality => get_store!(self, RightUnitality) = Some(Vec::new()),
            LeftApplication => get_store!(self, LeftApplication) = Some(Vec::new()),
            RightApplication => get_store!(self, RightApplication) = Some(Vec::new()),
            FunctIdentity => get_store!(self, FunctIdentity) = Some(Vec::new()),
            FunctComposition => get_store!(self, FunctComposition) = Some(Vec::new()),
            AppliedFunctEq => get_store!(self, AppliedFunctEq) = Some(Vec::new()),
        }
    }

    pub fn store(&mut self, feat: Feature) {
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

    pub fn query(&self, tag: Tag) -> Option<Vec<Feature>> {
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

    pub fn is_cat(&self) -> Option<Option<()>> {
        let v = self.query(Tag::Category)?;
        Some(v.into_iter().find_map(|feat| {
            if let Feature::Category = feat {
                Some(())
            } else {
                None
            }
        }))
    }

    pub fn is_obj<F>(&self, qcat: u64, mut repr: F) -> Option<Option<()>>
    where
        F: FnMut(u64) -> u64,
    {
        let v = self.query(Tag::Object)?;
        Some(v.into_iter().find_map(|feat| {
            if let Feature::Object { cat } = feat {
                if repr(cat) == repr(qcat) {
                    Some(())
                } else {
                    None
                }
            } else {
                None
            }
        }))
    }

    pub fn is_funct<F>(&self, qdcat: u64, mut repr: F) -> Option<Option<u64>>
    where
        F: FnMut(u64) -> u64,
    {
        let v = self.query(Tag::Functor)?;
        Some(v.into_iter().find_map(|feat| {
            if let Feature::Functor { scat, dcat } = feat {
                if repr(dcat) == repr(qdcat) {
                    Some(scat)
                } else {
                    None
                }
            } else {
                None
            }
        }))
    }

    pub fn is_mph<F>(&self, qcat: u64, mut repr: F) -> Option<Option<(u64, u64)>>
    where
        F: FnMut(u64) -> u64,
    {
        let v = self.query(Tag::Morphism)?;
        Some(v.into_iter().find_map(|feat| {
            if let Feature::Morphism { cat, src, dst } = feat {
                if repr(cat) == repr(qcat) {
                    Some((src, dst))
                } else {
                    None
                }
            } else {
                None
            }
        }))
    }

    pub fn is_eq<F>(&self, qcat: u64, mut repr: F) -> Option<Option<(u64, u64, u64, u64)>>
    where
        F: FnMut(u64) -> u64,
    {
        let v = self.query(Tag::Equality)?;
        Some(v.into_iter().find_map(|feat| {
            if let Feature::Equality {
                cat,
                src,
                dst,
                left,
                right,
            } = feat
            {
                if repr(cat) == repr(qcat) {
                    Some((src, dst, left, right))
                } else {
                    None
                }
            } else {
                None
            }
        }))
    }

    pub fn is_funct_obj<F>(&self, qdcat: u64, mut repr: F) -> Option<Option<(u64, u64, u64)>>
    where
        F: FnMut(u64) -> u64,
    {
        let v = self.query(Tag::AppliedFunctObj)?;
        Some(v.into_iter().find_map(|feat| {
            if let Feature::AppliedFunctObj {
                scat,
                dcat,
                funct,
                obj,
            } = feat
            {
                if repr(qdcat) == repr(dcat) {
                    Some((scat, funct, obj))
                } else {
                    None
                }
            } else {
                None
            }
        }))
    }

    pub fn is_identity<F>(&self, qcat: u64, mut repr: F) -> Option<Option<u64>>
    where
        F: FnMut(u64) -> u64,
    {
        let v = self.query(Tag::Identity)?;
        Some(v.into_iter().find_map(|feat| {
            if let Feature::Identity { cat, obj } = feat {
                if repr(qcat) == repr(cat) {
                    Some(obj)
                } else {
                    None
                }
            } else {
                None
            }
        }))
    }

    pub fn is_comp<F>(&self, qcat: u64, mut repr: F) -> Option<Option<(u64, u64, u64, u64, u64)>>
    where
        F: FnMut(u64) -> u64,
    {
        let v = self.query(Tag::ComposeMph)?;
        Some(v.into_iter().find_map(|feat| {
            if let Feature::ComposeMph {
                cat,
                src,
                mid,
                dst,
                m1,
                m2,
            } = feat
            {
                if repr(qcat) == repr(cat) {
                    Some((src, mid, dst, m1, m2))
                } else {
                    None
                }
            } else {
                None
            }
        }))
    }

    pub fn is_funct_mph<F>(
        &self,
        qdcat: u64,
        mut repr: F,
    ) -> Option<Option<(u64, u64, u64, u64, u64)>>
    where
        F: FnMut(u64) -> u64,
    {
        let v = self.query(Tag::AppliedFunctMph)?;
        Some(v.into_iter().find_map(|feat| {
            if let Feature::AppliedFunctMph {
                scat,
                dcat,
                funct,
                src,
                dst,
                mph,
            } = feat
            {
                if repr(qdcat) == repr(dcat) {
                    Some((scat, funct, src, dst, mph))
                } else {
                    None
                }
            } else {
                None
            }
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Feature, Tag};
    use super::{QueryCache, Store};

    #[test]
    fn cache() {
        let repr = |id: u64| -> u64 { id };
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
        cache.store_none(Tag::AppliedFunctMph);
        assert_eq!(cache.query(Tag::Category), None);
        assert_eq!(cache.query(Tag::ComposeMph).map(|v| v.len()), Some(2));
        assert_eq!(cache.is_comp(0, repr), Some(Some((1, 2, 3, 4, 5))));
        assert_eq!(cache.is_comp(3, repr), Some(None));
        assert_eq!(cache.is_comp(6, repr), Some(Some((1, 2, 3, 4, 5))));
        assert_eq!(cache.is_identity(0, repr), None);
        assert_eq!(cache.is_funct_mph(0, repr), Some(None));
    }

    #[test]
    fn store() {
        type ES = super::EvarStatus;
        let repr = |id: u64| -> u64 { id };
        let mut store = Store::new();
        store.register(0, "C".to_string(), None, ES::Grounded);
        store.get_mut(0).unwrap().cache.store(Feature::Category);
        store.register(1, "?x".to_string(), None, ES::Evar);
        store
            .get_mut(1)
            .unwrap()
            .cache
            .store(Feature::Object { cat: 0 });
        assert!(store.get(0).is_some());
        assert_eq!(store.get(1).map(|o| o.status), Some(ES::Evar));
        assert_eq!(store.get(1).map(|o| &o.label[..]), Some("?x"));
        assert!(store.get(2).is_none());

        store.push_state();
        store.register(1, "a".to_string(), None, ES::Grounded);
        store.register(2, "1_a".to_string(), None, ES::Grounded);
        store.get_mut(2).unwrap().cache.store(Feature::Morphism {
            cat: 0,
            src: 1,
            dst: 1,
        });
        store
            .get_mut(2)
            .unwrap()
            .cache
            .store(Feature::Identity { cat: 0, obj: 1 });
        assert_eq!(store.get(1).map(|o| o.status), Some(ES::Grounded));
        assert_eq!(
            store.get(2).unwrap().cache.is_identity(0, repr),
            Some(Some(1))
        );

        store.push_state();
        store.register(1, "a".to_string(), Some("a".to_string()), ES::Grounded);
        assert_eq!(
            store.get(1).map(|o| o.name.clone()),
            Some(Some("a".to_string()))
        );

        store.pop_state();
        assert_eq!(store.get(1).map(|o| o.name.clone()), Some(None));
        assert_eq!(
            store.get(2).unwrap().cache.is_identity(0, repr),
            Some(Some(1))
        );

        store.pop_state();
        assert_eq!(store.get(1).map(|o| o.status), Some(ES::Evar));
        assert!(store.get(2).is_none());
    }
}
