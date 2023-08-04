use crate::data::{EvarStatus, Feature, Obj, QueryCache, Store, Tag};
use crate::remote::Remote;
use std::collections::HashMap;

pub struct Context<Rm: Remote> {
    pub remote: Rm,
    // Map from lemmas namespaces to associated stores
    pub store: HashMap<u64, Store>,
    pub states: Vec<u64>,
    pub context: u64,
}

macro_rules! is_feat {
    ($ctx:expr, $cat:expr, $obj:expr, $tag:expr, $f:ident) => {{
        $ctx.do_query($obj, $tag);
        let mut cache = QueryCache::new();
        $ctx.swap_cache($obj, &mut cache);
        let repr = |id: u64| -> u64 { $ctx.get_stored_repr(id) };
        let r = cache.$f($cat, repr).unwrap();
        $ctx.swap_cache($obj, &mut cache);
        r
    }};
}

impl<Rm: Remote> Context<Rm> {
    pub fn new(rm: Rm) -> Self {
        let mut hash = HashMap::new();
        hash.insert(0, Store::new());
        Self {
            remote: rm,
            store: hash,
            states: Vec::new(),
            context: 0,
        }
    }

    pub fn set_lem_context(&mut self, lem: u64) {
        self.store.entry(lem).or_insert_with(|| Store::new());
        self.remote.set_lem_context(lem);
        self.context = lem;
    }

    pub fn unset_lem_context(&mut self) {
        self.remote.unset_lem_context();
        self.context = 0;
    }

    pub fn save_state(&mut self) -> u64 {
        let state = self.remote.save_state().unwrap();
        self.states.push(state);
        self.store.get_mut(&0).unwrap().push_state();
        state
    }

    pub fn restore_state(&mut self, state: u64) {
        self.remote.restore_state(state).unwrap();
        let pos = self.states.iter().position(|v| *v == state);
        if let Some(pos) = pos {
            let d = self.states.len() - pos;
            self.states.truncate(pos + 1);
            let store = self.store.get_mut(&0).unwrap();
            for _ in 0..d {
                store.pop_state();
            }
        }
    }

    fn query_info<'a>(&'a mut self, obj: u64) -> &'a Obj {
        let (label, name, status) = self.remote.info(obj).unwrap();
        self.store
            .get_mut(&self.context)
            .unwrap()
            .register(obj, label, name, status)
    }

    pub fn get_stored_label(&mut self, obj: u64) -> String {
        let data = self.store.get(&self.context).unwrap().get(obj);
        match data {
            Some(obj) => obj.label.clone(),
            None => self.query_info(obj).label.clone(),
        }
    }

    pub fn get_stored_name(&mut self, obj: u64) -> Option<String> {
        let data = self.store.get(&self.context).unwrap().get(obj);
        match data {
            Some(obj) => obj.name.clone(),
            None => self.query_info(obj).name.clone(),
        }
    }

    pub fn get_stored_status(&mut self, obj: u64) -> EvarStatus {
        let data = self.store.get(&self.context).unwrap().get(obj);
        match data {
            Some(obj) => obj.status,
            None => self.query_info(obj).status,
        }
    }

    pub fn get_stored_repr(&mut self, id: u64) -> u64 {
        let data = self.store.get(&self.context).unwrap().get(id);
        match data {
            Some(obj) => match obj.repr {
                Some(repr) => repr,
                None => {
                    let repr = self.remote.repr(id).unwrap();
                    self.store
                        .get_mut(&self.context)
                        .unwrap()
                        .register_repr(id, repr);
                    repr
                }
            },
            None => {
                self.query_info(id);
                let repr = self.remote.repr(id).unwrap();
                self.store
                    .get_mut(&self.context)
                    .unwrap()
                    .register_repr(id, repr);
                repr
            }
        }
    }

    fn do_query(&mut self, obj: u64, tag: Tag) -> Vec<Feature> {
        if self.store.get(&self.context).unwrap().get(obj).is_none() {
            self.query_info(obj);
        }
        let cache = &mut self
            .store
            .get_mut(&self.context)
            .unwrap()
            .get_mut(obj)
            .unwrap()
            .cache;
        match cache.query(tag) {
            Some(feats) => feats,
            None => {
                let feats = self.remote.query(obj, tag).unwrap();
                if feats.is_empty() {
                    cache.store_none(tag);
                } else {
                    feats.iter().for_each(|feat| cache.store(feat.clone()));
                }
                feats
            }
        }
    }

    pub fn get_stored_query(&mut self, obj: u64, tag: Tag) -> Vec<Feature> {
        self.do_query(obj, tag)
    }

    fn swap_cache(&mut self, obj: u64, cache: &mut QueryCache) {
        std::mem::swap(
            cache,
            &mut self
                .store
                .get_mut(&self.context)
                .unwrap()
                .get_mut(obj)
                .unwrap()
                .cache,
        );
    }

    pub fn is_cat(&mut self, obj: u64) -> bool {
        self.do_query(obj, Tag::Category);
        self.store
            .get(&self.context)
            .unwrap()
            .get(obj)
            .unwrap()
            .cache
            .is_cat()
            .unwrap()
            .is_some()
    }

    pub fn is_obj(&mut self, obj: u64, cat: u64) -> bool {
        is_feat!(self, cat, obj, Tag::Object, is_funct).is_some()
    }

    pub fn is_funct(&mut self, obj: u64, cat: u64) -> Option<u64> {
        is_feat!(self, cat, obj, Tag::Functor, is_funct)
    }

    pub fn is_mph(&mut self, obj: u64, cat: u64) -> Option<(u64, u64)> {
        is_feat!(self, cat, obj, Tag::Morphism, is_mph)
    }

    pub fn is_eq(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64)> {
        is_feat!(self, cat, obj, Tag::Equality, is_eq)
    }

    pub fn is_funct_obj(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64)> {
        is_feat!(self, cat, obj, Tag::AppliedFunctObj, is_funct_obj)
    }

    pub fn is_identity(&mut self, obj: u64, cat: u64) -> Option<u64> {
        is_feat!(self, cat, obj, Tag::Identity, is_identity)
    }

    pub fn is_comp(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        is_feat!(self, cat, obj, Tag::ComposeMph, is_comp)
    }

    pub fn is_funct_mph(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        is_feat!(self, cat, obj, Tag::AppliedFunctMph, is_funct_mph)
    }
}
