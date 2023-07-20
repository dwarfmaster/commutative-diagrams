use crate::data::{EvarStatus, Feature, Obj, Store, Tag};
use crate::remote::Remote;
use std::collections::HashMap;

pub struct Context<Rm: Remote> {
    pub remote: Rm,
    // Map from lemmas namespaces to associated stores
    pub store: HashMap<u64, Store>,
    pub states: Vec<u64>,
    pub context: u64,
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
        let feats = self.remote.query(obj, tag).unwrap();
        if feats.is_empty() {
            self.store
                .get_mut(&self.context)
                .unwrap()
                .store_none(obj, tag);
        } else {
            feats.iter().for_each(|feat| {
                self.store
                    .get_mut(&self.context)
                    .unwrap()
                    .store(obj, feat.clone())
            });
        }
        feats
    }

    pub fn get_stored_query(&mut self, obj: u64, tag: Tag) -> Vec<Feature> {
        let data = self.store.get(&self.context).unwrap().query(obj, tag);
        match data {
            Some(feats) => feats,
            None => self.do_query(obj, tag),
        }
    }

    pub fn is_cat(&mut self, obj: u64) -> bool {
        let data = self.store.get(&self.context).unwrap().is_cat(obj);
        match data {
            Some(cat) => cat.is_some(),
            None => {
                self.do_query(obj, Tag::Category);
                self.store
                    .get(&self.context)
                    .unwrap()
                    .is_cat(obj)
                    .unwrap()
                    .is_some()
            }
        }
    }

    pub fn is_obj(&mut self, obj: u64, cat: u64) -> bool {
        let data = self.store.get(&self.context).unwrap().is_obj(obj, cat);
        match data {
            Some(obj) => obj.is_some(),
            None => {
                self.do_query(obj, Tag::Object);
                self.store
                    .get(&self.context)
                    .unwrap()
                    .is_obj(obj, cat)
                    .unwrap()
                    .is_some()
            }
        }
    }

    pub fn is_funct(&mut self, obj: u64, cat: u64) -> Option<u64> {
        let data = self.store.get(&self.context).unwrap().is_funct(obj, cat);
        match data {
            Some(f) => f,
            None => {
                self.do_query(obj, Tag::Functor);
                self.store
                    .get(&self.context)
                    .unwrap()
                    .is_funct(obj, cat)
                    .unwrap()
            }
        }
    }

    pub fn is_mph(&mut self, obj: u64, cat: u64) -> Option<(u64, u64)> {
        let data = self.store.get(&self.context).unwrap().is_mph(obj, cat);
        match data {
            Some(mph) => mph,
            None => {
                self.do_query(obj, Tag::Morphism);
                self.store
                    .get(&self.context)
                    .unwrap()
                    .is_mph(obj, cat)
                    .unwrap()
            }
        }
    }

    pub fn is_eq(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64)> {
        let data = self.store.get(&self.context).unwrap().is_eq(obj, cat);
        match data {
            Some(eq) => eq,
            None => {
                self.do_query(obj, Tag::Equality);
                self.store
                    .get(&self.context)
                    .unwrap()
                    .is_eq(obj, cat)
                    .unwrap()
            }
        }
    }

    pub fn is_funct_obj(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64)> {
        let data = self
            .store
            .get(&self.context)
            .unwrap()
            .is_funct_obj(obj, cat);
        match data {
            Some(fobj) => fobj,
            None => {
                self.do_query(obj, Tag::AppliedFunctObj);
                self.store
                    .get(&self.context)
                    .unwrap()
                    .is_funct_obj(obj, cat)
                    .unwrap()
            }
        }
    }

    pub fn is_identity(&mut self, obj: u64, cat: u64) -> Option<u64> {
        let data = self.store.get(&self.context).unwrap().is_identity(obj, cat);
        match data {
            Some(id) => id,
            None => {
                self.do_query(obj, Tag::Identity);
                self.store
                    .get(&self.context)
                    .unwrap()
                    .is_identity(obj, cat)
                    .unwrap()
            }
        }
    }

    pub fn is_comp(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        let data = self.store.get(&self.context).unwrap().is_comp(obj, cat);
        match data {
            Some(comp) => comp,
            None => {
                self.do_query(obj, Tag::ComposeMph);
                self.store
                    .get(&self.context)
                    .unwrap()
                    .is_comp(obj, cat)
                    .unwrap()
            }
        }
    }

    pub fn is_funct_mph(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        let data = self
            .store
            .get(&self.context)
            .unwrap()
            .is_funct_mph(obj, cat);
        match data {
            Some(fmph) => fmph,
            None => {
                self.do_query(obj, Tag::AppliedFunctMph);
                self.store
                    .get(&self.context)
                    .unwrap()
                    .is_funct_mph(obj, cat)
                    .unwrap()
            }
        }
    }
}
