use crate::data::{EvarStatus, Feature, Obj, Tag};
use crate::remote::Remote;
use crate::vm::{Interactive, VM};

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    fn query_info<'a>(&'a mut self, obj: u64) -> &'a Obj {
        let (label, name, status) = self.remote.info(obj).unwrap();
        self.store.register(obj, label, name, status)
    }

    pub fn get_stored_label(&mut self, obj: u64) -> String {
        let data = self.store.get(obj);
        match data {
            Some(obj) => obj.label.clone(),
            None => self.query_info(obj).label.clone(),
        }
    }

    pub fn get_stored_name(&mut self, obj: u64) -> Option<String> {
        let data = self.store.get(obj);
        match data {
            Some(obj) => obj.name.clone(),
            None => self.query_info(obj).name.clone(),
        }
    }

    pub fn get_stored_status(&mut self, obj: u64) -> EvarStatus {
        let data = self.store.get(obj);
        match data {
            Some(obj) => obj.status,
            None => self.query_info(obj).status,
        }
    }

    fn do_query(&mut self, obj: u64, tag: Tag) -> Vec<Feature> {
        if self.store.get(obj).is_none() {
            self.query_info(obj);
        }
        let feats = self.remote.query(obj, tag).unwrap();
        if feats.is_empty() {
            self.store.store_none(obj, tag);
        } else {
            feats
                .iter()
                .for_each(|feat| self.store.store(obj, feat.clone()));
        }
        feats
    }

    pub fn get_stored_query(&mut self, obj: u64, tag: Tag) -> Vec<Feature> {
        let data = self.store.query(obj, tag);
        match data {
            Some(feats) => feats,
            None => self.do_query(obj, tag),
        }
    }

    pub fn is_cat(&mut self, obj: u64) -> bool {
        let data = self.store.is_cat(obj);
        match data {
            Some(cat) => cat.is_some(),
            None => {
                self.do_query(obj, Tag::Category);
                self.store.is_cat(obj).unwrap().is_some()
            }
        }
    }

    pub fn is_obj(&mut self, obj: u64, cat: u64) -> bool {
        let data = self.store.is_obj(obj, cat);
        match data {
            Some(obj) => obj.is_some(),
            None => {
                self.do_query(obj, Tag::Object);
                self.store.is_obj(obj, cat).unwrap().is_some()
            }
        }
    }

    pub fn is_funct(&mut self, obj: u64, cat: u64) -> Option<u64> {
        let data = self.store.is_funct(obj, cat);
        match data {
            Some(f) => f,
            None => {
                self.do_query(obj, Tag::Functor);
                self.store.is_funct(obj, cat).unwrap()
            }
        }
    }

    pub fn is_mph(&mut self, obj: u64, cat: u64) -> Option<(u64, u64)> {
        let data = self.store.is_mph(obj, cat);
        match data {
            Some(mph) => mph,
            None => {
                self.do_query(obj, Tag::Morphism);
                self.store.is_mph(obj, cat).unwrap()
            }
        }
    }

    pub fn is_eq(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64)> {
        let data = self.store.is_eq(obj, cat);
        match data {
            Some(eq) => eq,
            None => {
                self.do_query(obj, Tag::Equality);
                self.store.is_eq(obj, cat).unwrap()
            }
        }
    }

    pub fn is_funct_obj(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64)> {
        let data = self.store.is_funct_obj(obj, cat);
        match data {
            Some(fobj) => fobj,
            None => {
                self.do_query(obj, Tag::AppliedFunctObj);
                self.store.is_funct_obj(obj, cat).unwrap()
            }
        }
    }

    pub fn is_identity(&mut self, obj: u64, cat: u64) -> Option<u64> {
        let data = self.store.is_identity(obj, cat);
        match data {
            Some(id) => id,
            None => {
                self.do_query(obj, Tag::Identity);
                self.store.is_identity(obj, cat).unwrap()
            }
        }
    }

    pub fn is_comp(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        let data = self.store.is_comp(obj, cat);
        match data {
            Some(comp) => comp,
            None => {
                self.do_query(obj, Tag::ComposeMph);
                self.store.is_comp(obj, cat).unwrap()
            }
        }
    }

    pub fn is_funct_mph(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        let data = self.store.is_funct_mph(obj, cat);
        match data {
            Some(fmph) => fmph,
            None => {
                self.do_query(obj, Tag::AppliedFunctMph);
                self.store.is_funct_mph(obj, cat).unwrap()
            }
        }
    }
}
