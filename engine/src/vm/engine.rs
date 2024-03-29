use super::store::Context;
use crate::remote::{Remote, TermEngine};

impl<Rm: Remote> TermEngine for Context<Rm> {
    type R = Rm;

    fn remote<'a>(&'a mut self) -> &'a mut Self::R {
        &mut self.remote
    }

    fn get_name(&mut self, obj: u64) -> Option<String> {
        self.get_stored_name(obj)
    }

    fn get_label(&mut self, obj: u64) -> String {
        self.get_stored_label(obj)
    }

    fn get_status(&mut self, obj: u64) -> crate::data::EvarStatus {
        self.get_stored_status(obj)
    }

    fn get_repr(&mut self, obj: u64) -> u64 {
        self.get_stored_repr(obj)
    }

    fn is_cat(&mut self, obj: u64) -> bool {
        self.is_cat(obj)
    }

    fn is_obj(&mut self, obj: u64, cat: u64) -> bool {
        self.is_obj(obj, cat)
    }

    fn is_funct(&mut self, obj: u64, cat: u64) -> Option<u64> {
        self.is_funct(obj, cat)
    }

    fn is_mph(&mut self, obj: u64, cat: u64) -> Option<(u64, u64)> {
        self.is_mph(obj, cat)
    }

    fn is_eq(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64)> {
        self.is_eq(obj, cat)
    }

    fn is_funct_obj(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64)> {
        self.is_funct_obj(obj, cat)
    }

    fn is_identity(&mut self, obj: u64, cat: u64) -> Option<u64> {
        self.is_identity(obj, cat)
    }

    fn is_comp(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        self.is_comp(obj, cat)
    }

    fn is_funct_mph(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)> {
        self.is_funct_mph(obj, cat)
    }
}
