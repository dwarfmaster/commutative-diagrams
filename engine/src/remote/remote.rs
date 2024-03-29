use crate::data::{EvarStatus, Feature, Tag};
use crate::graph::GraphParsed;
use std::fmt::Debug;

// Remote is a trait to enable mocking for test purposes
pub trait Remote {
    type Error: Debug;

    // Query the goal from the proof assistant
    fn goal<NL, EL, FL>(&mut self) -> Result<GraphParsed<NL, EL, FL>, Self::Error>
    where
        NL: Default,
        EL: Default,
        FL: Default;
    fn info(&mut self, obj: u64) -> Result<(String, Option<String>, EvarStatus), Self::Error>;
    fn repr(&mut self, obj: u64) -> Result<u64, Self::Error>;
    fn unify<I>(&mut self, pairs: I) -> Result<bool, Self::Error>
    where
        I: Iterator<Item = (u64, u64)> + ExactSizeIterator + Clone;
    fn equalify(&mut self, obj1: u64, obj2: u64) -> Result<bool, Self::Error>;
    fn lemmas(&mut self) -> Result<Vec<(u64, String, Vec<String>)>, Self::Error>;
    fn instantiate<NL, EL, FL>(&mut self, lem: u64) -> Result<GraphParsed<NL, EL, FL>, Self::Error>
    where
        NL: Default,
        EL: Default,
        FL: Default;
    fn pattern<NL, EL, FL>(&mut self, lem: u64) -> Result<GraphParsed<NL, EL, FL>, Self::Error>
    where
        NL: Default,
        EL: Default,
        FL: Default;
    fn query(&mut self, obj: u64, tag: Tag) -> Result<Vec<Feature>, Self::Error>;
    fn build(&mut self, feat: Feature) -> Result<u64, Self::Error>;
    fn parse(&mut self, str: String) -> Result<Result<u64, String>, Self::Error>;
    fn save_state(&mut self) -> Result<u64, Self::Error>;
    fn restore_state(&mut self, state: u64) -> Result<(), Self::Error>;
    fn finish(&mut self, success: bool) -> Result<(), Self::Error>;

    // Set the lemma context
    fn set_lem_context(&mut self, lem: u64);
    fn unset_lem_context(&mut self);
}
