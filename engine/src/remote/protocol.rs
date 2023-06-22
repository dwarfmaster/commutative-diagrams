mod feature;
mod graph;
mod parse;
mod seq;
mod status;
mod tag;

use super::{Error, Remote, RPC};
use crate::data::{EvarStatus, Feature, Tag};
use crate::graph::GraphParsed;
use seq::Seq;

impl<In: std::io::Read, Out: std::io::Write> Remote for RPC<In, Out> {
    type Error = Error;

    fn goal<NL, EL, FL>(&mut self) -> Result<GraphParsed<NL, EL, FL>, Self::Error>
    where
        NL: Default,
        EL: Default,
        FL: Default,
    {
        let req = self.send_msg("goal", ())?;
        self.receive_msg(req)
    }

    fn info(&mut self, obj: u64) -> Result<(String, Option<String>, EvarStatus), Self::Error> {
        let req = self.send_msg("info", obj)?;
        self.receive_msg(req)
    }

    fn unify<I>(&mut self, pairs: I) -> Result<bool, Self::Error>
    where
        I: Iterator<Item = (u64, u64)> + ExactSizeIterator + Clone,
    {
        let req = self.send_msg("unify", Seq::new(pairs))?;
        self.receive_msg(req)
    }

    fn equalify(&mut self, obj1: u64, obj2: u64) -> Result<bool, Self::Error> {
        let req = self.send_msg("equalify", [obj1, obj2])?;
        self.receive_msg(req)
    }

    fn lemmas(&mut self) -> Result<Vec<(u64, String, String)>, Self::Error> {
        let req = self.send_msg("lemmas", ())?;
        self.receive_msg(req)
    }

    fn instantiate<NL, EL, FL>(&mut self, lem: u64) -> Result<GraphParsed<NL, EL, FL>, Self::Error>
    where
        NL: Default,
        EL: Default,
        FL: Default,
    {
        let req = self.send_msg("instantiate", lem)?;
        self.receive_msg(req)
    }

    fn query(&mut self, obj: u64, tag: Tag) -> Result<Vec<Feature>, Self::Error> {
        let req = self.send_msg("query", (obj, tag))?;
        self.receive_msg(req)
    }

    fn build(&mut self, feat: Feature) -> Result<u64, Self::Error> {
        let req = self.send_msg("build", feat)?;
        self.receive_msg(req)
    }

    fn parse(&mut self, str: String) -> Result<Result<u64, String>, Self::Error> {
        let req = self.send_msg("parse", str)?;
        self.receive_msg(req)
            .map(|pr: parse::ParseResult| pr.unpack())
    }

    fn save_state(&mut self) -> Result<u64, Self::Error> {
        let req = self.send_msg("save_state", ())?;
        self.receive_msg(req)
    }

    fn restore_state(&mut self, state: u64) -> Result<(), Self::Error> {
        let req = self.send_msg("restore_state", state)?;
        self.receive_msg(req)
    }

    fn finish(&mut self, success: bool) -> Result<(), Self::Error> {
        let req = self.send_msg("finish", success)?;
        self.receive_msg(req)
    }
}
