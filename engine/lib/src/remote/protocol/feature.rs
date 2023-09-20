use crate::data::{Feature, Tag};
use serde::de::{Error, SeqAccess, Visitor};
use serde::ser::SerializeSeq;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

impl Serialize for Feature {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let tag = self.tag();
        let args = self.iter();
        let mut seq = s.serialize_seq(Some(1 + args.len()))?;
        seq.serialize_element(&tag)?;
        args.map(|arg| seq.serialize_element(&arg))
            .collect::<Result<(), S::Error>>()?;
        seq.end()
    }
}

struct FeatureDeserializer {}
struct ArgsIterator<A> {
    access: A,
}
impl<'de, A> Iterator for ArgsIterator<A>
where
    A: SeqAccess<'de>,
{
    type Item = u64;

    fn next(&mut self) -> Option<u64> {
        self.access.next_element().ok().flatten()
    }
}
impl<A> ArgsIterator<A> {
    fn new(access: A) -> Self {
        Self { access }
    }
}

impl<'de> Visitor<'de> for FeatureDeserializer {
    type Value = Feature;

    fn expecting(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "a feature tag followed by arguments")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let tag: Tag = seq.next_element()?.ok_or(Error::invalid_length(0, &self))?;
        let iter = ArgsIterator::new(seq);
        let res = Feature::from_iter(tag, iter);
        match res {
            Some(feat) => Ok(feat),
            None => Err(Error::invalid_length(1 /* TODO get length */, &self)),
        }
    }
}

impl<'de> Deserialize<'de> for Feature {
    fn deserialize<D>(d: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        d.deserialize_seq(FeatureDeserializer {})
    }
}
