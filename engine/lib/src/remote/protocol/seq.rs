use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};

pub struct Seq<I> {
    iter: I,
}

impl<I> Seq<I>
where
    I: Iterator<Item = (u64, u64)> + ExactSizeIterator + Clone,
{
    pub fn new(iter: I) -> Self {
        Self { iter }
    }
}

impl<I> Serialize for Seq<I>
where
    I: Iterator<Item = (u64, u64)> + ExactSizeIterator + Clone,
{
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = s.serialize_seq(Some(self.iter.len()))?;
        let () = self
            .iter
            .clone()
            .map(|it| seq.serialize_element(&it))
            .collect::<Result<(), S::Error>>()?;
        seq.end()
    }
}
