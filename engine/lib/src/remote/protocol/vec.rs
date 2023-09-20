use core::marker::PhantomData;
use serde::de::{SeqAccess, Visitor};
use serde::ser::SerializeSeq;
use serde::{Deserialize, Serialize};

pub struct VecWrapper<T> {
    pub wrapped_vec: Vec<T>,
}
struct Visit<T> {
    phantom: PhantomData<T>,
}

impl<'de, T: Deserialize<'de>> Visitor<'de> for Visit<T> {
    type Value = VecWrapper<T>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "an array")
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E> {
        Ok(VecWrapper {
            wrapped_vec: Vec::new(),
        })
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut res = Vec::new();
        while let Some(v) = seq.next_element()? {
            res.push(v)
        }
        Ok(VecWrapper { wrapped_vec: res })
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for VecWrapper<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_seq(Visit {
            phantom: PhantomData,
        })
    }
}

impl<T: Serialize> Serialize for VecWrapper<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.wrapped_vec.len()))?;
        for elem in &self.wrapped_vec {
            seq.serialize_element(elem)?;
        }
        seq.end()
    }
}
