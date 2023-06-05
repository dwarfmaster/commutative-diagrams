use crate::data::EvarStatus;
use serde::de::{Error, Unexpected};
use serde::ser::Serializer;
use serde::{Deserialize, Serialize};

impl Serialize for EvarStatus {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use EvarStatus::*;
        match self {
            Evar => s.serialize_u8(0),
            Partial => s.serialize_u8(1),
            Grounded => s.serialize_u8(2),
        }
    }
}

impl<'de> Deserialize<'de> for EvarStatus {
    fn deserialize<D>(d: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use EvarStatus::*;
        let id = u8::deserialize(d)?;
        match id {
            0 => Ok(Evar),
            1 => Ok(Partial),
            2 => Ok(Grounded),
            _ => Err(Error::invalid_value(
                Unexpected::Unsigned(id as u64),
                &"0, 1 or 2",
            )),
        }
    }
}
