use serde::de::{Error, Unexpected, Visitor};
use serde::{Deserialize, Deserializer, Serialize};

pub struct ParseResult {
    content: Result<u64, String>,
}
impl ParseResult {
    pub fn unpack(self) -> Result<u64, String> {
        self.content
    }
}

macro_rules! make_signed_visitor {
    ($name:ident, $t:ty) => {
        fn $name<E: Error>(self, v: $t) -> Result<Self::Value, E> {
            if v < 0 {
                Err(E::invalid_value(
                    Unexpected::Signed(v as i64),
                    &"a positive number",
                ))
            } else {
                Ok(ParseResult {
                    content: Ok(v as u64),
                })
            }
        }
    };
}
macro_rules! make_unsigned_visitor {
    ($name:ident, $t:ty) => {
        fn $name<E>(self, v: $t) -> Result<Self::Value, E> {
            Ok(ParseResult {
                content: Ok(v as u64),
            })
        }
    };
}

struct ParseResultVisitor {}
impl<'de> Visitor<'de> for ParseResultVisitor {
    type Value = ParseResult;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "a string or a number")
    }

    make_signed_visitor!(visit_i8, i8);
    make_signed_visitor!(visit_i16, i16);
    make_signed_visitor!(visit_i32, i32);
    make_signed_visitor!(visit_i64, i64);
    make_unsigned_visitor!(visit_u8, u8);
    make_unsigned_visitor!(visit_u16, u16);
    make_unsigned_visitor!(visit_u32, u32);
    make_unsigned_visitor!(visit_u64, u64);

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> {
        Ok(ParseResult {
            content: Err(v.to_string()),
        })
    }
}

impl<'de> Deserialize<'de> for ParseResult {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(ParseResultVisitor {})
    }
}

impl Serialize for ParseResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match &self.content {
            Ok(v) => serializer.serialize_u64(*v),
            Err(str) => serializer.serialize_str(str),
        }
    }
}
