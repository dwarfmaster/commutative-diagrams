use serde::de::Visitor;
use serde::{Deserialize, Deserializer};

pub struct ParseResult {
    content: Result<u64, String>,
}
impl ParseResult {
    pub fn unpack(self) -> Result<u64, String> {
        self.content
    }
}

struct ParseResultVisitor {}
impl<'de> Visitor<'de> for ParseResultVisitor {
    type Value = ParseResult;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "a string or a number")
    }

    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E> {
        Ok(ParseResult { content: Ok(v) })
    }

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
