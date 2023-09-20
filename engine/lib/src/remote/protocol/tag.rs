use crate::data::Tag;
use serde::de::{Error, Expected, Unexpected};
use serde::{Deserialize, Serialize, Serializer};
use std::fmt;

impl Serialize for Tag {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use Tag::*;
        match self {
            Category => s.serialize_str("category"),
            Object => s.serialize_str("object"),
            Morphism => s.serialize_str("morphism"),
            Functor => s.serialize_str("functor"),
            Equality => s.serialize_str("equality"),
            AppliedFunctObj => s.serialize_str("applied_funct_obj"),
            Identity => s.serialize_str("identity"),
            ComposeMph => s.serialize_str("compose_mph"),
            AppliedFunctMph => s.serialize_str("applied_funct_mph"),
            Reflexivity => s.serialize_str("reflexivity"),
            Concat => s.serialize_str("concat"),
            InverseEq => s.serialize_str("inverse_eq"),
            ComposeEq => s.serialize_str("compose_eq"),
            Associativity => s.serialize_str("associativity"),
            LeftUnitality => s.serialize_str("left_unitality"),
            RightUnitality => s.serialize_str("right_unitality"),
            LeftApplication => s.serialize_str("left_application"),
            RightApplication => s.serialize_str("right_application"),
            FunctIdentity => s.serialize_str("funct_identity"),
            FunctComposition => s.serialize_str("funct_composition"),
            AppliedFunctEq => s.serialize_str("applied_funct_eq"),
        }
    }
}

struct TagExpecter {}
impl Expected for TagExpecter {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "one of: ")?;
        let tags: [&'static str; 21] = [
            "category",
            "object",
            "morphism",
            "functor",
            "equality",
            "applied_funct_obj",
            "identity",
            "compose_mph",
            "applied_funct_mph",
            "reflexivity",
            "concat",
            "inverse_eq",
            "compose_eq",
            "associativity",
            "left_unitality",
            "right_unitality",
            "left_application",
            "right_application",
            "funct_identity",
            "funct_composition",
            "applied_funct_eq",
        ];
        for tag in tags {
            write!(formatter, "{}, ", tag)?;
        }
        Ok(())
    }
}

impl<'de> Deserialize<'de> for Tag {
    fn deserialize<D>(d: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use Tag::*;
        let str = String::deserialize(d)?;
        match &str[..] {
            "category" => Ok(Category),
            "object" => Ok(Object),
            "morphism" => Ok(Morphism),
            "functor" => Ok(Functor),
            "equality" => Ok(Equality),
            "applied_funct_obj" => Ok(AppliedFunctObj),
            "identity" => Ok(Identity),
            "compose_mph" => Ok(ComposeMph),
            "applied_funct_mph" => Ok(AppliedFunctMph),
            "reflexivity" => Ok(Reflexivity),
            "concat" => Ok(Concat),
            "inverse_eq" => Ok(InverseEq),
            "compose_eq" => Ok(ComposeEq),
            "associativity" => Ok(Associativity),
            "left_unitality" => Ok(LeftUnitality),
            "right_unitality" => Ok(RightUnitality),
            "left_application" => Ok(LeftApplication),
            "right_application" => Ok(RightApplication),
            "funct_identity" => Ok(FunctIdentity),
            "funct_composition" => Ok(FunctComposition),
            "applied_funct_eq" => Ok(AppliedFunctEq),
            _ => Err(Error::invalid_value(Unexpected::Str(&str), &TagExpecter {})),
        }
    }
}
