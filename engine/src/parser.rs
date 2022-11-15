use crate::anyterm::AnyTerm;
use crate::data::{ActualCategory, ActualEquality, ActualFunctor, ActualMorphism, ActualObject};
use crate::data::{Category, Context, Equality, Functor, Morphism, Object, ProofObject};
use crate::data::{CategoryData, EqualityData, FunctorData, MorphismData, ObjectData};
use core::fmt;
use core::marker::PhantomData;
use serde::de::{DeserializeSeed, Error, Visitor};
use serde::de::{EnumAccess, MapAccess, SeqAccess, VariantAccess};
use serde::Deserializer;
use serde_json;

//  ____
// |  _ \ __ _ _ __ ___  ___ _ __
// | |_) / _` | '__/ __|/ _ \ '__|
// |  __/ (_| | |  \__ \  __/ |
// |_|   \__,_|_|  |___/\___|_|
//
// Parser
#[derive(Clone)]
struct Parser<T> {
    ctx: Context,
    _marker: PhantomData<T>,
}

impl<T> Parser<T> {
    fn to<U>(&self) -> Parser<U> {
        Parser {
            ctx: self.ctx.clone(),
            _marker: PhantomData::default(),
        }
    }
}

macro_rules! make_parser {
    ($f:ident, $t:ty) => {
        pub fn $f(self, str: &str) -> Result<$t, serde_json::Error> {
            let parser: Parser<$t> = Parser {
                ctx: self,
                _marker: PhantomData::default(),
            };
            let mut deserializer =
                serde_json::de::Deserializer::new(serde_json::de::StrRead::new(str));
            parser.clone().deserialize(&mut deserializer)
        }
    };
}

impl Context {
    make_parser!(parse_cat, Category);
    make_parser!(parse_funct, Functor);
    make_parser!(parse_obj, Object);
    make_parser!(parse_mph, Morphism);
    make_parser!(parse_eq, Equality);
    make_parser!(parse, AnyTerm);
}

macro_rules! deserializer_struct {
    ($t:ty) => {
        impl<'a> DeserializeSeed<'a> for Parser<$t> {
            type Value = $t;

            fn deserialize<D>(self, d: D) -> Result<Self::Value, D::Error>
            where
                D: Deserializer<'a>,
            {
                d.deserialize_map(self)
            }
        }
    };
}
macro_rules! deserializer_enum {
    ($t:ty, $n:expr, $cases:expr) => {
        impl<'a> DeserializeSeed<'a> for Parser<$t> {
            type Value = $t;

            fn deserialize<D>(self, d: D) -> Result<Self::Value, D::Error>
            where
                D: Deserializer<'a>,
            {
                d.deserialize_enum($n, $cases, self)
            }
        }
    };
}

// __     __         _             _
// \ \   / /_ _ _ __(_) __ _ _ __ | |_   _ __ ___   __ _  ___ _ __ ___
//  \ \ / / _` | '__| |/ _` | '_ \| __| | '_ ` _ \ / _` |/ __| '__/ _ \
//   \ V / (_| | |  | | (_| | | | | |_  | | | | | | (_| | (__| | | (_) |
//    \_/ \__,_|_|  |_|\__,_|_| |_|\__| |_| |_| |_|\__,_|\___|_|  \___/
//
// Variant macro

macro_rules! variant_visitor {
    ($mod:ident, $t:ident, $cons:ident, $($f:ident:$arg:ty),+) => {
        impl<'a> Visitor<'a> for Parser<parsers::$mod::$cons> {
            type Value = $t;
            fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                write!(
                    fmt,
                    "arguments for {}::{}",
                    stringify!($t),
                    stringify!($cons)
                )
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'a>
            {
                $(let $f = seq.next_element_seed(self.clone().to::<$arg>())?;)+
                match seq.next_element::<()>().unwrap_or(Some(())) {
                    Some(_) => return Err(Error::custom(
                        format!("Too many arguments for {}::{}", stringify!($t), stringify!($const)))),
                    None => ()
                }
                $(let $f = match $f {
                     Some(x) => x,
                     None => return Err(Error::custom(
                        format!("Not enough arguments for {}::{}", stringify!($t), stringify!($const))))
                 };)+
                Ok($t::$cons($($f),+))
            }
        }
    };
}

mod parsers {
    pub mod object {
        #[derive(Clone)]
        pub struct Funct {}
    }

    pub mod morphism {
        #[derive(Clone)]
        pub struct Comp {}
        #[derive(Clone)]
        pub struct Funct {}
    }

    pub mod eq {
        #[derive(Clone)]
        pub struct Concat;
        #[derive(Clone)]
        pub struct Compose;
        #[derive(Clone)]
        pub struct Assoc;
        #[derive(Clone)]
        pub struct RAp;
        #[derive(Clone)]
        pub struct LAp;
        #[derive(Clone)]
        pub struct FunctId;
        #[derive(Clone)]
        pub struct FunctComp;
        #[derive(Clone)]
        pub struct FunctCtx;
    }
}

variant_visitor!(object, ActualObject, Funct, f: Functor, o: Object);
variant_visitor!(morphism, ActualMorphism, Comp, m1: Morphism, m2: Morphism);
variant_visitor!(morphism, ActualMorphism, Funct, f: Functor, m: Morphism);
variant_visitor!(eq, ActualEquality, Concat, eq1: Equality, eq2: Equality);
variant_visitor!(eq, ActualEquality, Compose, eq1: Equality, eq2: Equality);
variant_visitor!(
    eq,
    ActualEquality,
    Assoc,
    m1: Morphism,
    m2: Morphism,
    m3: Morphism
);
variant_visitor!(eq, ActualEquality, RAp, eq: Equality, m: Morphism);
variant_visitor!(eq, ActualEquality, LAp, m: Morphism, eq: Equality);
variant_visitor!(eq, ActualEquality, FunctId, f: Functor, o: Object);
variant_visitor!(
    eq,
    ActualEquality,
    FunctComp,
    f: Functor,
    m1: Morphism,
    m2: Morphism
);
variant_visitor!(eq, ActualEquality, FunctCtx, f: Functor, eq: Equality);

//   ____      _
//  / ___|__ _| |_ ___  __ _  ___  _ __ _   _
// | |   / _` | __/ _ \/ _` |/ _ \| '__| | | |
// | |__| (_| | ||  __/ (_| | (_) | |  | |_| |
//  \____\__,_|\__\___|\__, |\___/|_|   \__, |
//                     |___/            |___/
// category
impl<'a> Visitor<'a> for Parser<CategoryData> {
    type Value = CategoryData;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "data for a category")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'a>,
    {
        let mut pobj: Option<ProofObject> = None;
        while let Some(k) = map.next_key::<String>()? {
            match k.as_str() {
                "pobj" => match pobj {
                    None => pobj = Some(map.next_value()?),
                    Some(_) => return Err(Error::duplicate_field("pobj")),
                },
                _ => return Err(Error::unknown_field(k.as_str(), &["fobj"])),
            }
        }
        let pobj = pobj.ok_or(Error::missing_field("fobj"))?;
        Ok(CategoryData { pobj })
    }
}
deserializer_struct!(CategoryData);

impl<'a> Visitor<'a> for Parser<Category> {
    type Value = Category;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "a category")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: EnumAccess<'a>,
    {
        let (id, variant) = data.variant::<String>()?;
        match id.as_str() {
            "atomic" => {
                let pobj = variant.newtype_variant_seed(self.to::<CategoryData>())?;
                let cat = ActualCategory::Atomic(pobj);
                Ok(self.ctx.mk(cat))
            }
            _ => return Err(Error::unknown_variant(id.as_str(), &["atomic"])),
        }
    }
}
deserializer_enum!(Category, "category", &["atomic"]);

//  _____                 _
// |  ___|   _ _ __   ___| |_ ___  _ __
// | |_ | | | | '_ \ / __| __/ _ \| '__|
// |  _|| |_| | | | | (__| || (_) | |
// |_|   \__,_|_| |_|\___|\__\___/|_|
//
// Functor
impl<'a> Visitor<'a> for Parser<FunctorData> {
    type Value = FunctorData;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "data for a functor")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'a>,
    {
        let mut pobj: Option<ProofObject> = None;
        let mut src: Option<Category> = None;
        let mut dst: Option<Category> = None;
        while let Some(k) = map.next_key::<String>()? {
            match k.as_str() {
                "pobj" => match pobj {
                    None => pobj = Some(map.next_value()?),
                    Some(_) => return Err(Error::duplicate_field("pobj")),
                },
                "src" => match src {
                    None => src = Some(map.next_value_seed(self.clone().to::<Category>())?),
                    Some(_) => return Err(Error::duplicate_field("src")),
                },
                "dst" => match dst {
                    None => dst = Some(map.next_value_seed(self.clone().to::<Category>())?),
                    Some(_) => return Err(Error::duplicate_field("dst")),
                },
                _ => return Err(Error::unknown_field(k.as_str(), &["fobj", "src", "dst"])),
            }
        }
        let pobj = pobj.ok_or(Error::missing_field("fobj"))?;
        let src = src.ok_or(Error::missing_field("src"))?;
        let dst = dst.ok_or(Error::missing_field("dst"))?;
        Ok(FunctorData { pobj, src, dst })
    }
}
deserializer_struct!(FunctorData);

impl<'a> Visitor<'a> for Parser<Functor> {
    type Value = Functor;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "a category")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: EnumAccess<'a>,
    {
        let (id, variant) = data.variant::<String>()?;
        match id.as_str() {
            "atomic" => {
                let pobj = variant.newtype_variant_seed(self.to::<FunctorData>())?;
                let fun = ActualFunctor::Atomic(pobj);
                Ok(self.ctx.mk(fun))
            }
            _ => return Err(Error::unknown_variant(id.as_str(), &["atomic"])),
        }
    }
}
deserializer_enum!(Functor, "functor", &["atomic"]);

//   ___  _     _           _
//  / _ \| |__ (_) ___  ___| |_
// | | | | '_ \| |/ _ \/ __| __|
// | |_| | |_) | |  __/ (__| |_
//  \___/|_.__// |\___|\___|\__|
//           |__/
// Object
impl<'a> Visitor<'a> for Parser<ObjectData> {
    type Value = ObjectData;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "data for an object")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'a>,
    {
        let mut pobj: Option<ProofObject> = None;
        let mut category: Option<Category> = None;
        while let Some(k) = map.next_key::<String>()? {
            match k.as_str() {
                "pobj" => match pobj {
                    None => pobj = Some(map.next_value()?),
                    Some(_) => return Err(Error::duplicate_field("pobj")),
                },
                "category" => match category {
                    None => category = Some(map.next_value_seed(self.clone().to::<Category>())?),
                    Some(_) => return Err(Error::duplicate_field("category")),
                },
                _ => return Err(Error::unknown_field(k.as_str(), &["fobj", "category"])),
            }
        }
        let pobj = pobj.ok_or(Error::missing_field("fobj"))?;
        let category = category.ok_or(Error::missing_field("category"))?;
        Ok(ObjectData { pobj, category })
    }
}
deserializer_struct!(ObjectData);

impl<'a> Visitor<'a> for Parser<Object> {
    type Value = Object;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "a category")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: EnumAccess<'a>,
    {
        let (id, variant) = data.variant::<String>()?;
        match id.as_str() {
            "atomic" => {
                let pobj = variant.newtype_variant_seed(self.to::<ObjectData>())?;
                let obj = ActualObject::Atomic(pobj);
                Ok(self.ctx.mk(obj))
            }
            "funct" => {
                let funct = variant.tuple_variant(2, self.to::<parsers::object::Funct>())?;
                Ok(self.ctx.mk(funct))
            }
            _ => return Err(Error::unknown_variant(id.as_str(), &["atomic", "funct"])),
        }
    }
}
deserializer_enum!(Object, "object", &["atomic", "funct"]);

//  __  __                  _     _
// |  \/  | ___  _ __ _ __ | |__ (_)___ _ __ ___
// | |\/| |/ _ \| '__| '_ \| '_ \| / __| '_ ` _ \
// | |  | | (_) | |  | |_) | | | | \__ \ | | | | |
// |_|  |_|\___/|_|  | .__/|_| |_|_|___/_| |_| |_|
//                   |_|
// Morphism
impl<'a> Visitor<'a> for Parser<MorphismData> {
    type Value = MorphismData;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "data for a morphism")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'a>,
    {
        let mut pobj: Option<ProofObject> = None;
        let mut category: Option<Category> = None;
        let mut src: Option<Object> = None;
        let mut dst: Option<Object> = None;
        while let Some(k) = map.next_key::<String>()? {
            match k.as_str() {
                "pobj" => match pobj {
                    None => pobj = Some(map.next_value()?),
                    Some(_) => return Err(Error::duplicate_field("pobj")),
                },
                "category" => match category {
                    None => category = Some(map.next_value_seed(self.clone().to::<Category>())?),
                    Some(_) => return Err(Error::duplicate_field("category")),
                },
                "src" => match src {
                    None => src = Some(map.next_value_seed(self.clone().to::<Object>())?),
                    Some(_) => return Err(Error::duplicate_field("src")),
                },
                "dst" => match dst {
                    None => dst = Some(map.next_value_seed(self.clone().to::<Object>())?),
                    Some(_) => return Err(Error::duplicate_field("dst")),
                },
                _ => {
                    return Err(Error::unknown_field(
                        k.as_str(),
                        &["fobj", "category", "src", "dst"],
                    ))
                }
            }
        }
        let pobj = pobj.ok_or(Error::missing_field("fobj"))?;
        let category = category.ok_or(Error::missing_field("category"))?;
        let src = src.ok_or(Error::missing_field("src"))?;
        let dst = dst.ok_or(Error::missing_field("dst"))?;
        Ok(MorphismData {
            pobj,
            category,
            src,
            dst,
        })
    }
}
deserializer_struct!(MorphismData);

impl<'a> Visitor<'a> for Parser<Morphism> {
    type Value = Morphism;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "a category")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: EnumAccess<'a>,
    {
        let (id, variant) = data.variant::<String>()?;
        match id.as_str() {
            "atomic" => {
                let pobj = variant.newtype_variant_seed(self.to::<MorphismData>())?;
                let mph = ActualMorphism::Atomic(pobj);
                Ok(self.ctx.mk(mph))
            }
            "identity" => {
                let obj = variant.newtype_variant_seed(self.to::<Object>())?;
                let mph = ActualMorphism::Identity(obj);
                Ok(self.ctx.mk(mph))
            }
            "comp" => {
                let comp = variant.tuple_variant(2, self.to::<parsers::morphism::Comp>())?;
                Ok(self.ctx.mk(comp))
            }
            "funct" => {
                let funct = variant.tuple_variant(2, self.to::<parsers::morphism::Funct>())?;
                Ok(self.ctx.mk(funct))
            }
            _ => {
                return Err(Error::unknown_variant(
                    id.as_str(),
                    &["atomic", "identity", "comp", "funct"],
                ))
            }
        }
    }
}
deserializer_enum!(
    Morphism,
    "morphism",
    &["atomic", "identity", "comp", "funct"]
);

//  _____                  _ _ _
// | ____|__ _ _   _  __ _| (_) |_ _   _
// |  _| / _` | | | |/ _` | | | __| | | |
// | |__| (_| | |_| | (_| | | | |_| |_| |
// |_____\__, |\__,_|\__,_|_|_|\__|\__, |
//          |_|                    |___/
// Equality
impl<'a> Visitor<'a> for Parser<EqualityData> {
    type Value = EqualityData;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "data for an equality")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'a>,
    {
        let mut pobj: Option<ProofObject> = None;
        let mut category: Option<Category> = None;
        let mut src: Option<Object> = None;
        let mut dst: Option<Object> = None;
        let mut left: Option<Morphism> = None;
        let mut right: Option<Morphism> = None;
        while let Some(k) = map.next_key::<String>()? {
            match k.as_str() {
                "pobj" => match pobj {
                    None => pobj = Some(map.next_value()?),
                    Some(_) => return Err(Error::duplicate_field("pobj")),
                },
                "category" => match category {
                    None => category = Some(map.next_value_seed(self.clone().to::<Category>())?),
                    Some(_) => return Err(Error::duplicate_field("category")),
                },
                "src" => match src {
                    None => src = Some(map.next_value_seed(self.clone().to::<Object>())?),
                    Some(_) => return Err(Error::duplicate_field("src")),
                },
                "dst" => match dst {
                    None => dst = Some(map.next_value_seed(self.clone().to::<Object>())?),
                    Some(_) => return Err(Error::duplicate_field("dst")),
                },
                "left" => match left {
                    None => left = Some(map.next_value_seed(self.clone().to::<Morphism>())?),
                    Some(_) => return Err(Error::duplicate_field("left")),
                },
                "right" => match right {
                    None => right = Some(map.next_value_seed(self.clone().to::<Morphism>())?),
                    Some(_) => return Err(Error::duplicate_field("right")),
                },
                _ => {
                    return Err(Error::unknown_field(
                        k.as_str(),
                        &["fobj", "category", "src", "dst", "left", "right"],
                    ))
                }
            }
        }
        let pobj = pobj.ok_or(Error::missing_field("fobj"))?;
        let category = category.ok_or(Error::missing_field("category"))?;
        let src = src.ok_or(Error::missing_field("src"))?;
        let dst = dst.ok_or(Error::missing_field("dst"))?;
        let left = left.ok_or(Error::missing_field("left"))?;
        let right = right.ok_or(Error::missing_field("right"))?;
        Ok(EqualityData {
            pobj,
            category,
            src,
            dst,
            left,
            right,
        })
    }
}
deserializer_struct!(EqualityData);

impl<'a> Visitor<'a> for Parser<Equality> {
    type Value = Equality;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "a category")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: EnumAccess<'a>,
    {
        let (id, variant) = data.variant::<String>()?;
        match id.as_str() {
            "atomic" => {
                let pobj = variant.newtype_variant_seed(self.to::<EqualityData>())?;
                let eq = ActualEquality::Atomic(pobj);
                Ok(self.ctx.mk(eq))
            }
            "refl" => {
                let mph = variant.newtype_variant_seed(self.to::<Morphism>())?;
                let eq = ActualEquality::Refl(mph);
                Ok(self.ctx.mk(eq))
            }
            "concat" => {
                let eq = variant.tuple_variant(2, self.to::<parsers::eq::Concat>())?;
                Ok(self.ctx.mk(eq))
            }
            "inv" => {
                let mph = variant.newtype_variant_seed(self.to::<Equality>())?;
                let eq = ActualEquality::Inv(mph);
                Ok(self.ctx.mk(eq))
            }
            "compose" => {
                let eq = variant.tuple_variant(2, self.to::<parsers::eq::Compose>())?;
                Ok(self.ctx.mk(eq))
            }
            "assoc" => {
                let eq = variant.tuple_variant(3, self.to::<parsers::eq::Assoc>())?;
                Ok(self.ctx.mk(eq))
            }
            "left_id" => {
                let mph = variant.newtype_variant_seed(self.to::<Morphism>())?;
                let eq = ActualEquality::LeftId(mph);
                Ok(self.ctx.mk(eq))
            }
            "right_id" => {
                let mph = variant.newtype_variant_seed(self.to::<Morphism>())?;
                let eq = ActualEquality::RightId(mph);
                Ok(self.ctx.mk(eq))
            }
            "rap" => {
                let eq = variant.tuple_variant(2, self.to::<parsers::eq::RAp>())?;
                Ok(self.ctx.mk(eq))
            }
            "lap" => {
                let eq = variant.tuple_variant(2, self.to::<parsers::eq::LAp>())?;
                Ok(self.ctx.mk(eq))
            }
            "funct_id" => {
                let eq = variant.tuple_variant(2, self.to::<parsers::eq::FunctId>())?;
                Ok(self.ctx.mk(eq))
            }
            "funct_comp" => {
                let eq = variant.tuple_variant(3, self.to::<parsers::eq::FunctComp>())?;
                Ok(self.ctx.mk(eq))
            }
            "funct_ctx" => {
                let eq = variant.tuple_variant(2, self.to::<parsers::eq::FunctCtx>())?;
                Ok(self.ctx.mk(eq))
            }
            _ => {
                return Err(Error::unknown_variant(
                    id.as_str(),
                    &[
                        "atomic",
                        "refl",
                        "concat",
                        "inv",
                        "compose",
                        "assoc",
                        "left_id",
                        "right_id",
                        "rap",
                        "lap",
                        "funct_id",
                        "funct_comp",
                        "funct_ctx",
                    ],
                ))
            }
        }
    }
}
deserializer_enum!(
    Equality,
    "equality",
    &[
        "atomic",
        "refl",
        "concat",
        "inv",
        "compose",
        "assoc",
        "left_id",
        "right_id",
        "rap",
        "lap",
        "funct_id",
        "funct_comp",
        "funct_ctx"
    ]
);

//     _               _____
//    / \   _ __  _   |_   _|__ _ __ _ __ ___
//   / _ \ | '_ \| | | || |/ _ \ '__| '_ ` _ \
//  / ___ \| | | | |_| || |  __/ |  | | | | | |
// /_/   \_\_| |_|\__, ||_|\___|_|  |_| |_| |_|
//                |___/
// AnyTerm

impl<'a> Visitor<'a> for Parser<AnyTerm> {
    type Value = AnyTerm;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "any term")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: EnumAccess<'a>,
    {
        let (id, variant) = data.variant::<String>()?;
        match id.as_str() {
            "category" => {
                let cat = variant.newtype_variant_seed(self.to::<Category>())?;
                Ok(AnyTerm::Cat(cat))
            }
            "functor" => {
                let funct = variant.newtype_variant_seed(self.to::<Functor>())?;
                Ok(AnyTerm::Funct(funct))
            }
            "object" => {
                let obj = variant.newtype_variant_seed(self.to::<Object>())?;
                Ok(AnyTerm::Obj(obj))
            }
            "morphism" => {
                let mph = variant.newtype_variant_seed(self.to::<Morphism>())?;
                Ok(AnyTerm::Mph(mph))
            }
            "equality" => {
                let eq = variant.newtype_variant_seed(self.to::<Equality>())?;
                Ok(AnyTerm::Eq(eq))
            }
            _ => {
                return Err(Error::unknown_variant(
                    id.as_str(),
                    &["category", "functor", "object", "morphism", "equality"],
                ))
            }
        }
    }
}
deserializer_enum!(
    AnyTerm,
    "term",
    &["category", "functor", "object", "morphism", "equality"]
);
