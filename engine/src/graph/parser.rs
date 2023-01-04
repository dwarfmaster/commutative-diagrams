use crate::data::{ActualMorphism, Equality, Morphism, Object};
use crate::graph::definition::{Face, Graph};
use crate::parser::{deserializer_struct, Parser};
use core::fmt;
use serde::de::{DeserializeSeed, Error, MapAccess, Visitor};
use serde::ser::SerializeStruct;
use serde::{Deserializer, Serialize, Serializer};
use std::ops::Deref;

#[derive(Serialize)]
struct Edge {
    src: usize,
    dst: usize,
    mph: ActualMorphism,
}

//  ____            _       _ _
// / ___|  ___ _ __(_) __ _| (_)_______
// \___ \ / _ \ '__| |/ _` | | |_  / _ \
//  ___) |  __/ |  | | (_| | | |/ /  __/
// |____/ \___|_|  |_|\__,_|_|_/___\___|
//

impl Serialize for Face {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut f = s.serialize_struct("face", 5)?;
        f.serialize_field("src", &self.start)?;
        f.serialize_field("dst", &self.end)?;
        f.serialize_field("left", &self.left)?;
        f.serialize_field("right", &self.right)?;
        f.serialize_field("eq", &self.eq.deref())?;
        f.end()
    }
}

impl Serialize for Graph {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let nodes: Vec<_> = self.nodes.iter().map(|e| e.deref()).collect();
        let edges: Vec<_> = self
            .edges
            .iter()
            .enumerate()
            .map(|(s, v)| {
                v.iter().map(move |(d, m)| Edge {
                    src: s.clone(),
                    dst: *d,
                    mph: (*m.deref()).clone(),
                })
            })
            .flatten()
            .collect();
        let mut g = s.serialize_struct("graph", 3)?;
        g.serialize_field("nodes", &nodes)?;
        g.serialize_field("edges", &edges)?;
        g.serialize_field("faces", &self.faces)?;
        g.end()
    }
}

//  ____                      _       _ _
// |  _ \  ___  ___  ___ _ __(_) __ _| (_)_______
// | | | |/ _ \/ __|/ _ \ '__| |/ _` | | |_  / _ \
// | |_| |  __/\__ \  __/ |  | | (_| | | |/ /  __/
// |____/ \___||___/\___|_|  |_|\__,_|_|_/___\___|
//

impl<'a> Visitor<'a> for Parser<Face> {
    type Value = Face;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "a face")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'a>,
    {
        let mut start: Option<usize> = None;
        let mut end: Option<usize> = None;
        let mut left: Option<Vec<usize>> = None;
        let mut right: Option<Vec<usize>> = None;
        let mut eq: Option<Equality> = None;
        while let Some(k) = map.next_key::<String>()? {
            match k.as_str() {
                "src" => match start {
                    None => start = Some(map.next_value()?),
                    Some(_) => return Err(Error::duplicate_field("src")),
                },
                "dst" => match end {
                    None => end = Some(map.next_value()?),
                    Some(_) => return Err(Error::duplicate_field("dst")),
                },
                "left" => match left {
                    None => left = Some(map.next_value()?),
                    Some(_) => return Err(Error::duplicate_field("left")),
                },
                "right" => match right {
                    None => right = Some(map.next_value()?),
                    Some(_) => return Err(Error::duplicate_field("right")),
                },
                "eq" => match eq {
                    None => eq = Some(map.next_value_seed(self.clone().to::<Equality>())?),
                    Some(_) => return Err(Error::duplicate_field("eq")),
                },
                _ => {
                    return Err(Error::unknown_field(
                        k.as_str(),
                        &["src", "dst", "left", "right", "eq"],
                    ))
                }
            }
        }
        let start = start.ok_or(Error::missing_field("src"))?;
        let end = end.ok_or(Error::missing_field("dst"))?;
        let left = left.ok_or(Error::missing_field("left"))?;
        let right = right.ok_or(Error::missing_field("right"))?;
        let eq = eq.ok_or(Error::missing_field("eq"))?;
        Ok(Face {
            start,
            end,
            left,
            right,
            eq,
        })
    }
}
deserializer_struct!(Face);

impl<'a> Visitor<'a> for Parser<Edge> {
    type Value = Edge;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "an edge")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'a>,
    {
        let mut src: Option<usize> = None;
        let mut dst: Option<usize> = None;
        let mut mph: Option<Morphism> = None;
        while let Some(k) = map.next_key::<String>()? {
            match k.as_str() {
                "src" => match src {
                    None => src = Some(map.next_value()?),
                    Some(_) => return Err(Error::duplicate_field("src")),
                },
                "dst" => match dst {
                    None => dst = Some(map.next_value()?),
                    Some(_) => return Err(Error::duplicate_field("dst")),
                },
                "mph" => match mph {
                    None => mph = Some(map.next_value_seed(self.clone().to::<Morphism>())?),
                    Some(_) => return Err(Error::duplicate_field("mph")),
                },
                _ => return Err(Error::unknown_field(k.as_str(), &["src", "dst", "mph"])),
            }
        }
        let src = src.ok_or(Error::missing_field("src"))?;
        let dst = dst.ok_or(Error::missing_field("dst"))?;
        let mph = mph.ok_or(Error::missing_field("mph"))?;
        Ok(Edge {
            src,
            dst,
            mph: (*mph.deref()).clone(),
        })
    }
}
deserializer_struct!(Edge);

impl<'a> Visitor<'a> for Parser<Graph> {
    type Value = Graph;
    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "a graph")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'a>,
    {
        let mut nodes: Option<Vec<Object>> = None;
        let mut edges: Option<Vec<Edge>> = None;
        let mut faces: Option<Vec<Face>> = None;
        while let Some(k) = map.next_key::<String>()? {
            match k.as_str() {
                "nodes" => match nodes {
                    None => nodes = Some(map.next_value_seed(self.clone().to_vec::<Object>())?),
                    Some(_) => return Err(Error::duplicate_field("nodes")),
                },
                "edges" => match edges {
                    None => edges = Some(map.next_value_seed(self.clone().to_vec::<Edge>())?),
                    Some(_) => return Err(Error::duplicate_field("edges")),
                },
                "faces" => match faces {
                    None => faces = Some(map.next_value_seed(self.clone().to_vec::<Face>())?),
                    Some(_) => return Err(Error::duplicate_field("faces")),
                },
                _ => {
                    return Err(Error::unknown_field(
                        k.as_str(),
                        &["nodes", "edges", "faces"],
                    ))
                }
            }
        }
        let nodes = nodes.ok_or(Error::missing_field("nodes"))?;
        let edges = edges.ok_or(Error::missing_field("edges"))?;
        let faces = faces.ok_or(Error::missing_field("faces"))?;
        let len = nodes.len();
        let mut gr = Graph {
            nodes,
            edges: vec![Vec::new(); len],
            faces,
        };
        for edge in edges {
            gr.edges[edge.src].push((edge.dst, self.ctx.mk(edge.mph)))
        }
        Ok(gr)
    }
}
deserializer_struct!(Graph);
