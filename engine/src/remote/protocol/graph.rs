use crate::graph;
use crate::graph::GraphParsed;
use core::marker::PhantomData;
use serde::de::{Error, SeqAccess, Visitor};
use serde::ser::SerializeSeq;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

struct Node {
    obj: u64,
    cat: u64,
}

struct Edge {
    src: usize,
    dst: usize,
    mph: u64,
}

struct Face {
    src: usize,
    dst: usize,
    left: Vec<usize>,
    right: Vec<usize>,
    eq: u64,
}

struct Nodes<'a, NL> {
    nodes: &'a [(u64, u64, NL)],
}

struct Edges<'a, EL, MPH> {
    edges: &'a [Vec<(usize, EL, u64, MPH)>],
}

struct Faces<'a, FL> {
    faces: &'a [graph::FaceParsed<FL>],
}

//  ____            _       _ _
// / ___|  ___ _ __(_) __ _| (_)_______
// \___ \ / _ \ '__| |/ _` | | |_  / _ \
//  ___) |  __/ |  | | (_| | | |/ /  __/
// |____/ \___|_|  |_|\__,_|_|_/___\___|
//

impl Serialize for Node {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(2))?;
        seq.serialize_element(&self.obj)?;
        seq.serialize_element(&self.cat)?;
        seq.end()
    }
}

impl<'a, NL> Serialize for Nodes<'a, NL> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_seq(self.nodes.iter().map(|(obj, cat, _)| Node {
            obj: *obj,
            cat: *cat,
        }))
    }
}

impl Serialize for Edge {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(3))?;
        seq.serialize_element(&self.src)?;
        seq.serialize_element(&self.dst)?;
        seq.serialize_element(&self.mph)?;
        seq.end()
    }
}

impl<'a, EL, MPH> Serialize for Edges<'a, EL, MPH> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_seq(
            self.edges
                .iter()
                .enumerate()
                .map(|(src, v)| {
                    v.iter().map(move |(dst, _, mph, _)| Edge {
                        src,
                        dst: *dst,
                        mph: *mph,
                    })
                })
                .flatten(),
        )
    }
}

impl Serialize for Face {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(5))?;
        seq.serialize_element(&self.src)?;
        seq.serialize_element(&self.dst)?;
        seq.serialize_element(&self.left)?;
        seq.serialize_element(&self.right)?;
        seq.serialize_element(&self.eq)?;
        seq.end()
    }
}

impl<'a, FL> Serialize for Faces<'a, FL> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_seq(self.faces.iter().map(|f| Face {
            src: f.start,
            dst: f.end,
            left: f.left.clone(),
            right: f.right.clone(),
            eq: f.eq,
        }))
    }
}

impl<NL, EL, FL> Serialize for GraphParsed<NL, EL, FL> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(3))?;
        seq.serialize_element(&Nodes { nodes: &self.nodes })?;
        seq.serialize_element(&Edges { edges: &self.edges })?;
        seq.serialize_element(&Faces { faces: &self.faces })?;
        seq.end()
    }
}

//  ____                      _       _ _
// |  _ \  ___  ___  ___ _ __(_) __ _| (_)_______
// | | | |/ _ \/ __|/ _ \ '__| |/ _` | | |_  / _ \
// | |_| |  __/\__ \  __/ |  | | (_| | | |/ /  __/
// |____/ \___||___/\___|_|  |_|\__,_|_|_/___\___|
//

struct NodeDeserializer {}
impl<'de> Visitor<'de> for NodeDeserializer {
    type Value = Node;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "a tuple of an object and a category")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let obj = seq.next_element()?.ok_or(Error::invalid_length(0, &self))?;
        let cat = seq.next_element()?.ok_or(Error::invalid_length(1, &self))?;
        if seq.next_element::<()>()?.is_some() {
            Err(Error::invalid_length(3 /* TODO find length */, &self))
        } else {
            Ok(Node { obj, cat })
        }
    }
}
impl<'de> Deserialize<'de> for Node {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_tuple(2, NodeDeserializer {})
    }
}

struct EdgeDeserializer {}
impl<'de> Visitor<'de> for EdgeDeserializer {
    type Value = Edge;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "a triple representing an edge")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let src = seq.next_element()?.ok_or(Error::invalid_length(0, &self))?;
        let dst = seq.next_element()?.ok_or(Error::invalid_length(1, &self))?;
        let mph = seq.next_element()?.ok_or(Error::invalid_length(2, &self))?;
        if seq.next_element::<()>()?.is_some() {
            Err(Error::invalid_length(4 /* TODO find length */, &self))
        } else {
            Ok(Edge { src, dst, mph })
        }
    }
}
impl<'de> Deserialize<'de> for Edge {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_tuple(3, EdgeDeserializer {})
    }
}

struct FaceDeserializer {}
impl<'de> Visitor<'de> for FaceDeserializer {
    type Value = Face;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "a quintuplet representing a face")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let src = seq.next_element()?.ok_or(Error::invalid_length(0, &self))?;
        let dst = seq.next_element()?.ok_or(Error::invalid_length(1, &self))?;
        let left = seq.next_element()?.ok_or(Error::invalid_length(2, &self))?;
        let right = seq.next_element()?.ok_or(Error::invalid_length(3, &self))?;
        let eq = seq.next_element()?.ok_or(Error::invalid_length(4, &self))?;
        if seq.next_element::<()>()?.is_some() {
            Err(Error::invalid_length(6 /* TODO find length */, &self))
        } else {
            Ok(Face {
                src,
                dst,
                left,
                right,
                eq,
            })
        }
    }
}
impl<'de> Deserialize<'de> for Face {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_tuple(5, FaceDeserializer {})
    }
}

struct GraphDeserializer<NL, EL, FL> {
    nl: PhantomData<NL>,
    el: PhantomData<EL>,
    fl: PhantomData<FL>,
}
impl<'de, NL: Default, EL: Default, FL: Default> Visitor<'de> for GraphDeserializer<NL, EL, FL> {
    type Value = GraphParsed<NL, EL, FL>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            formatter,
            "a graph as a triple of nodes, morphisms and edges"
        )
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let nodes: Vec<Node> = seq.next_element()?.ok_or(Error::invalid_length(0, &self))?;
        let edges: Vec<Edge> = seq.next_element()?.ok_or(Error::invalid_length(1, &self))?;
        let faces: Vec<Face> = seq.next_element()?.ok_or(Error::invalid_length(2, &self))?;
        if seq.next_element::<()>()?.is_some() {
            return Err(Error::invalid_length(4 /* TODO get length */, &self));
        }

        let gr_nodes: Vec<(u64, u64, NL)> = nodes
            .into_iter()
            .map(|nd| (nd.obj, nd.cat, Default::default()))
            .collect();

        let mut gr_edges: Vec<Vec<(usize, EL, u64, ())>> = Vec::new();
        for _ in 0..gr_nodes.len() {
            gr_edges.push(Vec::new())
        }
        let mut edges_map: Vec<(usize, usize)> = Vec::new();
        for edge in edges {
            edges_map.push((edge.src, gr_edges[edge.src].len()));
            gr_edges[edge.src].push((edge.dst, Default::default(), edge.mph, ()));
        }

        let mut gr_faces: Vec<graph::FaceParsed<FL>> = Vec::new();
        for face in faces {
            let fce = graph::FaceParsed {
                start: face.src,
                end: face.dst,
                left: face.left.iter().map(|m| edges_map[*m].1).collect(),
                right: face.right.iter().map(|m| edges_map[*m].1).collect(),
                eq: face.eq,
                label: Default::default(),
            };
            gr_faces.push(fce);
        }

        Ok(Self::Value {
            nodes: gr_nodes,
            edges: gr_edges,
            faces: gr_faces,
        })
    }
}

impl<'de, NL, EL, FL> Deserialize<'de> for GraphParsed<NL, EL, FL>
where
    NL: Default,
    EL: Default,
    FL: Default,
{
    fn deserialize<D>(d: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        d.deserialize_tuple(
            3,
            GraphDeserializer {
                nl: PhantomData,
                el: PhantomData,
                fl: PhantomData,
            },
        )
    }
}
