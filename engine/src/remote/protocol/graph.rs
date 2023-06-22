use crate::graph;
use crate::graph::GraphParsed;
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

struct Nodes<'a, NL> {
    nodes: &'a [(u64, u64, NL)],
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

struct Edges<'a, EL> {
    edges: &'a [Vec<(usize, EL, u64)>],
}

impl<'a, EL> Serialize for Edges<'a, EL> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_seq(
            self.edges
                .iter()
                .enumerate()
                .map(|(src, v)| {
                    v.iter().map(move |(dst, _, mph)| Edge {
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

struct Faces<'a, FL> {
    faces: &'a [graph::FaceParsed<FL>],
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

impl<'de, NL, EL, FL> Deserialize<'de> for GraphParsed<NL, EL, FL>
where
    NL: Default,
    EL: Default,
    FL: Default,
{
    fn deserialize<D>(_deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // TODO
        todo!()
    }
}
