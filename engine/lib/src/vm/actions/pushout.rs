use crate::graph::{Face, GraphId};
use crate::remote::Remote;
use crate::vm::{Graph, Interactive, VM};
use std::collections::HashMap;

type Ins = crate::vm::asm::Instruction;

struct PartialMap {
    nodes: Vec<Option<usize>>,
    edges: Vec<Vec<Option<(usize, usize)>>>,
    faces: Vec<Option<usize>>,
}

impl PartialMap {
    fn new(gr: &Graph) -> Self {
        PartialMap {
            nodes: vec![None; gr.nodes.len()],
            edges: gr.edges.iter().map(|v| vec![None; v.len()]).collect(),
            faces: vec![None; gr.faces.len()],
        }
    }
}

fn extract_node(id: &GraphId) -> usize {
    match id {
        GraphId::Node(nd) => *nd,
        _ => panic!(),
    }
}

fn extract_morphism(id: &GraphId) -> (usize, usize) {
    match id {
        GraphId::Morphism(src, mph) => (*src, *mph),
        _ => panic!(),
    }
}

fn extract_face(id: &GraphId) -> usize {
    match id {
        GraphId::Face(fce) => *fce,
        _ => panic!(),
    }
}

struct Mapping {
    nodes: Vec<(usize, Vec<usize>)>,
    edges: Vec<((usize, usize), Vec<(usize, usize)>)>,
    faces: Vec<(usize, Vec<usize>)>,
}

impl Mapping {
    fn new(map: &HashMap<GraphId, Vec<GraphId>>) -> Mapping {
        let mut ret = Mapping {
            nodes: Vec::new(),
            edges: Vec::new(),
            faces: Vec::new(),
        };
        for mapping in map.iter() {
            use GraphId::*;
            match mapping.0 {
                Node(nd) => {
                    ret.nodes
                        .push((*nd, mapping.1.iter().map(extract_node).collect()));
                }
                Morphism(src, dst) => {
                    ret.edges.push((
                        (*src, *dst),
                        mapping.1.iter().map(extract_morphism).collect(),
                    ));
                }
                Face(fce) => {
                    ret.faces
                        .push((*fce, mapping.1.iter().map(extract_face).collect()));
                }
            }
        }
        ret
    }
}

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    fn pushout_merge_nodes(&mut self, direct: &mut Mapping, map: &mut PartialMap) {
        while let Some(mapping) = direct.nodes.pop() {
            let nd1 = mapping.1[0];
            let mut nd = nd1;
            mapping.1.iter().skip(1).for_each(|nd2| {
                let mut prevlen = self.graph.graph.edges[nd].len();
                let mut newlen = self.graph.graph.edges[*nd2].len();
                let mut prev = nd;
                let mut new = *nd2;

                nd = self.merge_nodes(nd, *nd2);
                if nd != *nd2 {
                    std::mem::swap(&mut prev, &mut new);
                    std::mem::swap(&mut prevlen, &mut newlen);
                }
                direct.edges.iter_mut().for_each(|(_, v)| {
                    v.iter_mut().for_each(|(src, mph)| {
                        if *src == prev {
                            *src = new;
                            *mph = newlen + prevlen - *mph - 1;
                        }
                    });
                });
                direct.nodes.iter_mut().for_each(|(_, v)| {
                    v.iter_mut().for_each(|n| {
                        if *n == prev {
                            *n = new;
                        }
                    });
                });
                map.nodes.iter_mut().for_each(|n| {
                    if let Some(n) = n {
                        if *n == prev {
                            *n = new;
                        }
                    }
                });
            });
            map.nodes[mapping.0] = Some(nd);
        }
    }

    fn pushout_merge_edges(&mut self, direct: &mut Mapping, map: &mut PartialMap) {
        while let Some(mapping) = direct.edges.pop() {
            let (src, mph1) = mapping.1[0];
            let mut mph = mph1;
            mapping.1.iter().skip(1).for_each(|(_, mph2)| {
                let prev = mph;
                mph = self.merge_edges(src, mph, *mph2);
                direct.edges.iter_mut().for_each(|(_, v)| {
                    v.iter_mut().for_each(|(s, m)| {
                        if *s == src && (*m == prev || *m == *mph2) {
                            *m = mph;
                        }
                    });
                });
                map.edges.iter_mut().for_each(|v| {
                    v.iter_mut().for_each(|o| {
                        if let Some((s, m)) = o {
                            if *s == src && (*m == prev || *m == *mph2) {
                                *m = mph;
                            }
                        }
                    });
                });
            });
            map.edges[mapping.0 .0][mapping.0 .1] = Some((src, mph));
        }
    }

    fn pushout_merge_faces(&mut self, direct: &mut Mapping, map: &mut PartialMap) {
        while let Some(mapping) = direct.faces.pop() {
            let fce1 = mapping.1[0];
            let mut fce = fce1;
            mapping.1.iter().skip(1).for_each(|fce2| {
                let prev = fce;
                fce = self.merge_faces(fce, *fce2);
                direct.faces.iter_mut().for_each(|(_, v)| {
                    v.iter_mut().for_each(|f| {
                        if *f == prev || *f == *fce2 {
                            *f = fce;
                        }
                    });
                });
                map.faces.iter_mut().for_each(|o| {
                    if let Some(f) = o {
                        if *f == prev || *f == *fce2 {
                            *f = fce;
                        }
                    }
                });
            });
            map.faces[mapping.0] = Some(fce);
        }
    }

    // The direct mapping is a mapping from other into self.graph.graph. The object
    // under the mapped together must be equal. The pushout proceed by
    // incrementally filling a graph morphism from other and the current graph,
    // first on nodes, then on edges and finally on faces. The morphism starts
    // from the mapping. When the mapping is not well defined, the target
    // objects are merged. Then objects are added to the graph to complete the
    // injection. It also keeps a map from the initial graph to position in the
    // resulting graph.
    pub fn pushout(&mut self, other: &Graph, direct: &HashMap<GraphId, Vec<GraphId>>) {
        let mut map = PartialMap::new(&other);
        let mut direct = Mapping::new(direct);

        // Nodes
        self.pushout_merge_nodes(&mut direct, &mut map);
        for nd in 0..other.nodes.len() {
            if map.nodes[nd].is_some() {
                continue;
            }
            self.register_instruction(Ins::InsertNode(other.nodes[nd].0, other.nodes[nd].1));
            map.nodes[nd] = Some(self.graph.graph.nodes.len() - 1);
        }

        // Edges
        self.pushout_merge_edges(&mut direct, &mut map);
        for src in 0..other.nodes.len() {
            for mph in 0..other.edges[src].len() {
                if map.edges[src][mph].is_some() {
                    continue;
                }
                let nsrc = map.nodes[src].unwrap();
                let ndst = map.nodes[other.edges[src][mph].0].unwrap();
                self.register_instruction(Ins::InsertMorphism(
                    nsrc,
                    ndst,
                    other.edges[src][mph].2,
                    other.edges[src][mph].3.clone(),
                ));
                map.edges[src][mph] = Some((nsrc, self.graph.graph.edges[nsrc].len() - 1));
            }
        }

        // Faces
        self.pushout_merge_faces(&mut direct, &mut map);
        for fce in 0..other.faces.len() {
            if map.faces[fce].is_some() {
                continue;
            }
            let rface = &other.faces[fce];
            let face = Face {
                start: map.nodes[rface.start].unwrap(),
                end: map.nodes[rface.end].unwrap(),
                left: rface
                    .left
                    .iter()
                    .scan(rface.start, |src, mph| {
                        let nmph = map.edges[*src][*mph].unwrap().1;
                        *src = other.edges[*src][*mph].0;
                        Some(nmph)
                    })
                    .collect(),
                right: rface
                    .right
                    .iter()
                    .scan(rface.start, |src, mph| {
                        let nmph = map.edges[*src][*mph].unwrap().1;
                        *src = other.edges[*src][*mph].0;
                        Some(nmph)
                    })
                    .collect(),
                eq: rface.eq.clone(),
                label: Default::default(),
            };
            self.register_instruction(Ins::InsertFace(face));
            map.faces[fce] = Some(self.graph.graph.faces.len() - 1);
        }

        // Update metadata
        self.relabel();
    }
}
