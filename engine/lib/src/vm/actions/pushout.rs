use crate::graph::{Face, GraphId};
use crate::remote::Remote;
use crate::vm::actions::lemma::UnifyPair;
use crate::vm::{Graph, Interactive, VM};
use std::collections::HashMap;

type Ins = crate::vm::asm::Instruction;

struct PartialMap {
    nodes: Vec<Option<usize>>,
    edges: Vec<Vec<Option<(usize, Vec<usize>)>>>,
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

fn extract_node(id: &UnifyPair) -> usize {
    match id {
        UnifyPair::Nodes(_, nd) => *nd,
        _ => panic!(),
    }
}

fn extract_morphism(id: &UnifyPair) -> Option<(usize, usize)> {
    match id {
        UnifyPair::Morphisms(_, (src, mph)) => Some((*src, *mph)),
        _ => None,
    }
}

fn extract_expand_morphism(id: &UnifyPair) -> Option<(usize, Vec<usize>)> {
    match id {
        UnifyPair::PathVM(_, src, mphs) => Some((*src, mphs.clone())),
        _ => None,
    }
}

fn extract_face(id: &UnifyPair) -> usize {
    match id {
        UnifyPair::Faces(_, fce) => *fce,
        _ => panic!(),
    }
}

struct Mapping {
    nodes: Vec<(usize, Vec<usize>)>,
    edges: Vec<(
        (usize, usize),
        Vec<(usize, usize)>,
        Option<(usize, Vec<usize>)>,
    )>,
    faces: Vec<(usize, Vec<usize>)>,
}

impl Mapping {
    fn new(map: &HashMap<GraphId, Vec<UnifyPair>>) -> Mapping {
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
                        mapping.1.iter().map(extract_morphism).flatten().collect(),
                        mapping.1.iter().find_map(extract_expand_morphism),
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
                direct.edges.iter_mut().for_each(|(_, v, multi)| {
                    v.iter_mut().for_each(|(src, mph)| {
                        if *src == prev {
                            *src = new;
                            *mph = newlen + prevlen - *mph - 1;
                        }
                    });
                    multi.iter_mut().for_each(|(src, mphs)| {
                        if *src == prev {
                            *src = new;
                            mphs.iter_mut().for_each(|mph| {
                                *mph = newlen + prevlen - *mph - 1;
                            });
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
            // Merge all simples
            let mut tgt = Vec::new();
            let mut src_node = 0;
            let mph = if mapping.1.is_empty() {
                None
            } else {
                let (src, mph1) = mapping.1[0];
                let mut mph = mph1;
                mapping.1.iter().skip(1).for_each(|(_, mph2)| {
                    let prev = mph;
                    mph = self.merge_edges(src, mph, *mph2);
                    direct.edges.iter_mut().for_each(|(_, v, multi)| {
                        v.iter_mut().for_each(|(s, m)| {
                            if *s == src && (*m == prev || *m == *mph2) {
                                *m = mph;
                            }
                        });
                        multi.iter_mut().for_each(|(s, mphs)| {
                            let mut nd = *s;
                            for m in mphs.iter_mut() {
                                if nd == src && (*m == prev || *m == *mph2) {
                                    *m = mph;
                                }
                                nd = self.graph.graph.edges[nd][*m].0;
                            }
                        });
                    });
                    map.edges.iter_mut().for_each(|v| {
                        v.iter_mut().for_each(|o| {
                            o.iter_mut().for_each(|(s, ms)| {
                                let mut nd = *s;
                                for m in ms.iter_mut() {
                                    if nd == src && (*m == prev || *m == *mph2) {
                                        *m = mph;
                                    }
                                    nd = self.graph.graph.edges[nd][*m].0;
                                }
                            });
                        });
                    });
                });
                tgt = vec![mph];
                src_node = src;
                Some(mph)
            };

            // Splice if required
            if let Some((src, dmphs)) = mapping.2 {
                if let Some(mph) = mph {
                    self.splice_edge(src, mph, &dmphs);
                    direct.edges.iter_mut().for_each(|(_, v, multi)| {
                        for i in 0..v.len() {
                            if v[i].0 == src && v[i].1 == mph {
                                if multi.is_none() {
                                    *multi = Some((src, dmphs.clone()));
                                } else {
                                    panic!() // TODO should be treated more gracefully
                                }
                            }
                        }
                        multi.iter_mut().for_each(|(s, mphs)| {
                            let mut splice_at = Vec::new();
                            let mut nd = *s;
                            for i in 0..mphs.len() {
                                if nd == src && mphs[i] == mph {
                                    splice_at.push(i);
                                }
                                nd = self.graph.graph.edges[nd][mphs[i]].0;
                            }
                            for i in splice_at.into_iter().rev() {
                                mphs.splice(i..(i + 1), dmphs.iter().copied());
                            }
                        });
                    });
                    map.edges.iter_mut().for_each(|v| {
                        v.iter_mut().for_each(|o| {
                            o.iter_mut().for_each(|(s, ms)| {
                                let mut nd = *s;
                                let mut splice_at = Vec::new();
                                for i in 0..ms.len() {
                                    if nd == src && ms[i] == mph {
                                        splice_at.push(i);
                                    }
                                    nd = self.graph.graph.edges[nd][ms[i]].0;
                                }
                                for i in splice_at.into_iter().rev() {
                                    ms.splice(i..(i + 1), dmphs.iter().copied());
                                }
                            });
                        });
                    });
                }
                tgt = dmphs;
                src_node = src;
            }

            map.edges[mapping.0 .0][mapping.0 .1] = Some((src_node, tgt));
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
    pub fn pushout(&mut self, other: &Graph, direct: &HashMap<GraphId, Vec<UnifyPair>>) {
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
                map.edges[src][mph] = Some((nsrc, vec![self.graph.graph.edges[nsrc].len() - 1]));
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
                        let old = *src;
                        *src = other.edges[*src][*mph].0;
                        Some(map.edges[old][*mph].as_ref().unwrap().1.iter())
                    })
                    .flatten()
                    .copied()
                    .collect(),
                right: rface
                    .right
                    .iter()
                    .scan(rface.start, |src, mph| {
                        let old = *src;
                        *src = other.edges[*src][*mph].0;
                        Some(map.edges[old][*mph].as_ref().unwrap().1.iter())
                    })
                    .flatten()
                    .copied()
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
