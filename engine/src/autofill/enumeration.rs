use crate::graph::eq::Morphism;
use crate::graph::Graph;
use std::collections::HashMap;

pub struct Enum {
    pub paths: Vec<(u64, Morphism)>,
    // Map from morphism ids to path indexes
    reverse: HashMap<(u64, Morphism), usize>,
}

impl Enum {
    pub fn new() -> Self {
        Self {
            paths: Vec::new(),
            reverse: HashMap::new(),
        }
    }

    /// Get the path index associated to a morphism
    pub fn get(&self, cat: u64, mph: &Morphism) -> Option<usize> {
        match self.reverse.get(&(cat, mph.clone())) {
            Some(id) => Some(*id),
            None => None,
        }
    }
}

impl<NL, EL, FL> Graph<NL, EL, FL> {
    /// Enumerate paths on graph
    pub fn enumerate(&self, iters: usize) -> Enum {
        let nnodes = self.nodes.len();
        // First indice is the endpoint of the vector
        let mut paths: Vec<Vec<(u64, Morphism)>> = vec![Vec::new(); nnodes];
        // Map from morphisms uids to the associated path id
        let mut rev: HashMap<(u64, Morphism), (usize, usize)> = HashMap::new();
        // Add identities
        for n in 0..nnodes {
            let cat = self.nodes[n].1;
            let mph = Morphism::id(self.nodes[n].0);
            rev.insert((cat, mph.clone()), (n, paths[n].len()));
            paths[n].push((cat, mph));
        }

        // Add simple morphisms
        for src in 0..nnodes {
            for mph_id in 0..self.edges[src].len() {
                let cat = self.nodes[src].1;
                let dst = self.edges[src][mph_id].0;
                let norm = &self.edges[src][mph_id].3;
                if rev.get(&(cat, norm.clone())).is_none() {
                    rev.insert((cat, norm.clone()), (dst, paths[dst].len()));
                    paths[dst].push((cat, norm.clone()));
                }
            }
        }

        // Init ranges
        let mut ranges: Vec<(usize, usize)> = vec![(0, 0); nnodes];
        for n in 0..nnodes {
            ranges[n].0 = 1; // skip identities
            ranges[n].1 = paths[n].len();
        }

        // Add compositions
        for _ in 2..(iters + 1) {
            for src in 0..nnodes {
                let cat = self.nodes[src].1;
                for mph_id in 0..self.edges[src].len() {
                    let (dst, _, _, mph) = &self.edges[src][mph_id];
                    for nxt in (ranges[src].0)..(ranges[src].1) {
                        let path = &paths[src][nxt];
                        let mut nmph = path.1.clone();
                        nmph.compose(&mph);
                        if rev.get(&(cat, nmph.clone())).is_none() {
                            rev.insert((cat, nmph.clone()), (*dst, paths[*dst].len()));
                            paths[*dst].push((cat, nmph));
                        }
                    }
                }
            }
            for n in 0..nnodes {
                ranges[n].0 = ranges[n].1;
                ranges[n].1 = paths[n].len();
            }
        }

        let shifts: Vec<_> = paths
            .iter()
            .scan(0, |acc, v| {
                let prev = *acc;
                *acc = *acc + v.len();
                Some(prev)
            })
            .collect();
        Enum {
            paths: paths.into_iter().flatten().collect(),
            reverse: rev
                .into_iter()
                .map(|(k, (src, mph))| (k, shifts[src] + mph))
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::data::EvarStatus::Grounded;
    use crate::data::Feature;
    use crate::graph::eq::Morphism;
    use crate::graph::Graph;
    use crate::remote::Mock;

    #[test]
    pub fn count_transitive_closure() {
        let mut ctx = Mock::new();
        let cat = ctx.new_term("C".to_string(), None, Grounded);
        ctx.add_feat(cat, Feature::Category);
        let x = ctx.new_term("x".to_string(), None, Grounded);
        ctx.add_feat(x, Feature::Object { cat });
        let y = ctx.new_term("y".to_string(), None, Grounded);
        ctx.add_feat(y, Feature::Object { cat });
        let z = ctx.new_term("z".to_string(), None, Grounded);
        ctx.add_feat(z, Feature::Object { cat });
        let m1 = ctx.new_term("m1".to_string(), None, Grounded);
        ctx.add_feat(
            m1,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        let m2 = ctx.new_term("m2".to_string(), None, Grounded);
        ctx.add_feat(
            m2,
            Feature::Morphism {
                cat,
                src: y,
                dst: z,
            },
        );
        let m3 = ctx.new_term("m3".to_string(), None, Grounded);
        ctx.add_feat(
            m3,
            Feature::Morphism {
                cat,
                src: x,
                dst: z,
            },
        );
        let mz = ctx.new_term("mz".to_string(), None, Grounded);
        ctx.add_feat(
            mz,
            Feature::Morphism {
                cat,
                src: z,
                dst: z,
            },
        );

        let gr: Graph<(), (), ()> = Graph {
            nodes: vec![(x, cat, ()), (y, cat, ()), (z, cat, ())],
            edges: vec![
                vec![
                    (1, (), m1, Morphism::atom(x, y, m1)),
                    (2, (), m3, Morphism::atom(x, z, m3)),
                ],
                vec![(2, (), m2, Morphism::atom(y, z, m2))],
                vec![(2, (), mz, Morphism::atom(z, z, mz))],
            ],
            faces: Vec::new(),
        };

        let enum1 = gr.enumerate(2);
        let enum2 = gr.enumerate(3);

        assert_eq!(enum1.paths.len(), 11, "Number of paths of length at most 2");
        assert_eq!(enum2.paths.len(), 15, "Number of paths of length at most 3");
    }

    #[test]
    fn reverse() {
        let mut ctx = Mock::new();
        let cat = ctx.new_term("C".to_string(), None, Grounded);
        ctx.add_feat(cat, Feature::Category);
        let x = ctx.new_term("x".to_string(), None, Grounded);
        ctx.add_feat(x, Feature::Object { cat });
        let y = ctx.new_term("y".to_string(), None, Grounded);
        ctx.add_feat(y, Feature::Object { cat });
        let z = ctx.new_term("z".to_string(), None, Grounded);
        ctx.add_feat(z, Feature::Object { cat });
        let m1 = ctx.new_term("m1".to_string(), None, Grounded);
        ctx.add_feat(
            m1,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        let id = ctx.new_term("1".to_string(), None, Grounded);
        ctx.add_feat(
            id,
            Feature::Morphism {
                cat,
                src: y,
                dst: y,
            },
        );
        ctx.add_feat(id, Feature::Identity { cat, obj: y });
        let m2 = ctx.new_term("1 o m1".to_string(), None, Grounded);
        ctx.add_feat(
            m2,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        ctx.add_feat(
            m2,
            Feature::ComposeMph {
                cat,
                src: x,
                mid: y,
                dst: y,
                m1,
                m2: id,
            },
        );
        let m3 = ctx.new_term("m3".to_string(), None, Grounded);
        ctx.add_feat(
            m3,
            Feature::Morphism {
                cat,
                src: y,
                dst: z,
            },
        );

        let mph1 = Morphism::atom(x, y, m1);
        let mph3 = Morphism::atom(y, z, m3);
        let mph = Morphism {
            src: x,
            dst: z,
            comps: vec![(x, y, m1), (y, z, m3)],
        };

        let gr: Graph<(), (), ()> = Graph {
            nodes: vec![(x, cat, ()), (y, cat, ()), (z, cat, ())],
            edges: vec![
                vec![(1, (), m1, mph1.clone()), (1, (), m2, mph1)],
                vec![(2, (), m3, mph3)],
                vec![],
            ],
            faces: vec![],
        };

        let enm = gr.enumerate(2);
        assert_eq!(
            enm.paths.len(),
            6,
            "There actually are only three non trivial path"
        );
        assert!(
            enm.get(cat, &mph).is_some(),
            "m1 >> m3 should be found in enumeration"
        );
    }
}
