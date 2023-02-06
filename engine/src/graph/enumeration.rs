use crate::data::{ActualEquality, ActualMorphism};
use crate::data::{Context, Equality, Morphism};
use crate::graph::definition::Graph;
use crate::normalize;
use std::collections::HashMap;

#[derive(Clone)]
pub struct Path {
    pub start: usize,
    pub nexts: Vec<usize>,
    // mph is the non-normalized realisation of the path
    pub mph: Morphism,
    // eq is an equality between the mph of the cell and the mph
    // of its parent
    pub eq: Equality,
}

pub struct Enum {
    pub paths: Vec<Path>,
    // Map from morphism ids to path indexes
    reverse: HashMap<u64, usize>,
}

impl Enum {
    pub fn new() -> Self {
        Self {
            paths: Vec::new(),
            reverse: HashMap::new(),
        }
    }

    /// Get the path index associated to a morphism
    pub fn get(&self, mph: &Morphism) -> Option<usize> {
        match self.reverse.get(&mph.uid()) {
            Some(id) => Some(*id),
            None => None,
        }
    }
}

impl Graph {
    /// Enumerate paths on graph
    pub fn enumerate(&self, ctx: &mut Context, iters: usize) -> Enum {
        let nnodes = self.nodes.len();
        // First indice is the endpoint of the vector
        let mut paths: Vec<Vec<Path>> = vec![Vec::new(); nnodes];
        // Map from morphisms uids to the associated path id
        let mut rev: HashMap<u64, (usize, usize)> = HashMap::new();
        // Add identities
        for n in 0..nnodes {
            let mph = ctx.mk(ActualMorphism::Identity(self.nodes[n].clone()));
            rev.insert(mph.uid(), (n, paths[n].len()));
            paths[n].push(Path {
                start: n,
                nexts: Vec::new(),
                mph: mph.clone(),
                eq: ctx.mk(ActualEquality::Refl(mph)),
            })
        }

        // Add simple morphisms
        for src in 0..nnodes {
            for mph_id in 0..self.edges[src].len() {
                let (_, mph) = &self.edges[src][mph_id];
                let (norm, eq) = normalize::morphism(ctx, mph.clone());
                // The map is necessary to prevent rust from complaining about
                // found borrowing rev
                let found = rev.get(&norm.uid()).map(|v| *v);
                match found {
                    Some(id) => {
                        rev.insert(mph.uid(), id);
                    }
                    None => {
                        rev.insert(mph.uid(), (src, paths[src].len()));
                        rev.insert(norm.uid(), (src, paths[src].len()));
                        paths[src].push(Path {
                            start: src,
                            nexts: vec![mph_id],
                            mph: norm,
                            eq: eq.clone(),
                        });
                    }
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
                for mph_id in 0..self.edges[src].len() {
                    let (dst, mph) = &self.edges[src][mph_id];
                    for nxt in (ranges[*dst].0)..(ranges[*dst].1) {
                        let path = &paths[*dst][nxt];
                        let nmph = ctx.mk(ActualMorphism::Comp(mph.clone(), path.mph.clone()));
                        let (norm_mph, eq) = normalize::morphism(ctx, nmph.clone());
                        let eq = ctx.mk(ActualEquality::Concat(
                            ctx.mk(ActualEquality::LAp(mph.clone(), path.eq.clone())),
                            eq,
                        ));
                        let found = rev.get(&norm_mph.uid()).map(|v| *v);
                        match found {
                            Some(id) => {
                                rev.insert(nmph.uid(), id);
                            }
                            None => {
                                rev.insert(nmph.uid(), (src, paths[src].len()));
                                rev.insert(norm_mph.uid(), (src, paths[src].len()));
                                let mut nexts = vec![mph_id];
                                nexts.extend(path.nexts.iter());
                                paths[src].push(Path {
                                    start: src,
                                    nexts,
                                    mph: norm_mph,
                                    eq,
                                })
                            }
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
    use crate::data::{ActualCategory, ActualMorphism, ActualObject};
    use crate::data::{CategoryData, MorphismData, ObjectData};
    use crate::data::{Context, ProofObject};
    use crate::dsl::{cat, mph, obj};
    use crate::graph::Graph;

    #[test]
    pub fn count_transitive_closure() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let x = obj!(ctx, (:1) in cat);
        let y = obj!(ctx, (:2) in cat);
        let z = obj!(ctx, (:3) in cat);
        let m1 = mph!(ctx, (:4) : x -> y);
        let m2 = mph!(ctx, (:5) : y -> z);
        let m3 = mph!(ctx, (:6) : x -> z);
        let mz = mph!(ctx, (:7) : z -> z);

        let gr: Graph = Graph {
            nodes: vec![x, y, z],
            edges: vec![vec![(1, m1), (2, m3)], vec![(2, m2)], vec![(2, mz)]],
            faces: Vec::new(),
        };

        let enum1 = gr.enumerate(&mut ctx, 2);
        let enum2 = gr.enumerate(&mut ctx, 3);

        assert_eq!(enum1.paths.len(), 11, "Number of paths of length at most 2");
        assert_eq!(enum2.paths.len(), 15, "Number of paths of length at most 3");
    }

    #[test]
    fn reverse() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let x = obj!(ctx, (:1) in cat);
        let y = obj!(ctx, (:2) in cat);
        let z = obj!(ctx, (:3) in cat);
        let m1 = mph!(ctx, (:4) : x -> y);
        let m2 = mph!(ctx, m1 >> (id y));
        let m3 = mph!(ctx, (:5) : y -> z);
        let m = mph!(ctx, m2 >> m3);

        let gr: Graph = Graph {
            nodes: vec![x, y, z],
            edges: vec![
                vec![(1, m1), (1, m2.clone())],
                vec![(2, m3.clone())],
                vec![],
            ],
            faces: vec![],
        };

        let enm = gr.enumerate(&mut ctx, 2);
        assert_eq!(
            enm.paths.len(),
            6,
            "There actually are only three non trivial path"
        );
        assert!(enm.get(&m2).is_some(), "m2 should be found in enumeration");
        assert!(
            enm.get(&m).is_some(),
            "m2 >> m3 should be found in enumeration"
        );
    }
}
