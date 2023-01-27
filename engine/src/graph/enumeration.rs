use crate::data::{ActualEquality, ActualMorphism};
use crate::data::{Context, Equality, Morphism};
use crate::graph::definition::Graph;
use crate::normalize;

#[derive(Clone)]
pub struct Path {
    pub start: usize,
    pub nexts: Vec<usize>,
    pub mph: Morphism,
    pub eq: Equality,
}

pub type Enum = Vec<Path>;

impl Graph {
    /// Enumerate paths on graph
    pub fn enumerate(&self, ctx: &mut Context, iters: usize) -> Enum {
        let nnodes = self.nodes.len();
        // First indice is the endpoint of the vector
        let mut paths: Vec<Vec<Path>> = vec![Vec::new(); nnodes];
        // Add identities
        for n in 0..nnodes {
            let mph = ctx.mk(ActualMorphism::Identity(self.nodes[n].clone()));
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
                let (mph, eq) = normalize::morphism(ctx, mph.clone());
                paths[src].push(Path {
                    start: src,
                    nexts: vec![mph_id],
                    mph: mph.clone(),
                    eq: eq.clone(),
                });
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
                        let (nmph, eq) = normalize::morphism(ctx, nmph);
                        let eq = ctx.mk(ActualEquality::Concat(
                            ctx.mk(ActualEquality::LAp(mph.clone(), path.eq.clone())),
                            eq,
                        ));
                        // TODO check if morphism is already present
                        let mut nexts = vec![mph_id];
                        nexts.extend(path.nexts.iter());
                        paths[src].push(Path {
                            start: src,
                            nexts,
                            mph: nmph,
                            eq,
                        })
                    }
                }
            }
            for n in 0..nnodes {
                ranges[n].0 = ranges[n].1;
                ranges[n].1 = paths[n].len();
            }
        }

        paths.into_iter().flatten().collect()
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

        assert_eq!(enum1.len(), 11, "Number of paths of length at most 2");
        assert_eq!(enum2.len(), 15, "Number of paths of length at most 3");
    }
}
