use crate::data::{ActualEquality, ActualMorphism, Context, Morphism, Object};
use crate::graph::{Face, FaceParsed, Graph, GraphParsed};
use crate::normalize;
use std::ops::Deref;

impl<NL, EL, FL> GraphParsed<NL, EL, FL> {
    pub fn prepare(self, ctx: &mut Context) -> Graph<NL, EL, FL> {
        let nodes = self.nodes;
        let mut edges = self.edges;
        for src in 0..nodes.len() {
            for mph in 0..edges[src].len() {
                let (nmph, _) = normalize::identities(ctx, edges[src][mph].2.clone());
                edges[src][mph].2 = nmph;
            }
        }
        let faces = self
            .faces
            .into_iter()
            .map(|fce| fce.prepare(ctx, &nodes, &edges))
            .collect();
        Graph {
            nodes,
            edges,
            faces,
        }
    }
}

impl<FL> FaceParsed<FL> {
    fn prepare<NL, EL>(
        self,
        ctx: &mut Context,
        _nodes: &[(Object, NL)],
        edges: &[Vec<(usize, EL, Morphism)>],
    ) -> Face<FL> {
        use ActualEquality::*;
        let nxt_mph = |src: &mut usize, mph: usize| -> Option<(usize, usize)> {
            let prev = *src;
            *src = edges[*src][mph].0;
            Some((prev, mph))
        };
        let isnt_identity = |(src, mph): &(usize, usize)| -> bool {
            match edges[*src][*mph].2.deref() {
                ActualMorphism::Identity(..) => false,
                _ => true,
            }
        };

        let left = self.eq.left(&ctx);
        let (_, lefteq) = normalize::identities(ctx, left);
        let left = self
            .left
            .into_iter()
            .scan(self.start, nxt_mph)
            .filter(isnt_identity)
            .map(|pr| pr.1)
            .collect();

        let right = self.eq.right(&ctx);
        let (_, righteq) = normalize::identities(ctx, right);
        let right = self
            .right
            .into_iter()
            .scan(self.start, nxt_mph)
            .filter(isnt_identity)
            .map(|pr| pr.1)
            .collect();

        let eq = ctx.mk(Concat(
            ctx.mk(Inv(lefteq)),
            ctx.mk(Concat(self.eq, righteq)),
        ));

        Face {
            start: self.start,
            end: self.end,
            left,
            right,
            eq,
            label: self.label,
        }
    }
}
