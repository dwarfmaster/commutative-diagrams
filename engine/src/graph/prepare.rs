use crate::anyterm::IsTerm;
use crate::data::{ActualEquality, EqualityData};
use crate::data::{ActualMorphism, ActualProofObject, Context, Morphism, Object};
use crate::graph::{Face, FaceParsed, Graph, GraphParsed};
use crate::normalize;
use crate::substitution::Substitution;
use std::ops::Deref;

impl<NL, EL, FL> GraphParsed<NL, EL, FL> {
    pub fn prepare(self, ctx: &mut Context) -> (Graph<NL, EL, FL>, Substitution) {
        let mut sigma = Vec::new();
        let nodes = self.nodes;

        // Normalize edges and remember identites
        let mut edges = self.edges;
        let mut edge_map: Vec<Vec<Option<usize>>> = vec![Vec::new(); nodes.len()];
        for src in 0..nodes.len() {
            for mph in 0..edges[src].len() {
                let (nmph, _) = normalize::identities(ctx, edges[src][mph].2.clone());
                edges[src][mph].2 = nmph;
            }
            edge_map[src] = edges[src]
                .iter()
                .scan(0, |index, (_, _, m)| match m.deref() {
                    ActualMorphism::Identity(..) => Some(None),
                    _ => {
                        *index += 1;
                        Some(Some(*index - 1))
                    }
                })
                .collect();
        }

        let faces = self
            .faces
            .into_iter()
            .map(|fce| fce.prepare(ctx, &nodes, &edges, &edge_map, &mut sigma))
            .collect();

        // Remove identites from edges
        for src in 0..nodes.len() {
            let mut edge = Vec::new();
            std::mem::swap(&mut edge, &mut edges[src]);
            edges[src] = edge
                .into_iter()
                .enumerate()
                .filter(|(i, _)| edge_map[src][*i].is_some())
                .map(|v| v.1)
                .collect();
        }
        (
            Graph {
                nodes,
                edges,
                faces,
            },
            sigma,
        )
    }
}

impl<FL> FaceParsed<FL> {
    fn prepare<NL, EL>(
        self,
        ctx: &mut Context,
        _nodes: &[(Object, NL)],
        edges: &[Vec<(usize, EL, Morphism)>],
        edge_map: &[Vec<Option<usize>>],
        sigma: &mut Substitution,
    ) -> Face<FL> {
        use ActualEquality::*;
        let nxt_mph = |src: &mut usize, mph: usize| -> Option<(usize, usize)> {
            let prev = *src;
            *src = edges[*src][mph].0;
            Some((prev, mph))
        };

        let left = self.eq.left(&ctx);
        let (leftmph, lefteq) = normalize::identities(ctx, left);
        let left = self
            .left
            .into_iter()
            .scan(self.start, nxt_mph)
            .filter_map(|(src, mph)| edge_map[src][mph])
            .collect();

        let right = self.eq.right(&ctx);
        let (rightmph, righteq) = normalize::identities(ctx, right);
        let right = self
            .right
            .into_iter()
            .scan(self.start, nxt_mph)
            .filter_map(|(src, mph)| edge_map[src][mph])
            .collect();

        let eq = if let ActualEquality::Atomic(data) = self.eq.deref() {
            if let ActualProofObject::Existential(ex) = data.pobj.deref() {
                let nex = ctx.new_existential();
                let hole = ctx.mk(ActualEquality::Atomic(EqualityData {
                    category: self.eq.cat(ctx),
                    src: self.eq.src(ctx),
                    dst: self.eq.dst(ctx),
                    left: leftmph,
                    right: rightmph,
                    pobj: ctx.mk(ActualProofObject::Existential(nex)),
                }));
                let refined = ctx
                    .mk(Concat(
                        lefteq.clone(),
                        ctx.mk(Concat(hole.clone(), ctx.mk(Inv(righteq.clone())))),
                    ))
                    .simpl(ctx);
                sigma.push((*ex, refined.term()));
                Some(hole)
            } else {
                None
            }
        } else {
            None
        };

        let eq = eq.unwrap_or(
            ctx.mk(Concat(
                ctx.mk(Inv(lefteq)),
                ctx.mk(Concat(self.eq, righteq)),
            ))
            .simpl(ctx),
        );

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
