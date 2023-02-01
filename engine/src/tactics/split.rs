use crate::data::{Context, Morphism};
use crate::graph::{Face, Graph};
use crate::normalize;
use crate::tactics::insert_mph_at;
use std::ops::Deref;

/// Normalize a morphism of the graph, then split it along composition,
/// introduce the components as edges, add a face between the source morphism
/// and the new path, and return the index of the new face
pub fn split_norm(ctx: &mut Context, gr: &mut Graph, src: usize, mph: usize) -> usize {
    assert!(src < gr.nodes.len(), "src out of bounds");
    assert!(mph < gr.edges[src].len(), "mph out of bounds");
    let (norm, eqnorm) = normalize::morphism(ctx, gr.edges[src][mph].1.clone());
    let (mut path, dst) = insert_split_at(ctx, gr, src, norm);
    path.reverse();
    let fce = Face {
        start: src,
        end: dst,
        left: vec![mph],
        right: path,
        eq: eqnorm,
    };
    gr.faces.push(fce);
    gr.faces.len() - 1
}

/// Split a morphism along compositions, add the components to the graph, and
/// add a reflexivity face between the source morphism and the new path. Returns
/// the id of the new face
pub fn split(ctx: &mut Context, gr: &mut Graph, src: usize, mph: usize) -> usize {
    assert!(src < gr.nodes.len(), "src out of bounds");
    assert!(mph < gr.edges[src].len(), "mph out of bounds");
    let (mut path, dst) = insert_split_at(ctx, gr, src, gr.edges[src][mph].1.clone());
    path.reverse();
    let fce = Face {
        start: src,
        end: dst,
        left: vec![mph],
        right: path.clone(),
        eq: ctx.mk(crate::data::ActualEquality::Refl(
            gr.edges[src][mph].1.clone(),
        )),
    };
    gr.faces.push(fce);
    gr.faces.len() - 1
}

/// Split a morphism along composition and add its components to the graph. The
/// returned vector is in reverse order.
fn insert_split_at(
    ctx: &mut Context,
    gr: &mut Graph,
    src: usize,
    mph: Morphism,
) -> (Vec<usize>, usize) {
    use crate::data::ActualMorphism::Comp;
    match mph.deref() {
        Comp(l, r) => {
            let (mph_id, dst) = insert_mph_at(ctx, gr, src, l.clone());
            let (mut res, ndst) = insert_split_at(ctx, gr, dst, r.clone());
            res.push(mph_id);
            (res, ndst)
        }
        _ => {
            let (mph_id, ndst) = insert_mph_at(ctx, gr, src, mph);
            (vec![mph_id], ndst)
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
    use crate::tactics;

    #[test]
    fn split() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let v = obj!(ctx, (:1) in cat);
        let w = obj!(ctx, (:2) in cat);
        let x = obj!(ctx, (:3) in cat);
        let y = obj!(ctx, (:4) in cat);
        let z = obj!(ctx, (:5) in cat);
        let m1 = mph!(ctx, (:6) : v -> w);
        let m2 = mph!(ctx, (:7) : w -> x);
        let m3 = mph!(ctx, (:8) : x -> y);
        let m4 = mph!(ctx, (:9) : y -> z);

        let m = mph!(ctx, m1 >> (m2 >> (m3 >> m4)));
        assert!(m.check(&ctx), "m should be a valid morphism");

        let mut gr = Graph {
            nodes: vec![v, z],
            edges: vec![vec![(1, m)], vec![]],
            faces: vec![],
        };
        tactics::split(&mut ctx, &mut gr, 0, 0);

        assert!(gr.check(&ctx), "Graph is not valid after split");
        assert_eq!(gr.nodes.len(), 5, "There should be 5 nodes now");
        assert_eq!(
            gr.edges[0].len(),
            2,
            "There should be two outgoing edges from first node"
        );
        assert_eq!(gr.faces.len(), 1, "There should be a face");
    }

    #[test]
    fn split_norm() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let v = obj!(ctx, (:1) in cat);
        let w = obj!(ctx, (:2) in cat);
        let x = obj!(ctx, (:3) in cat);
        let y = obj!(ctx, (:4) in cat);
        let z = obj!(ctx, (:5) in cat);
        let m1 = mph!(ctx, (:6) : v -> w);
        let m2 = mph!(ctx, (:7) : w -> x);
        let m3 = mph!(ctx, (:8) : x -> y);
        let m4 = mph!(ctx, (:9) : y -> z);

        let m = mph!(ctx, (m1 >> m2) >> (m3 >> m4));
        assert!(m.check(&ctx), "m should be a valid morphism");

        let mut gr = Graph {
            nodes: vec![v, z],
            edges: vec![vec![(1, m)], vec![]],
            faces: vec![],
        };
        tactics::split_norm(&mut ctx, &mut gr, 0, 0);

        assert!(gr.check(&ctx), "Graph is not valid after split");
        assert_eq!(gr.nodes.len(), 5, "There should be 5 nodes now");
        assert_eq!(
            gr.edges[0].len(),
            2,
            "There should be two outgoing edges from first node"
        );
        assert_eq!(gr.faces.len(), 1, "There should be a face");
    }
}
