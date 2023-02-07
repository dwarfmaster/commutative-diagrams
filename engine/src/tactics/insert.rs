use crate::data::{Context, Morphism, Object};
use crate::graph::Graph;

/// Look over graph nodes, if obj is already present returns its index,
/// otherwise insert it and return the index of the nely inserted node.
pub fn insert_node<NL, EL, FL>(gr: &mut Graph<NL, EL, FL>, obj: Object) -> usize
where
    NL: Default,
{
    for n in 0..gr.nodes.len() {
        if gr.nodes[n].0 == obj {
            return n;
        }
    }
    gr.nodes.push((obj, Default::default()));
    gr.edges.push(Vec::new());
    gr.nodes.len() - 1
}

/// Try to find the morphism in the output edges of node, and return its index.
/// Otherwise add it and return its new index. Also return the index of the
/// codomain of the morphism.
pub fn insert_mph_at<NL, EL, FL>(
    ctx: &mut Context,
    gr: &mut Graph<NL, EL, FL>,
    node: usize,
    mph: Morphism,
) -> (usize, usize)
where
    NL: Default,
    EL: Default,
{
    assert!(node < gr.nodes.len(), "Trying to insert at unexisting node");
    for m in 0..gr.edges[node].len() {
        if gr.edges[node][m].2 == mph {
            return (m, gr.edges[node][m].0);
        }
    }
    let dst = mph.dst(ctx);
    let ndst = insert_node(gr, dst);
    gr.edges[node].push((ndst, Default::default(), mph));
    (gr.edges[node].len() - 1, ndst)
}

/// Same as insert_mph_at, but finds or insert automatically the source of the
/// morhism. Returns the index of the source node, that of the morphism and that
/// of the destination.
pub fn insert_mph<NL, EL, FL>(
    ctx: &mut Context,
    gr: &mut Graph<NL, EL, FL>,
    mph: Morphism,
) -> (usize, usize, usize)
where
    NL: Default,
    EL: Default,
{
    let src = mph.src(ctx);
    let nsrc = insert_node(gr, src);
    let (nmph, ndst) = insert_mph_at(ctx, gr, nsrc, mph);
    (nsrc, nmph, ndst)
}

#[cfg(test)]
mod tests {
    use crate::data::{ActualCategory, ActualMorphism, ActualObject};
    use crate::data::{CategoryData, MorphismData, ObjectData};
    use crate::data::{Context, ProofObject};
    use crate::dsl::{cat, mph, obj};
    use crate::graph::Graph;
    use crate::tactics::{insert_mph, insert_mph_at};

    #[test]
    fn basic() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let x = obj!(ctx, (:1) in cat);
        let y = obj!(ctx, (:2) in cat);
        let m1 = mph!(ctx, (:3) : x -> y);
        let m2 = mph!(ctx, (:4) : x -> y);

        let mut gr: Graph<(), (), ()> = Graph {
            nodes: Vec::new(),
            edges: Vec::new(),
            faces: Vec::new(),
        };

        let (m1_src, _, _) = insert_mph(&mut ctx, &mut gr, m1);
        assert_eq!(
            gr.nodes.len(),
            2,
            "There should be two nodes after the insertion"
        );
        assert_eq!(
            gr.edges.len(),
            2,
            "The len of edges should be the same as the number of nodes"
        );

        let (m2_id, _) = insert_mph_at(&mut ctx, &mut gr, m1_src, m2.clone());
        assert!(gr.check(&ctx), "The graph should still be valid");
        assert_eq!(gr.nodes.len(), 2, "There should still be 2 nodes");
        assert_eq!(
            gr.edges[m1_src].len(),
            2,
            "There should be 2 outgoing edges"
        );

        let (m2_src, m2_id2, _) = insert_mph(&mut ctx, &mut gr, m2);
        assert_eq!(m2_src, m1_src, "m1 and m2 have the same source");
        assert_eq!(m2_id, m2_id2, "m2 was already present in the graph");
    }
}
