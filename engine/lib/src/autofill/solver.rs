use crate::autofill::hooks;
use crate::autofill::union_find::UF;
use crate::graph::eq::Eq;
use crate::graph::Graph;
use crate::remote::TermEngine;

/// Use a union find based algorithm to try and find an equality for the face /
//given by its index. In case of success, / returns the constructed equality.
//mask decides which equality / will be used when solving. If mask[face] ==
//true, the solution found will / probably not be interesting.
pub fn solve<NL, EL, FL, R: TermEngine>(
    ctx: &mut R,
    gr: &Graph<NL, EL, FL>,
    mask: &Vec<bool>,
    face: usize,
    max_size: usize,
) -> Option<Eq> {
    assert_eq!(mask.len(), gr.faces.len());
    let cat = gr.nodes[gr.faces[face].start].1;
    let paths = gr.enumerate(ctx, max_size);
    let mut uf = UF::new(&paths.paths);
    setup_hooks(&mut uf, ctx, gr);
    for fce in 0..gr.faces.len() {
        if !mask[fce] {
            continue;
        }
        let eq = gr.faces[fce].eq.get_repr(ctx);
        uf.connect(ctx, &paths, eq);
    }

    let face = &gr.faces[face].eq;
    let m1 = paths.get(cat, &face.inp.get_repr(ctx));
    let m2 = paths.get(cat, &face.outp.get_repr(ctx));
    match (m1, m2) {
        (Some(m1), Some(m2)) => uf.query(m1, m2),
        _ => {
            if m1.is_none() {
                log::debug!("Couldn't find left part of goal in enumeration");
            }
            if m2.is_none() {
                log::debug!("Couldn't find right part of goal in enumeration");
            }
            None
        }
    }
}

fn setup_hooks<NL, EL, FL, R: TermEngine>(uf: &mut UF<R>, ctx: &mut R, gr: &Graph<NL, EL, FL>) {
    for src in 0..gr.nodes.len() {
        for m in 0..gr.edges[src].len() {
            let mph = gr.edges[src][m].3.get_repr(ctx);
            uf.register_hook(move |ctx, eq, opts| hooks::precompose::hook(&mph, ctx, eq, opts));
            let mph = gr.edges[src][m].3.get_repr(ctx);
            uf.register_hook(move |ctx, eq, opts| hooks::postcompose::hook(&mph, ctx, eq, opts));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::autofill::solve;
    use crate::data::EvarStatus::Grounded;
    use crate::data::Feature;
    use crate::graph::eq::{Eq, Morphism};
    use crate::graph::{Face, Graph};
    use crate::remote::Mock;
    use crate::vm::Context;

    #[test]
    fn basic() {
        let mut ctx = Mock::new();
        let cat = ctx.new_term("C".to_string(), None, Grounded);
        ctx.add_feat(cat, Feature::Category);
        let x = ctx.new_term("x".to_string(), None, Grounded);
        ctx.add_feat(x, Feature::Object { cat });
        let y = ctx.new_term("y".to_string(), None, Grounded);
        ctx.add_feat(y, Feature::Object { cat });
        let m1 = ctx.new_term("m1".to_string(), None, Grounded);
        ctx.add_feat(
            m1,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        let idx = ctx.new_term("1".to_string(), None, Grounded);
        ctx.add_feat(
            idx,
            Feature::Morphism {
                cat,
                src: x,
                dst: x,
            },
        );
        ctx.add_feat(idx, Feature::Identity { cat, obj: x });
        let idy = ctx.new_term("1".to_string(), None, Grounded);
        ctx.add_feat(
            idy,
            Feature::Morphism {
                cat,
                src: y,
                dst: y,
            },
        );
        ctx.add_feat(idy, Feature::Identity { cat, obj: y });
        let m1__ = ctx.new_term("1 o m1".to_string(), None, Grounded);
        ctx.add_feat(
            m1__,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        ctx.add_feat(
            m1__,
            Feature::ComposeMph {
                cat,
                src: x,
                mid: y,
                dst: y,
                m1,
                m2: idy,
            },
        );
        let m1_ = ctx.new_term("(1 o m1) o 1".to_string(), None, Grounded);
        ctx.add_feat(
            m1_,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        ctx.add_feat(
            m1_,
            Feature::ComposeMph {
                cat,
                src: x,
                mid: x,
                dst: y,
                m1: idx,
                m2: m1__,
            },
        );
        let m2 = ctx.new_term("m2".to_string(), None, Grounded);
        ctx.add_feat(
            m2,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        let m3 = ctx.new_term("m3".to_string(), None, Grounded);
        ctx.add_feat(
            m3,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        let m3_ = ctx.new_term("1 o m3".to_string(), None, Grounded);
        ctx.add_feat(
            m3_,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        ctx.add_feat(
            m3_,
            Feature::ComposeMph {
                cat,
                src: x,
                mid: y,
                dst: y,
                m1: m3,
                m2: idy,
            },
        );
        let m4 = ctx.new_term("m4".to_string(), None, Grounded);
        ctx.add_feat(
            m4,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );
        let m5 = ctx.new_term("m5".to_string(), None, Grounded);
        ctx.add_feat(
            m5,
            Feature::Morphism {
                cat,
                src: x,
                dst: y,
            },
        );

        let mph1 = Morphism::atom(x, y, m1);
        let mph2 = Morphism::atom(x, y, m2);
        let mph3 = Morphism::atom(x, y, m3);
        let mph4 = Morphism::atom(x, y, m4);
        let mph5 = Morphism::atom(x, y, m5);

        let eq12 = ctx.new_term("H12".to_string(), None, Grounded);
        ctx.add_feat(
            eq12,
            Feature::Equality {
                cat,
                src: x,
                dst: y,
                left: m1_,
                right: m2,
            },
        );
        let h12 = Eq::atomic(cat, mph1.clone(), mph2.clone(), eq12);
        let eq13 = ctx.new_term("H13".to_string(), None, Grounded);
        ctx.add_feat(
            eq12,
            Feature::Equality {
                cat,
                src: x,
                dst: y,
                left: m1,
                right: m3,
            },
        );
        let h13 = Eq::atomic(cat, mph1.clone(), mph3.clone(), eq13);
        let eq23 = ctx.new_term("H23".to_string(), None, Grounded);
        ctx.add_feat(
            eq23,
            Feature::Equality {
                cat,
                src: x,
                dst: y,
                left: m2,
                right: m3_,
            },
        );
        let h23 = Eq::atomic(cat, mph2.clone(), mph3.clone(), eq23);
        let eq45 = ctx.new_term("H45".to_string(), None, Grounded);
        ctx.add_feat(
            eq45,
            Feature::Equality {
                cat,
                src: x,
                dst: y,
                left: m4,
                right: m5,
            },
        );
        let h45 = Eq::atomic(cat, mph4.clone(), mph5.clone(), eq45);

        let fce12 = Face {
            start: 0,
            end: 1,
            left: vec![0],
            right: vec![1],
            eq: h12,
            label: (),
        };
        let fce13 = Face {
            start: 0,
            end: 1,
            left: vec![0],
            right: vec![2],
            eq: h13,
            label: (),
        };
        let fce23 = Face {
            start: 0,
            end: 1,
            left: vec![1],
            right: vec![2],
            eq: h23,
            label: (),
        };
        let fce45 = Face {
            start: 0,
            end: 1,
            left: vec![3],
            right: vec![4],
            eq: h45,
            label: (),
        };

        let gr = Graph {
            nodes: vec![(x, cat, ()), (y, cat, ())],
            edges: vec![
                vec![
                    (1, (), m1.clone(), mph1.clone()),
                    (1, (), m2.clone(), mph2.clone()),
                    (1, (), m3.clone(), mph3.clone()),
                    (1, (), m3_.clone(), mph3.clone()),
                    (1, (), m4.clone(), mph4.clone()),
                    (1, (), m5.clone(), mph5.clone()),
                ],
                Vec::new(),
            ],
            faces: vec![fce23, fce12, fce13, fce45],
        };

        let mut ctx = Context::new(ctx);
        let mask = vec![false, true, true, true];
        let result = solve(&mut ctx, &gr, &mask, 0, 1);
        assert!(result.is_some(), "Solving should succeed");
    }

    #[test]
    fn precompose() {
        let mut ctx = Mock::new();
        let cat = ctx.new_term("C".to_string(), None, Grounded);
        ctx.add_feat(cat, Feature::Category);
        let a = ctx.new_term("a".to_string(), None, Grounded);
        ctx.add_feat(a, Feature::Object { cat });
        let b = ctx.new_term("b".to_string(), None, Grounded);
        ctx.add_feat(b, Feature::Object { cat });
        let c = ctx.new_term("c".to_string(), None, Grounded);
        ctx.add_feat(c, Feature::Object { cat });
        let m = ctx.new_term("m".to_string(), None, Grounded);
        ctx.add_feat(
            m,
            Feature::Morphism {
                cat,
                src: a,
                dst: b,
            },
        );
        let m1 = ctx.new_term("m1".to_string(), None, Grounded);
        ctx.add_feat(
            m1,
            Feature::Morphism {
                cat,
                src: b,
                dst: c,
            },
        );
        let m2 = ctx.new_term("m2".to_string(), None, Grounded);
        ctx.add_feat(
            m2,
            Feature::Morphism {
                cat,
                src: b,
                dst: c,
            },
        );
        let p1 = ctx.new_term("m1 o m".to_string(), None, Grounded);
        ctx.add_feat(
            p1,
            Feature::Morphism {
                cat,
                src: a,
                dst: c,
            },
        );
        ctx.add_feat(
            p1,
            Feature::ComposeMph {
                cat,
                src: a,
                mid: b,
                dst: c,
                m1: m,
                m2: m1,
            },
        );
        let p2 = ctx.new_term("m2 o m".to_string(), None, Grounded);
        ctx.add_feat(
            p2,
            Feature::Morphism {
                cat,
                src: a,
                dst: c,
            },
        );
        ctx.add_feat(
            p2,
            Feature::ComposeMph {
                cat,
                src: a,
                mid: b,
                dst: c,
                m1: m,
                m2,
            },
        );

        let mph = Morphism::atom(a, b, m);
        let mph1 = Morphism::atom(b, c, m1);
        let mph2 = Morphism::atom(b, c, m2);
        let mphm1 = Morphism {
            src: a,
            dst: c,
            comps: vec![(a, b, m), (b, c, m1)],
        };
        let mphm2 = Morphism {
            src: a,
            dst: c,
            comps: vec![(a, b, m), (b, c, m2)],
        };

        let eq12 = ctx.new_term("H12".to_string(), None, Grounded);
        ctx.add_feat(
            eq12,
            Feature::Equality {
                cat,
                src: b,
                dst: c,
                left: m1,
                right: m2,
            },
        );
        let h12 = Eq::atomic(cat, mph1.clone(), mph2.clone(), eq12);
        let eq_goal = ctx.new_term("?0".to_string(), None, Grounded);
        ctx.add_feat(
            eq_goal,
            Feature::Equality {
                cat,
                src: a,
                dst: c,
                left: p1,
                right: p2,
            },
        );
        let goal = Eq::atomic(cat, mphm1, mphm2, eq_goal);

        let fce = Face {
            start: 1,
            end: 2,
            left: vec![0],
            right: vec![1],
            eq: h12,
            label: (),
        };
        let exist = Face {
            start: 0,
            end: 2,
            left: vec![0, 0],
            right: vec![0, 1],
            eq: goal,
            label: (),
        };

        let gr = Graph {
            nodes: vec![(a, cat, ()), (b, cat, ()), (c, cat, ())],
            edges: vec![
                vec![(1, (), m, mph)],
                vec![(2, (), m1, mph1), (2, (), m2, mph2)],
                vec![],
            ],
            faces: vec![exist, fce],
        };
        let mask = vec![false, true];

        let mut ctx = Context::new(ctx);
        let result = solve(&mut ctx, &gr, &mask, 0, 2);
        assert!(result.is_some(), "Solving should succeed");
    }

    #[test]
    fn postcompose() {
        let mut ctx = Mock::new();
        let cat = ctx.new_term("C".to_string(), None, Grounded);
        ctx.add_feat(cat, Feature::Category);
        let a = ctx.new_term("a".to_string(), None, Grounded);
        ctx.add_feat(a, Feature::Object { cat });
        let b = ctx.new_term("b".to_string(), None, Grounded);
        ctx.add_feat(b, Feature::Object { cat });
        let c = ctx.new_term("c".to_string(), None, Grounded);
        ctx.add_feat(c, Feature::Object { cat });
        let m1 = ctx.new_term("m1".to_string(), None, Grounded);
        ctx.add_feat(
            m1,
            Feature::Morphism {
                cat,
                src: a,
                dst: b,
            },
        );
        let m2 = ctx.new_term("m2".to_string(), None, Grounded);
        ctx.add_feat(
            m2,
            Feature::Morphism {
                cat,
                src: a,
                dst: b,
            },
        );
        let m = ctx.new_term("m".to_string(), None, Grounded);
        ctx.add_feat(
            m,
            Feature::Morphism {
                cat,
                src: b,
                dst: c,
            },
        );
        let p1 = ctx.new_term("m o m1".to_string(), None, Grounded);
        ctx.add_feat(
            p1,
            Feature::Morphism {
                cat,
                src: a,
                dst: c,
            },
        );
        ctx.add_feat(
            p1,
            Feature::ComposeMph {
                cat,
                src: a,
                mid: b,
                dst: c,
                m1,
                m2: m,
            },
        );
        let p2 = ctx.new_term("m o m2".to_string(), None, Grounded);
        ctx.add_feat(
            p2,
            Feature::Morphism {
                cat,
                src: a,
                dst: c,
            },
        );
        ctx.add_feat(
            p2,
            Feature::ComposeMph {
                cat,
                src: a,
                mid: b,
                dst: c,
                m1: m2,
                m2: m,
            },
        );

        let mph = Morphism::atom(b, c, m);
        let mph1 = Morphism::atom(a, b, m1);
        let mph2 = Morphism::atom(a, b, m2);
        let mphm1 = Morphism {
            src: a,
            dst: c,
            comps: vec![(a, b, m1), (b, c, m)],
        };
        let mphm2 = Morphism {
            src: a,
            dst: c,
            comps: vec![(a, b, m2), (b, c, m)],
        };

        let eq12 = ctx.new_term("H12".to_string(), None, Grounded);
        ctx.add_feat(
            eq12,
            Feature::Equality {
                cat,
                src: a,
                dst: b,
                left: m1,
                right: m2,
            },
        );
        let h12 = Eq::atomic(cat, mph1.clone(), mph2.clone(), eq12);
        let eq_goal = ctx.new_term("?0".to_string(), None, Grounded);
        ctx.add_feat(
            eq_goal,
            Feature::Equality {
                cat,
                src: a,
                dst: c,
                left: p1,
                right: p2,
            },
        );
        let goal = Eq::atomic(cat, mphm1, mphm2, eq_goal);

        let fce = Face {
            start: 0,
            end: 1,
            left: vec![0],
            right: vec![1],
            eq: h12,
            label: (),
        };
        let exist = Face {
            start: 0,
            end: 2,
            left: vec![0, 0],
            right: vec![1, 0],
            eq: goal,
            label: (),
        };

        let gr = Graph {
            nodes: vec![(a, cat, ()), (b, cat, ()), (c, cat, ())],
            edges: vec![
                vec![(1, (), m1, mph1), (1, (), m2, mph2)],
                vec![(2, (), m, mph)],
                vec![],
            ],
            faces: vec![exist, fce],
        };
        let mask = vec![false, true];

        let mut ctx = Context::new(ctx);
        let result = solve(&mut ctx, &gr, &mask, 0, 2);
        assert!(result.is_some(), "Solving should succeed");
    }
}
