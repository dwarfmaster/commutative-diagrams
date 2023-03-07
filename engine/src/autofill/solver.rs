use crate::anyterm::IsTerm;
use crate::autofill::hooks;
use crate::autofill::union_find::UF;
use crate::data::{ActualEquality, Context};
use crate::dsl::eq;
use crate::graph::Graph;
use crate::normalize;
use crate::substitution::Substitution;
use crate::unification::unify;

/// Use a union find based algorithm to try and find an equality for the face
/// given by its index. In case of success, unify the found equality with the
/// one of the face and return the substitution.
pub fn solve<NL, EL, FL>(
    ctx: &mut Context,
    gr: &Graph<NL, EL, FL>,
    face: usize,
    max_size: usize,
) -> Option<Substitution> {
    let paths = gr.enumerate(ctx, max_size);
    let mut uf = UF::new(ctx, &paths.paths);
    setup_hooks(&mut uf, ctx, gr);
    for fce in 0..gr.faces.len() {
        if fce == face {
            continue;
        }
        let eq = gr.faces[fce].eq.clone();
        let (_, eql) = normalize::morphism(ctx, eq.left(ctx));
        let (_, eqr) = normalize::morphism(ctx, eq.right(ctx));
        uf.connect(ctx, &paths, eq!(ctx, (~eql) . (eq . eqr)));
    }

    // TODO guide solution along shape of face equality
    let face = &gr.faces[face].eq;
    let (left, eql) = normalize::morphism(ctx, face.left(ctx));
    let (right, eqr) = normalize::morphism(ctx, face.right(ctx));
    let m1 = paths.get(&left);
    let m2 = paths.get(&right);
    match (m1, m2) {
        (Some(m1), Some(m2)) => {
            let result = uf.query(ctx, m1, m2)?;
            let result = ctx.mk(ActualEquality::Concat(eql, result));
            let result = ctx.mk(ActualEquality::Concat(
                result,
                ctx.mk(ActualEquality::Inv(eqr)),
            ));
            assert!(result.check(ctx), "Invalid equality returned");
            unify(ctx, face.clone().term(), result.term())
        }
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

fn setup_hooks<NL, EL, FL>(uf: &mut UF, _ctx: &mut Context, gr: &Graph<NL, EL, FL>) {
    for src in 0..gr.nodes.len() {
        for m in 0..gr.edges[src].len() {
            let mph = gr.edges[src][m].2.clone();
            uf.register_hook(move |ctx, eq, opts| {
                hooks::precompose::hook(mph.clone(), ctx, eq, opts)
            });
            let mph = gr.edges[src][m].2.clone();
            uf.register_hook(move |ctx, eq, opts| {
                hooks::postcompose::hook(mph.clone(), ctx, eq, opts)
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::autofill::solve;
    use crate::data::Context;
    use crate::dsl::{cat, eq, mph, obj};
    use crate::graph::{Face, Graph};
    use crate::substitution::Substitutable;

    #[test]
    fn basic() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let x = obj!(ctx, (:1) in cat);
        let y = obj!(ctx, (:2) in cat);
        let m1 = mph!(ctx, (:3) : x -> y);
        let m1_ = mph!(ctx, (id x) >> (m1 >> (id y)));
        let m2 = mph!(ctx, (:4) : x -> y);
        let m3 = mph!(ctx, (:5) : x -> y);
        let m3_ = mph!(ctx, m3 >> (id y));
        let m4 = mph!(ctx, (:6) : x -> y);
        let m5 = mph!(ctx, (:7) : x -> y);
        let fce12 = Face {
            start: 0,
            end: 1,
            left: vec![0],
            right: vec![1],
            eq: eq!(ctx, (:8) : m1_ == m2),
            label: (),
        };
        let fce13 = Face {
            start: 0,
            end: 1,
            left: vec![0],
            right: vec![2],
            eq: eq!(ctx, (:9) : m1 == m3),
            label: (),
        };
        let fce23 = Face {
            start: 0,
            end: 1,
            left: vec![1],
            right: vec![2],
            eq: eq!(ctx, (?0) : m1 == m3_),
            label: (),
        };
        let fce45 = Face {
            start: 0,
            end: 1,
            left: vec![3],
            right: vec![4],
            eq: eq!(ctx, (:10) : m4 == m5),
            label: (),
        };

        let gr = Graph {
            nodes: vec![(x, ()), (y, ())],
            edges: vec![
                vec![
                    (1, (), m1.clone()),
                    (1, (), m2.clone()),
                    (1, (), m3.clone()),
                    (1, (), m3_.clone()),
                    (1, (), m4.clone()),
                    (1, (), m5.clone()),
                ],
                Vec::new(),
            ],
            faces: vec![fce23, fce12, fce13, fce45],
        };

        let result = solve(&mut ctx, &gr, 0, 1);
        assert!(result.is_some(), "Solving should succeed");
        let eq = gr.faces[0].eq.clone().subst(&ctx, &result.unwrap());
        assert!(eq.check(&ctx), "Resulting equality is incorrect");
    }

    #[test]
    fn precompose() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, (:0));
        let a = obj!(ctx, (:1) in cat);
        let b = obj!(ctx, (:2) in cat);
        let c = obj!(ctx, (:3) in cat);
        let m = mph!(ctx, (:4) : a -> b);
        let m1 = mph!(ctx, (:5) : b -> c);
        let m2 = mph!(ctx, (:6) : b -> c);
        let p1 = mph!(ctx, m >> m1);
        let p2 = mph!(ctx, m >> m2);

        let fce = Face {
            start: 1,
            end: 2,
            left: vec![0],
            right: vec![1],
            eq: eq!(ctx, (:7) : m1 == m2),
            label: (),
        };
        let exist = Face {
            start: 0,
            end: 2,
            left: vec![0, 0],
            right: vec![0, 1],
            eq: eq!(ctx, (?0) : p1 == p2),
            label: (),
        };

        let gr = Graph {
            nodes: vec![(a, ()), (b, ()), (c, ())],
            edges: vec![vec![(1, (), m)], vec![(2, (), m1), (2, (), m2)], vec![]],
            faces: vec![exist, fce],
        };

        let result = solve(&mut ctx, &gr, 0, 2);
        assert!(result.is_some(), "Solving should succeed");
        let eq = gr.faces[0].eq.clone().subst(&ctx, &result.unwrap());
        assert!(eq.check(&ctx), "Resulting equality is incorrect");
    }

    #[test]
    fn postcompose() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, (:0));
        let a = obj!(ctx, (:1) in cat);
        let b = obj!(ctx, (:2) in cat);
        let c = obj!(ctx, (:3) in cat);
        let m1 = mph!(ctx, (:4) : a -> b);
        let m2 = mph!(ctx, (:5) : a -> b);
        let m = mph!(ctx, (:6) : b -> c);
        let p1 = mph!(ctx, m1 >> m);
        let p2 = mph!(ctx, m2 >> m);

        let fce = Face {
            start: 0,
            end: 1,
            left: vec![0],
            right: vec![1],
            eq: eq!(ctx, (:7) : m1 == m2),
            label: (),
        };
        let exist = Face {
            start: 0,
            end: 2,
            left: vec![0, 0],
            right: vec![1, 0],
            eq: eq!(ctx, (?0) : p1 == p2),
            label: (),
        };

        let gr = Graph {
            nodes: vec![(a, ()), (b, ()), (c, ())],
            edges: vec![vec![(1, (), m1), (1, (), m2)], vec![(2, (), m)], vec![]],
            faces: vec![exist, fce],
        };

        let result = solve(&mut ctx, &gr, 0, 2);
        assert!(result.is_some(), "Solving should succeed");
        let eq = gr.faces[0].eq.clone().subst(&ctx, &result.unwrap());
        assert!(eq.check(&ctx), "Resulting equality is incorrect");
    }
}
