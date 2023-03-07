use crate::data::{ActualEquality, ActualMorphism, ActualObject};
use crate::data::{Context, Equality, Functor, Morphism, Object};
use crate::dsl::{eq, mph, obj};
use std::ops::Deref;

pub fn morphism(ctx: &mut Context, mph: Morphism) -> (Morphism, Equality) {
    let mut functs: Vec<Functor> = Vec::new();
    norm_under_functors(ctx, &mut functs, mph)
}

// Given a list of functors and a morphism, give the image of the morphism
// by the composition of functors
fn apply_functors(ctx: &mut Context, functs: &[Functor], m: Morphism) -> Morphism {
    functs
        .iter()
        .rev()
        .fold(m, |m, f| ctx.mk(ActualMorphism::Funct(f.clone(), m)))
}

// The equality f_n (... (f_1 (id_e))) = id_(f_n (... (f_1 e)))
fn raise_identity(ctx: &mut Context, functs: &[Functor], e: Object) -> (Object, Equality) {
    functs
        .iter()
        .rev()
        .fold((e.clone(), eq!(ctx, 1_((id e)))), |acc, f| {
            (
                ctx.mk(ActualObject::Funct(f.clone(), acc.0.clone())),
                ctx.mk(ActualEquality::Concat(
                    ctx.mk(ActualEquality::FunctCtx(f.clone(), acc.1)),
                    ctx.mk(ActualEquality::FunctId(f.clone(), acc.0)),
                )),
            )
        })
}

// Same with composition
fn raise_composition(
    ctx: &mut Context,
    functs: &[Functor],
    m1: Morphism,
    m2: Morphism,
) -> (Morphism, Morphism, Equality) {
    functs.iter().rev().fold(
        (
            m1.clone(),
            m2.clone(),
            ctx.mk(ActualEquality::Refl(ctx.mk(ActualMorphism::Comp(m1, m2)))),
        ),
        |acc, f| {
            (
                ctx.mk(ActualMorphism::Funct(f.clone(), acc.0.clone())),
                ctx.mk(ActualMorphism::Funct(f.clone(), acc.1.clone())),
                ctx.mk(ActualEquality::Concat(
                    ctx.mk(ActualEquality::FunctCtx(f.clone(), acc.2)),
                    ctx.mk(ActualEquality::FunctComp(f.clone(), acc.0, acc.1)),
                )),
            )
        },
    )
}

// (m11 >> m12 >> ... >> m1n) >> (m21 >> ... >> m2m) -> (m11 >> ... >> m1n >> m21 >> ... >> m2m)
// Assumes m and post are normalized
fn post_compose(ctx: &mut Context, m: Morphism, post: Morphism) -> (Morphism, Equality) {
    use ActualEquality::*;
    use ActualMorphism::*;
    match m.deref() {
        Identity(_) => (post.clone(), ctx.mk(LeftId(post))),
        Comp(m1, m2) => {
            let (r, req) = post_compose(ctx, m2.clone(), post.clone());
            let eq = ctx.mk(Concat(
                ctx.mk(Assoc(m1.clone(), m2.clone(), post.clone())),
                ctx.mk(LAp(m1.clone(), req)),
            ));
            match r.deref() {
                Identity(_) => (m1.clone(), ctx.mk(Concat(eq, ctx.mk(RightId(m1.clone()))))),
                _ => (ctx.mk(Comp(m1.clone(), r)), eq),
            }
        }
        _ => match post.deref() {
            Identity(_) => (m.clone(), ctx.mk(RightId(m))),
            _ => {
                let m = ctx.mk(Comp(m, post));
                (m.clone(), ctx.mk(Refl(m)))
            }
        },
    }
}

fn norm_under_functors(
    ctx: &mut Context,
    functs: &mut Vec<Functor>,
    m: Morphism,
) -> (Morphism, Equality) {
    use ActualMorphism::*;
    match m.deref() {
        Identity(e) => {
            let (e, eq) = raise_identity(ctx, &functs, e.clone());
            assert!(eq.check(&ctx), "ID");
            (ctx.mk(Identity(e)), eq)
        }
        Comp(m1, m2) => {
            let (_, _, eq) = raise_composition(ctx, &functs, m1.clone(), m2.clone());
            assert!(eq.check(&ctx), "EQ");
            let (m1, eq1) = norm_under_functors(ctx, functs, m1.clone());
            assert!(eq1.check(&ctx), "EQ1");
            let (m2, eq2) = norm_under_functors(ctx, functs, m2.clone());
            assert!(eq2.check(&ctx), "EQ2");
            let eq = ctx.mk(ActualEquality::Concat(
                eq,
                ctx.mk(ActualEquality::Compose(eq1, eq2)),
            ));
            assert!(eq.check(&ctx), "A");
            let (r, req) = post_compose(ctx, m1, m2);
            assert!(req.check(&ctx), "REQ");
            assert_eq!(eq.right(&ctx), req.left(&ctx), "Incompatible");
            (r, ctx.mk(ActualEquality::Concat(eq, req)))
        }
        Funct(f, m) => {
            functs.push(f.clone());
            let norm = norm_under_functors(ctx, functs, m.clone());
            functs.pop();
            norm
        }
        _ => {
            let m = apply_functors(ctx, &functs, m);
            assert!(m.check(&ctx), "MPH");
            (m.clone(), ctx.mk(ActualEquality::Refl(m)))
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::data::Context;
    use crate::dsl::{cat, funct, mph, obj};
    use crate::normalize::{
        apply_functors, morphism, post_compose, raise_composition, raise_identity,
    };

    #[test]
    pub fn raise_identity_test() {
        let mut ctx = Context::new();
        let cat0 = cat!(ctx, :0);
        let cat1 = cat!(ctx, :1);
        let cat2 = cat!(ctx, :2);
        let cat3 = cat!(ctx, :3);
        let cat4 = cat!(ctx, :4);
        let cat5 = cat!(ctx, :5);
        let functs = vec![
            funct!(ctx, (:10) : cat4 => cat5),
            funct!(ctx, (:9) : cat3 => cat4),
            funct!(ctx, (:8) : cat2 => cat3),
            funct!(ctx, (:7) : cat1 => cat2),
            funct!(ctx, (:6) : cat0 => cat1),
        ];
        let e = obj!(ctx, (:11) in cat0);
        let o = obj!(ctx, (functs[0]) _0
                          ((functs[1]) _0
                          ((functs[2]) _0
                          ((functs[3]) _0
                          ((functs[4]) _0 e)))));
        let id = mph!(ctx, id e);
        let f_id = apply_functors(&mut ctx, &functs, id);
        let id_f = mph!(ctx, id o);
        let (r, req) = raise_identity(&mut ctx, &functs, e);
        assert!(r.check(&ctx), "Invalid object returned");
        assert!(req.check(&ctx), "Invalid equality returned");
        assert_eq!(r, o, "Expected functors applied to element");
        assert_eq!(r, req.src(&ctx), "Expected equality from r");
        assert_eq!(r, req.dst(&ctx), "Expected equality to r");
        assert_eq!(
            req.left(&ctx),
            f_id,
            "Expected functors applied to equality"
        );
        assert_eq!(
            req.right(&ctx),
            id_f,
            "Expected identity of functors of element"
        );
    }

    #[test]
    pub fn raise_composition_test() {
        let mut ctx = Context::new();
        let cat0 = cat!(ctx, :0);
        let cat1 = cat!(ctx, :1);
        let cat2 = cat!(ctx, :2);
        let cat3 = cat!(ctx, :3);
        let cat4 = cat!(ctx, :4);
        let cat5 = cat!(ctx, :5);
        let functs = vec![
            funct!(ctx, (:10) : cat4 => cat5),
            funct!(ctx, (:9) : cat3 => cat4),
            funct!(ctx, (:8) : cat2 => cat3),
            funct!(ctx, (:7) : cat1 => cat2),
            funct!(ctx, (:6) : cat0 => cat1),
        ];
        let x = obj!(ctx, (:11) in cat0);
        let y = obj!(ctx, (:12) in cat0);
        let z = obj!(ctx, (:13) in cat0);
        let f = mph!(ctx, (:14) : x -> y);
        let g = mph!(ctx, (:15) : y -> z);
        let m = mph!(ctx, f >> g);
        let (r_f, r_g, eq) = raise_composition(&mut ctx, &functs, f.clone(), g.clone());
        let f_f = apply_functors(&mut ctx, &functs, f);
        let f_g = apply_functors(&mut ctx, &functs, g);
        let m_f = mph!(ctx, f_f >> f_g);
        let f_m = apply_functors(&mut ctx, &functs, m);

        assert!(r_f.check(&ctx), "Invalid f returned");
        assert!(r_g.check(&ctx), "Invalid g returned");
        assert!(eq.check(&ctx), "Invalid eq returned");
        assert_eq!(r_f, f_f, "Expected functors applied to f");
        assert_eq!(r_g, f_g, "Expected functors applied to g");
        assert_eq!(
            eq.left(&ctx),
            f_m,
            "Expected equality from functors of composition"
        );
        assert_eq!(
            eq.right(&ctx),
            m_f,
            "Expected equality to composition of functors"
        );
    }

    #[test]
    pub fn post_compose_test() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let u = obj!(ctx, (:1) in cat);
        let v = obj!(ctx, (:2) in cat);
        let w = obj!(ctx, (:3) in cat);
        let x = obj!(ctx, (:4) in cat);
        let y = obj!(ctx, (:5) in cat);
        let z = obj!(ctx, (:6) in cat);
        let m1 = mph!(ctx, (:7) : u -> v);
        let m2 = mph!(ctx, (:8) : v -> w);
        let m3 = mph!(ctx, (:9) : w -> x);
        let m4 = mph!(ctx, (:10) : x -> y);
        let m5 = mph!(ctx, (:11) : y -> z);
        let m123 = mph!(ctx, m1 >> (m2 >> m3));
        let m45 = mph!(ctx, m4 >> m5);
        let comp = mph!(ctx, m123 >> m45);
        let m12345 = mph!(ctx, m1 >> (m2 >> (m3 >> (m4 >> m5))));
        let (pc, pceq) = post_compose(&mut ctx, m123, m45);

        assert!(pc.check(&ctx), "Invalid morphism returned");
        assert!(pceq.check(&ctx), "Invalid equality returned");
        assert_eq!(pc, m12345, "Wrong composite returned");
        assert_eq!(
            pceq.left(&ctx),
            comp,
            "Expected equality from naive compositon"
        );
        assert_eq!(
            pceq.right(&ctx),
            m12345,
            "Expected equality to flat composition"
        );
    }

    #[test]
    pub fn normalize_test() {
        let mut ctx = Context::new();
        let cat0 = cat!(ctx, :0);
        let cat1 = cat!(ctx, :1);
        let x = obj!(ctx, (:2) in cat0);
        let y = obj!(ctx, (:3) in cat0);
        let z = obj!(ctx, (:4) in cat1);
        let f = funct!(ctx, (:5) : cat0 => cat1);
        let u = mph!(ctx, (:6) : x -> y);
        let v = mph!(ctx, (:7) : (f _0 y) -> z);
        let m =
            mph!(ctx, ((f _1 ((id x) >> (u >> (id y)))) >> (f _1 (id y))) >> ((id (f _0 y)) >> v));
        assert!(m.check(&ctx), "Test faulty");
        let expected = mph!(ctx, (f _1 u) >> v);
        let (norm, eq) = morphism(&mut ctx, m.clone());

        assert!(norm.check(&ctx), "Invalid morphism returned");
        assert!(eq.check(&ctx), "Invalid equality returned");
        assert_eq!(norm, expected, "Wrong morphism returned");
        assert_eq!(eq.left(&ctx), m, "Expected equality from source morphism");
        assert_eq!(
            eq.right(&ctx),
            expected,
            "Expected equality to normalized morphism"
        );
    }

    #[test]
    pub fn normalize_identity() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let x = obj!(ctx, (:1) in cat);
        let y = obj!(ctx, (:2) in cat);
        let m = mph!(ctx, (:3) : x -> y);
        let mph = mph!(ctx, (id x) >> (m >> (id y)));
        assert!(mph.check(&ctx), "Test faulty");

        let (norm, eq) = morphism(&mut ctx, mph.clone());
        assert!(norm.check(&ctx), "Invalid morphism returned");
        assert!(eq.check(&ctx), "Invalid equality returned");
        assert_eq!(norm, m, "Faulty normalisation");
        assert_eq!(eq.left(&ctx), mph, "Expected equality from source morphism");
        assert_eq!(
            eq.right(&ctx),
            norm,
            "Expected equality to normalized morphism"
        );
    }
}
