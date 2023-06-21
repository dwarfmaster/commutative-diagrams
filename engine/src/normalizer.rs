use crate::data::Feature;
use crate::remote::Remote;
use crate::remote::TermEngine;

pub fn morphism<R: TermEngine>(
    rm: &mut R,
    cat: u64,
    src: u64,
    dst: u64,
    mph: u64,
) -> (
    /*morphism*/ u64,
    /*equality*/ u64,
    /* components */ Vec<u64>,
) {
    let mut functs = Vec::new();
    norm_under_functors(rm, &mut functs, cat, src, dst, mph)
}

// Given a list of functors and a morphism, give the image of the morphism
// by the composition of functors
fn apply_functors<R: TermEngine>(
    rm: &mut R,
    functs: &[(u64, u64, u64)],
    cat: u64,
    src: u64,
    dst: u64,
    mph: u64,
) -> (u64, u64, u64, u64) {
    use Feature::*;
    functs
        .iter()
        .copied()
        .rev()
        .fold((cat, src, dst, mph), |(_, s, d, m), (scat, dcat, funct)| {
            let nsrc = rm
                .remote()
                .build(AppliedFunctObj {
                    scat,
                    dcat,
                    funct,
                    obj: s,
                })
                .unwrap();
            let ndst = rm
                .remote()
                .build(AppliedFunctObj {
                    scat,
                    dcat,
                    funct,
                    obj: d,
                })
                .unwrap();
            let nmph = rm
                .remote()
                .build(AppliedFunctMph {
                    scat,
                    dcat,
                    funct,
                    src: s,
                    dst: d,
                    mph: m,
                })
                .unwrap();
            (dcat, nsrc, ndst, nmph)
        })
}

// The equality f_n (... (f_1 (id_e))) = id_(f_n (... (f_1 e)))
fn raise_identity<R: TermEngine>(
    rm: &mut R,
    functs: &[(u64, u64, u64)],
    cat: u64,
    e: u64,
) -> (
    /*Object*/ u64,
    /*left*/ u64,
    /*right*/ u64,
    /*Equality*/ u64,
) {
    use Feature::*;
    let id = rm.remote().build(Identity { cat, obj: e }).unwrap();
    let rf = rm
        .remote()
        .build(Reflexivity {
            cat,
            src: e,
            dst: e,
            mph: id,
        })
        .unwrap();
    functs.iter().copied().rev().fold(
        (e, id, id, rf),
        |(e, left, right, eq), (scat, dcat, funct)| {
            let ne = rm
                .remote()
                .build(AppliedFunctObj {
                    scat,
                    dcat,
                    funct,
                    obj: e,
                })
                .unwrap();
            let nleft = rm
                .remote()
                .build(AppliedFunctMph {
                    scat,
                    dcat,
                    funct,
                    src: e,
                    dst: e,
                    mph: left,
                })
                .unwrap();
            let nright = rm.remote().build(Identity { cat: dcat, obj: ne }).unwrap();
            let rec = rm
                .remote()
                .build(AppliedFunctEq {
                    scat,
                    dcat,
                    funct,
                    src: ne,
                    dst: ne,
                    left,
                    right,
                    eq,
                })
                .unwrap();
            let fid = rm
                .remote()
                .build(FunctIdentity {
                    scat,
                    dcat,
                    funct,
                    obj: e,
                })
                .unwrap();
            let fright = rm
                .remote()
                .build(AppliedFunctMph {
                    scat,
                    dcat,
                    funct,
                    src: e,
                    dst: e,
                    mph: right,
                })
                .unwrap();
            let neq = rm
                .remote()
                .build(Concat {
                    cat: dcat,
                    src: ne,
                    dst: ne,
                    left: nleft,
                    mid: fright,
                    right: nright,
                    eq1: rec,
                    eq2: fid,
                })
                .unwrap();
            (ne, nleft, nright, neq)
        },
    )
}

// Same with composition
fn raise_composition<R: TermEngine>(
    rm: &mut R,
    functs: &[(u64, u64, u64)],
    cat: u64,
    src: u64,
    mid: u64,
    dst: u64,
    m1: u64,
    m2: u64,
) -> (
    /*dcat*/ u64,
    /*src*/ u64,
    /*mid*/ u64,
    /*dst*/ u64,
    /*m1*/ u64,
    /*m2*/ u64,
    /*left*/ u64,
    /*right*/ u64,
    /*eq*/ u64,
) {
    use Feature::*;
    let cmp = rm
        .remote()
        .build(ComposeMph {
            cat,
            src,
            mid,
            dst,
            m1,
            m2,
        })
        .unwrap();
    let rf = rm
        .remote()
        .build(Reflexivity {
            cat,
            src,
            dst,
            mph: cmp,
        })
        .unwrap();
    functs.iter().copied().rev().fold(
        (cat, src, mid, dst, m1, m2, cmp, cmp, rf),
        |(_, src, mid, dst, m1, m2, left, right, eq), (scat, dcat, funct)| {
            let nsrc = rm
                .remote()
                .build(AppliedFunctObj {
                    scat,
                    dcat,
                    funct,
                    obj: src,
                })
                .unwrap();
            let nmid = rm
                .remote()
                .build(AppliedFunctObj {
                    scat,
                    dcat,
                    funct,
                    obj: mid,
                })
                .unwrap();
            let ndst = rm
                .remote()
                .build(AppliedFunctObj {
                    scat,
                    dcat,
                    funct,
                    obj: dst,
                })
                .unwrap();
            let nm1 = rm
                .remote()
                .build(AppliedFunctMph {
                    scat,
                    dcat,
                    funct,
                    src,
                    dst: mid,
                    mph: m1,
                })
                .unwrap();
            let nm2 = rm
                .remote()
                .build(AppliedFunctMph {
                    scat,
                    dcat,
                    funct,
                    src: mid,
                    dst,
                    mph: m2,
                })
                .unwrap();
            let nleft = rm
                .remote()
                .build(AppliedFunctMph {
                    scat,
                    dcat,
                    funct,
                    src,
                    dst,
                    mph: left,
                })
                .unwrap();
            let nright = rm
                .remote()
                .build(ComposeMph {
                    cat: dcat,
                    src: nsrc,
                    mid: nmid,
                    dst: ndst,
                    m1: nm1,
                    m2: nm2,
                })
                .unwrap();
            let rec = rm
                .remote()
                .build(AppliedFunctEq {
                    scat,
                    dcat,
                    funct,
                    src,
                    dst,
                    left,
                    right,
                    eq,
                })
                .unwrap();
            let fcomp = rm
                .remote()
                .build(FunctComposition {
                    scat,
                    dcat,
                    funct,
                    src,
                    mid,
                    dst,
                    m1,
                    m2,
                })
                .unwrap();
            let fright = rm
                .remote()
                .build(AppliedFunctMph {
                    scat,
                    dcat,
                    funct,
                    src,
                    dst,
                    mph: right,
                })
                .unwrap();
            let neq = rm
                .remote()
                .build(Concat {
                    cat: dcat,
                    src: nsrc,
                    dst: ndst,
                    left: nleft,
                    mid: fright,
                    right: nright,
                    eq1: rec,
                    eq2: fcomp,
                })
                .unwrap();
            (dcat, nsrc, nmid, ndst, nm1, nm2, nleft, nright, neq)
        },
    )
}

// (m11 >> m12 >> ... >> m1n) >> (m21 >> ... >> m2m) -> (m11 >> ... >> m1n >> m21 >> ... >> m2m)
// Assumes m and post are normalized
fn post_compose<R: TermEngine>(
    rm: &mut R,
    cat: u64,
    src: u64,
    mid: u64,
    dst: u64,
    m: u64,
    post: u64,
) -> (/*morphism*/ u64, /*equality*/ u64) {
    use Feature::*;
    if let Some(_) = rm.is_identity(cat, m) {
        let lid = rm
            .remote()
            .build(LeftUnitality {
                cat,
                src: mid,
                dst,
                mph: post,
            })
            .unwrap();
        (post, lid)
    } else if let Some((_, mmid, _, m1, m2)) = rm.is_comp(cat, m) {
        let (r, req) = post_compose(rm, cat, mmid, mid, dst, m2, post);
        let assoc = rm
            .remote()
            .build(Associativity {
                cat,
                src,
                mid1: mmid,
                mid2: mid,
                dst,
                m1,
                m2,
                m3: post,
            })
            .unwrap();
        let cmp = rm
            .remote()
            .build(ComposeMph {
                cat,
                src: mmid,
                mid,
                dst,
                m1: m2,
                m2: post,
            })
            .unwrap();
        let lap = rm
            .remote()
            .build(LeftApplication {
                cat,
                src,
                mid: mmid,
                dst,
                mph: m1,
                left: cmp,
                right: r,
                eq: req,
            })
            .unwrap();
        let left = rm
            .remote()
            .build(ComposeMph {
                cat,
                src,
                mid,
                dst,
                m1: m,
                m2: post,
            })
            .unwrap();
        let mid = rm
            .remote()
            .build(ComposeMph {
                cat,
                src,
                mid: mmid,
                dst,
                m1,
                m2: cmp,
            })
            .unwrap();
        let right = rm
            .remote()
            .build(ComposeMph {
                cat,
                src,
                mid: mmid,
                dst,
                m1,
                m2: r,
            })
            .unwrap();
        let eq = rm
            .remote()
            .build(Concat {
                cat,
                src,
                dst,
                left,
                mid,
                right,
                eq1: assoc,
                eq2: lap,
            })
            .unwrap();
        if let Some(_) = rm.is_identity(cat, r) {
            let rid = rm
                .remote()
                .build(RightUnitality {
                    cat,
                    src,
                    dst: mmid,
                    mph: m1,
                })
                .unwrap();
            let eq = rm
                .remote()
                .build(Concat {
                    cat,
                    src,
                    dst,
                    left,
                    mid: right,
                    right: m1,
                    eq1: eq,
                    eq2: rid,
                })
                .unwrap();
            (m1, eq)
        } else {
            (right, eq)
        }
    } else if let Some(_) = rm.is_identity(cat, post) {
        let eq = rm
            .remote()
            .build(RightUnitality {
                cat,
                src,
                dst: mid,
                mph: m,
            })
            .unwrap();
        (m, eq)
    } else {
        let m = rm
            .remote()
            .build(ComposeMph {
                cat,
                src,
                mid,
                dst,
                m1: m,
                m2: post,
            })
            .unwrap();
        let rfl = rm
            .remote()
            .build(Reflexivity {
                cat,
                src,
                dst,
                mph: m,
            })
            .unwrap();
        (m, rfl)
    }
}

fn norm_under_functors<R: TermEngine>(
    rm: &mut R,
    functs: &mut Vec<(u64, u64, u64)>,
    cat: u64,
    src: u64,
    dst: u64,
    m: u64,
) -> (
    /*morphism*/ u64,
    /*equality*/ u64,
    /* components */ Vec<u64>,
) {
    use Feature::*;
    if let Some(e) = rm.is_identity(cat, m) {
        let (_, _, id, eq) = raise_identity(rm, functs, cat, e);
        (id, eq, Vec::new())
    } else if let Some((_, mid, _, m1, m2)) = rm.is_comp(cat, m) {
        let (fcat, fsrc, fmid, fdst, fm1, fm2, fleft, fright, feq) =
            raise_composition(rm, functs, cat, src, mid, dst, m1, m2);
        let (nm1, eq1, mut comps) = norm_under_functors(rm, functs, cat, src, mid, m1);
        let (nm2, eq2, comps2) = norm_under_functors(rm, functs, cat, mid, dst, m2);
        comps.append(&mut comps2);
        let cmpeq = rm
            .remote()
            .build(ComposeEq {
                cat: fcat,
                src: fsrc,
                mid: fmid,
                dst: fdst,
                left1: fm1,
                right1: nm1,
                eq1,
                left2: fm2,
                right2: nm2,
                eq2,
            })
            .unwrap();
        let nm12 = rm
            .remote()
            .build(ComposeMph {
                cat: fcat,
                src: fsrc,
                mid: fmid,
                dst: fdst,
                m1: nm1,
                m2: nm2,
            })
            .unwrap();
        let eq = rm
            .remote()
            .build(Concat {
                cat: fcat,
                src: fsrc,
                dst: fdst,
                left: fleft,
                mid: fright,
                right: nm12,
                eq1: feq,
                eq2: cmpeq,
            })
            .unwrap();
        let (r, req) = post_compose(rm, fcat, fsrc, fmid, fdst, nm1, nm2);
        let eq = rm
            .remote()
            .build(Concat {
                cat: fcat,
                src: fsrc,
                dst: fdst,
                left: fleft,
                mid: nm12,
                right: r,
                eq1: eq,
                eq2: req,
            })
            .unwrap();
        (r, eq, comps)
    } else if let Some((scat, funct, src, dst, mph)) = rm.is_funct_mph(cat, m) {
        functs.push((scat, cat, funct));
        let norm = norm_under_functors(rm, functs, scat, src, dst, mph);
        functs.pop();
        norm
    } else {
        let (fcat, fsrc, fdst, fmph) = apply_functors(rm, functs, cat, src, dst, m);
        let rfl = rm
            .remote()
            .build(Reflexivity {
                cat: fcat,
                src: fsrc,
                dst: fdst,
                mph: fmph,
            })
            .unwrap();
        (fmph, rfl, vec![fmph])
    }
}
