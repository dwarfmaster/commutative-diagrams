use crate::data::Feature;
use crate::graph::eq::{Block, BlockData, Eq, Morphism, Slice};
use crate::normalizer::morphism;
use crate::remote::{Remote, TermEngine};

fn morphism_sub(mph: &Morphism, start: usize, size: usize) -> Morphism {
    if size == 0 && start >= mph.comps.len() {
        Morphism {
            src: mph.dst,
            dst: mph.dst,
            comps: Vec::new(),
        }
    } else if size == 0 {
        Morphism {
            src: mph.comps[start].0,
            dst: mph.comps[start].0,
            comps: Vec::new(),
        }
    } else {
        let comps: Vec<(u64, u64, u64)> =
            mph.comps[start..(start + size)].iter().copied().collect();
        Morphism {
            src: comps.first().unwrap().0,
            dst: comps.last().unwrap().1,
            comps,
        }
    }
}

// From a list of non-overlapping blocks covering part of a morphism, get a
// complete covering with ranges given by their index and size (and ordered),
// and wether the range is handled by a block.
fn normalize_slice(
    blks: &[(usize, usize, Block)],
    mph_in: &Morphism,
    mph_out: &Morphism,
) -> Vec<(
    /*start_in*/ usize,
    /*len_in*/ usize,
    /*start_out*/ usize,
    /*len_out*/ usize,
    Option<Block>,
)> {
    let mut current_in = 0;
    let mut current_out = 0;
    let mut blk = 0;
    let mut r = Vec::new();
    while current_in < mph_in.comps.len() || current_out < mph_out.comps.len() {
        if blk < blks.len() && current_in == blks[blk].0 {
            assert_eq!(current_out, blks[blk].1);
            let len_in = blks[blk].2.inp.comps.len();
            let len_out = blks[blk].2.outp.comps.len();
            r.push((
                current_in,
                len_in,
                current_out,
                len_out,
                Some(blks[blk].2.clone()),
            ));
            current_in += len_in;
            current_out += len_out;
            blk += 1;
        } else {
            let next = if blk < blks.len() {
                blks[blk].0
            } else {
                mph_in.comps.len()
            };
            r.push((
                current_in,
                next - current_in,
                current_out,
                next - current_in,
                None,
            ));
            current_out += next - current_in;
            current_in = next;
        }
    }
    r
}

//  ___ _   _ ____   _____  _   _ _____ ____
// |_ _| \ | |  _ \ / / _ \| | | |_   _|  _ \
//  | ||  \| | |_) / / | | | | | | | | | |_) |
//  | || |\  |  __/ /| |_| | |_| | | | |  __/
// |___|_| \_|_| /_/  \___/ \___/  |_| |_|
//
fn mk_inp_eq<R: TermEngine>(rm: &mut R, eq: &Eq) -> u64 {
    if let Some(slice) = eq.slices.first() {
        mk_inp_slice(rm, eq.cat, slice)
    } else {
        mk_morphism(rm, eq.cat, &eq.inp)
    }
}

fn mk_outp_eq<R: TermEngine>(rm: &mut R, eq: &Eq) -> u64 {
    if let Some(slice) = eq.slices.last() {
        mk_outp_slice(rm, eq.cat, slice)
    } else {
        mk_morphism(rm, eq.cat, &eq.outp)
    }
}

fn mk_morphism<R: TermEngine>(rm: &mut R, cat: u64, mph: &Morphism) -> u64 {
    let r = mph
        .comps
        .iter()
        .copied()
        .reduce(|(s1, d1, m1), (s2, d2, m2)| {
            assert_eq!(d1, s2);
            let m = rm
                .remote()
                .build(Feature::ComposeMph {
                    cat,
                    src: s1,
                    mid: d1,
                    dst: d2,
                    m1,
                    m2,
                })
                .unwrap();
            (s1, d2, m)
        });
    if let Some((_, m, _)) = r {
        m
    } else {
        rm.remote()
            .build(Feature::Identity { cat, obj: mph.src })
            .unwrap()
    }
}

fn mk_slice<R: TermEngine, F>(
    rm: &mut R,
    cat: u64,
    mph: &Morphism,
    nm: Vec<(usize, usize, Option<Block>)>,
    blkmake: F,
) -> u64
where
    F: Fn(&mut R, &Block) -> u64,
{
    nm.into_iter()
        .map(|(start, len, blk)| {
            let mph = morphism_sub(mph, start, len);
            if let Some(blk) = blk {
                let m = blkmake(rm, &blk);
                (mph.src, mph.dst, m)
            } else {
                let m = mk_morphism(rm, cat, &mph);
                (mph.src, mph.dst, m)
            }
        })
        .collect::<Vec<_>>() // Necessary to prevent rust complaining about rm being captured twice
        .into_iter()
        .reduce(|(s1, d1, m1), (s2, d2, m2)| {
            assert_eq!(d1, s2);
            let m = rm
                .remote()
                .build(Feature::ComposeMph {
                    cat,
                    src: s1,
                    mid: d1,
                    dst: d2,
                    m1,
                    m2,
                })
                .unwrap();
            (s1, d2, m)
        })
        .unwrap_or_else(|| {
            let obj = mph.src;
            let m = rm.remote().build(Feature::Identity { cat, obj }).unwrap();
            (obj, obj, m)
        })
        .2
}

fn mk_inp_slice<R: TermEngine>(rm: &mut R, cat: u64, slice: &Slice) -> u64 {
    let nm = normalize_slice(&slice.blocks[..], &slice.inp, &slice.outp)
        .into_iter()
        .map(|(start, len, _, _, blk)| (start, len, blk))
        .collect();
    mk_slice(rm, cat, &slice.inp, nm, |rm, blk| mk_inp_blk(rm, cat, blk))
}

fn mk_outp_slice<R: TermEngine>(rm: &mut R, cat: u64, slice: &Slice) -> u64 {
    let nm = normalize_slice(&slice.blocks[..], &slice.inp, &slice.outp)
        .into_iter()
        .map(|(_, _, start, len, blk)| (start, len, blk))
        .collect();
    mk_slice(rm, cat, &slice.outp, nm, |rm, blk| {
        mk_outp_blk(rm, cat, blk)
    })
}

fn mk_inp_blk<R: TermEngine>(rm: &mut R, cat: u64, blk: &Block) -> u64 {
    use BlockData::*;
    match &blk.data {
        Direct(eq) => rm.is_eq(eq.clone(), cat).unwrap().2,
        Inv(eq) => rm.is_eq(eq.clone(), cat).unwrap().3,
        Funct(f, eq) => {
            let scat = rm.is_funct(*f, cat).unwrap();
            let m = mk_inp_eq(rm, &eq);
            let (src, dst) = rm.is_mph(m, scat).unwrap();
            rm.remote()
                .build(Feature::AppliedFunctMph {
                    scat,
                    dcat: cat,
                    funct: *f,
                    src,
                    dst,
                    mph: m,
                })
                .unwrap()
        }
        Split => mk_morphism(rm, cat, &blk.inp),
    }
}

fn mk_outp_blk<R: TermEngine>(rm: &mut R, cat: u64, blk: &Block) -> u64 {
    use BlockData::*;
    match &blk.data {
        Direct(eq) => rm.is_eq(eq.clone(), cat).unwrap().3,
        Inv(eq) => rm.is_eq(eq.clone(), cat).unwrap().2,
        Funct(f, eq) => {
            let scat = rm.is_funct(*f, cat).unwrap();
            let m = mk_outp_eq(rm, &eq);
            let (src, dst) = rm.is_mph(m, scat).unwrap();
            rm.remote()
                .build(Feature::AppliedFunctMph {
                    scat,
                    dcat: cat,
                    funct: *f,
                    src,
                    dst,
                    mph: m,
                })
                .unwrap()
        }
        Split => mk_morphism(rm, cat, &blk.outp),
    }
}

//  ____            _ _
// |  _ \ ___  __ _| (_)_______
// | |_) / _ \/ _` | | |_  / _ \
// |  _ <  __/ (_| | | |/ /  __/
// |_| \_\___|\__,_|_|_/___\___|
//

// Create an equality between two morphisms that have the same normal form
fn repar<R: TermEngine>(rm: &mut R, cat: u64, src: u64, dst: u64, left: u64, right: u64) -> u64 {
    if left == right {
        rm.remote()
            .build(Feature::Reflexivity {
                cat,
                src,
                dst,
                mph: left,
            })
            .unwrap()
    } else {
        let (m1, eq1, _) = morphism(rm, cat, src, dst, left);
        let (m2, eq2, _) = morphism(rm, cat, src, dst, right);
        assert_eq!(m1, m2); // TODO maybe a bit strong ? They only need to be convertible
        let eq2 = rm
            .remote()
            .build(Feature::InverseEq {
                cat,
                src,
                dst,
                left: right,
                right: m2,
                eq: eq2,
            })
            .unwrap();

        rm.remote()
            .build(Feature::Concat {
                cat,
                src,
                dst,
                left,
                mid: m2,
                right,
                eq1,
                eq2,
            })
            .unwrap()
    }
}

pub fn realize_eq<R: TermEngine>(rm: &mut R, left: u64, right: u64, eq: &Eq) -> u64 {
    let cat = eq.cat;
    let src = eq.inp.src;
    let dst = eq.inp.dst;

    let mut input = left;
    let mut acc = None;
    for slice in &eq.slices {
        let (output, eq) = realize_slice(rm, cat, src, dst, input, slice);
        if let Some(acc_eq) = acc {
            acc = Some(
                rm.remote()
                    .build(Feature::Concat {
                        cat,
                        src,
                        dst,
                        left,
                        mid: input,
                        right: output,
                        eq1: acc_eq,
                        eq2: eq,
                    })
                    .unwrap(),
            );
        } else {
            acc = Some(eq);
        }
        input = output;
    }

    if let Some(acc) = acc {
        if input == right {
            acc
        } else {
            let eq = repar(rm, cat, src, dst, input, right);
            rm.remote()
                .build(Feature::Concat {
                    cat,
                    src,
                    dst,
                    left,
                    mid: input,
                    right,
                    eq1: acc,
                    eq2: eq,
                })
                .unwrap()
        }
    } else {
        repar(rm, cat, src, dst, input, right)
    }
}

fn realize_slice<R: TermEngine>(
    rm: &mut R,
    cat: u64,
    src: u64,
    dst: u64,
    input: u64,
    slice: &Slice,
) -> (/*output*/ u64, /*equality*/ u64) {
    let nm = normalize_slice(&slice.blocks[..], &slice.inp, &slice.outp);
    let mut partial_state = None;
    for (start_in, len_in, _, _, blk) in nm {
        if let Some(blk) = blk {
            let mph = morphism_sub(&slice.inp, start_in, len_in);
            let (blk_in, blk_out, eq) = realize_block(rm, cat, &blk);
            if let Some((peq, pin, pout)) = partial_state {
                let eq = rm
                    .remote()
                    .build(Feature::ComposeEq {
                        cat,
                        src,
                        mid: mph.src,
                        dst: mph.dst,
                        left1: pin,
                        right1: pout,
                        eq1: peq,
                        left2: blk_in,
                        right2: blk_out,
                        eq2: eq,
                    })
                    .unwrap();
                let min = rm
                    .remote()
                    .build(Feature::ComposeMph {
                        cat,
                        src,
                        mid: mph.src,
                        dst: mph.dst,
                        m1: pin,
                        m2: blk_in,
                    })
                    .unwrap();
                let mout = rm
                    .remote()
                    .build(Feature::ComposeMph {
                        cat,
                        src,
                        mid: mph.src,
                        dst: mph.dst,
                        m1: pout,
                        m2: blk_out,
                    })
                    .unwrap();
                partial_state = Some((eq, min, mout));
            } else {
                partial_state = Some((eq, blk_in, blk_out));
            }
        } else {
            let mph = morphism_sub(&slice.inp, start_in, len_in);
            let m = mk_morphism(rm, cat, &mph);
            if let Some((peq, pin, pout)) = partial_state {
                let eq = rm
                    .remote()
                    .build(Feature::RightApplication {
                        cat,
                        src,
                        mid: mph.src,
                        dst: mph.dst,
                        left: pin,
                        right: pout,
                        eq: peq,
                        mph: m,
                    })
                    .unwrap();
                let min = rm
                    .remote()
                    .build(Feature::ComposeMph {
                        cat,
                        src,
                        mid: mph.src,
                        dst: mph.dst,
                        m1: pin,
                        m2: m,
                    })
                    .unwrap();
                let mout = rm
                    .remote()
                    .build(Feature::ComposeMph {
                        cat,
                        src,
                        mid: mph.src,
                        dst: mph.dst,
                        m1: pout,
                        m2: m,
                    })
                    .unwrap();
                partial_state = Some((eq, min, mout));
            } else {
                partial_state = Some((
                    rm.remote()
                        .build(Feature::Reflexivity {
                            cat,
                            src: mph.src,
                            dst: mph.dst,
                            mph: m,
                        })
                        .unwrap(),
                    m,
                    m,
                ));
            }
        }
    }

    let (expecting, output, eq) =
        partial_state.unwrap_or_else(|| panic!("There should be no empty slices in equalities"));
    let rep = repar(rm, cat, src, dst, input, expecting);
    let eq = rm
        .remote()
        .build(Feature::Concat {
            cat,
            src,
            dst,
            left: input,
            mid: expecting,
            right: output,
            eq1: rep,
            eq2: eq,
        })
        .unwrap();

    (output, eq)
}

fn realize_block<R: TermEngine>(
    rm: &mut R,
    cat: u64,
    blk: &Block,
) -> (/*left*/ u64, /*right*/ u64, /*eq*/ u64) {
    use BlockData::*;
    match &blk.data {
        Direct(eq) => {
            let (_, _, left, right) = rm.is_eq(*eq, cat).unwrap();
            (left, right, *eq)
        }
        Inv(eq) => {
            let (_, _, left, right) = rm.is_eq(*eq, cat).unwrap();
            let eq = rm
                .remote()
                .build(Feature::InverseEq {
                    cat,
                    src: blk.inp.src,
                    dst: blk.inp.dst,
                    left,
                    right,
                    eq: *eq,
                })
                .unwrap();
            (right, left, eq)
        }
        Funct(f, eq) => {
            let scat = rm.is_funct(*f, cat).unwrap();
            let m_in = mk_inp_eq(rm, &eq);
            let m_out = mk_outp_eq(rm, &eq);
            let m_eq = realize_eq(rm, m_in, m_out, &eq);
            let f_in = rm
                .remote()
                .build(Feature::AppliedFunctMph {
                    scat,
                    dcat: cat,
                    funct: *f,
                    src: eq.inp.src,
                    dst: eq.inp.dst,
                    mph: m_in,
                })
                .unwrap();
            let f_out = rm
                .remote()
                .build(Feature::AppliedFunctMph {
                    scat,
                    dcat: cat,
                    funct: *f,
                    src: eq.inp.src,
                    dst: eq.inp.dst,
                    mph: m_out,
                })
                .unwrap();
            let f_eq = rm
                .remote()
                .build(Feature::AppliedFunctEq {
                    scat,
                    dcat: cat,
                    funct: *f,
                    src: eq.inp.src,
                    dst: eq.inp.dst,
                    left: m_in,
                    right: m_out,
                    eq: m_eq,
                })
                .unwrap();
            (f_in, f_out, f_eq)
        }
        Split => {
            let m = mk_morphism(rm, cat, &blk.inp);
            let eq = rm
                .remote()
                .build(Feature::Reflexivity {
                    cat,
                    src: blk.inp.src,
                    dst: blk.inp.dst,
                    mph: m,
                })
                .unwrap();
            (m, m, eq)
        }
    }
}
