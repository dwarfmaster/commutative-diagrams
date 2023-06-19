use crate::data::Feature;
use crate::graph::eq::{Block, BlockData, Eq, Morphism, Slice};
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
fn normalize_slice<F>(
    start: F, // Gives the range covered by a block
    blks: &[(usize, usize, Block)],
    mph: &Morphism,
) -> Vec<(usize, usize, Option<Block>)>
where
    F: Fn(&(usize, usize, Block)) -> (usize, usize),
{
    let mut current = 0;
    let mut blk = 0;
    let mut r = Vec::new();
    while current < mph.comps.len() {
        if blk < blks.len() && current == start(&blks[blk]).0 {
            let len = start(&blks[blk]).1;
            r.push((current, len, Some(blks[blk].2.clone())));
            current += len;
            blk += 1;
        } else {
            let next = if blk < blks.len() {
                start(&blks[blk]).0
            } else {
                mph.comps.len()
            };
            r.push((current, next - current, None));
            current = next;
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
    let nm = normalize_slice(
        |(id, _, blk)| (*id, blk.inp.comps.len()),
        &slice.blocks[..],
        &slice.inp,
    );
    mk_slice(rm, cat, &slice.inp, nm, |rm, blk| mk_inp_blk(rm, cat, blk))
}

fn mk_outp_slice<R: TermEngine>(rm: &mut R, cat: u64, slice: &Slice) -> u64 {
    let nm = normalize_slice(
        |(_, id, blk)| (*id, blk.outp.comps.len()),
        &slice.blocks[..],
        &slice.outp,
    );
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
