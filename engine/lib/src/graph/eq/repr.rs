use super::{Block, BlockData, Eq, Morphism, Slice};
use crate::remote::TermEngine;

impl Morphism {
    pub fn get_repr<R: TermEngine>(&self, ctx: &mut R) -> Morphism {
        Morphism {
            src: ctx.get_repr(self.src),
            dst: ctx.get_repr(self.dst),
            comps: self
                .comps
                .iter()
                .map(|(s, d, m)| (ctx.get_repr(*s), ctx.get_repr(*d), ctx.get_repr(*m)))
                .collect(),
        }
    }
}

impl Eq {
    pub fn get_repr<R: TermEngine>(&self, ctx: &mut R) -> Eq {
        Eq {
            cat: self.cat,
            inp: self.inp.get_repr(ctx),
            outp: self.outp.get_repr(ctx),
            slices: self
                .slices
                .iter()
                .map(|slice| slice.get_repr(ctx))
                .collect(),
        }
    }
}

impl Slice {
    pub fn get_repr<R: TermEngine>(&self, ctx: &mut R) -> Slice {
        Slice {
            inp: self.inp.get_repr(ctx),
            outp: self.outp.get_repr(ctx),
            blocks: self
                .blocks
                .iter()
                .map(|(ins, outs, block)| (*ins, *outs, block.get_repr(ctx)))
                .collect(),
        }
    }
}

impl Block {
    pub fn get_repr<R: TermEngine>(&self, ctx: &mut R) -> Block {
        use BlockData::*;
        Block {
            inp: self.inp.get_repr(ctx),
            outp: self.outp.get_repr(ctx),
            data: match &self.data {
                Direct(eq) => Direct(*eq),
                Inv(eq) => Inv(*eq),
                Funct(f, eq) => Funct(ctx.get_repr(*f), eq.get_repr(ctx)),
                Split => Split,
            },
        }
    }
}
