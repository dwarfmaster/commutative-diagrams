use itertools::Itertools;
use std::cmp::Ordering;

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct Morphism {
    pub src: u64,
    pub dst: u64,
    // (src, dst, mph)
    pub comps: Vec<(u64, u64, u64)>,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct Eq {
    pub slices: Vec<Slice>,
    pub cat: u64,
    pub inp: Morphism,
    pub outp: Morphism,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct Slice {
    pub inp: Morphism,
    pub outp: Morphism,
    // Assumes blocks are sorted by position. The first usize in the position in
    // the input and the second the position in the outputs. Relative positions
    // of blocks are the same in inputs and outputs.
    pub blocks: Vec<(usize, usize, Block)>,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct Block {
    pub inp: Morphism,
    pub outp: Morphism,
    pub data: BlockData,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum BlockData {
    Direct(u64),
    Inv(u64),
    Funct(u64, Eq),
    Split,
}

// TODO handle functors

impl Morphism {
    pub fn atom(src: u64, dst: u64, mph: u64) -> Self {
        Morphism {
            src,
            dst,
            comps: vec![(src, dst, mph)],
        }
    }

    pub fn id(obj: u64) -> Self {
        Morphism {
            src: obj,
            dst: obj,
            comps: Vec::new(),
        }
    }

    pub fn compose(&mut self, other: &Morphism) {
        // self.dst and other.src must be the same object in the proof assistant
        self.dst = other.dst;
        self.comps.extend(other.comps.iter());
    }
}

impl Eq {
    pub fn refl(cat: u64, p: Morphism) -> Eq {
        Eq {
            cat,
            inp: p.clone(),
            outp: p,
            slices: Vec::new(),
        }
    }

    // Check if the equality is a single block, with either a Direct(v) (Ok(v))
    // or Inv(v) (Err(v))
    pub fn is_simple(&self) -> Option<Result<u64, u64>> {
        use BlockData::*;
        if self.slices.len() == 1
            && self.inp.comps.len() == self.slices[0].inp.comps.len()
            && self.outp.comps.len() == self.slices[0].outp.comps.len()
            && self.slices[0].blocks.len() == 1
            && self.slices[0].blocks[0].0 == 0
            && self.slices[0].blocks[0].1 == 0
            && self.inp.comps.len() == self.slices[0].blocks[0].2.inp.comps.len()
            && self.outp.comps.len() == self.slices[0].blocks[0].2.outp.comps.len()
        {
            match self.slices[0].blocks[0].2.data {
                Direct(eq) => Some(Ok(eq)),
                Inv(eq) => Some(Err(eq)),
                _ => None,
            }
        } else {
            None
        }
    }

    // start is relative to the outputs of the block. Does not update outp or inp
    fn append_block(&mut self, start: usize, blk: Block) {
        // Find insertion point
        let mut insert_at = self.slices.len();
        let mut nstart = start;
        while insert_at > 0 {
            if self.slices[insert_at - 1].block_compatible(nstart, &blk) {
                nstart = self.slices[insert_at - 1].output_source(nstart).unwrap();
                insert_at -= 1;
            } else {
                break;
            }
        }

        // If new slice is needed
        if insert_at == self.slices.len() {
            let mut slice = Slice::refl(
                self.slices
                    .last()
                    .map(|sl| sl.outp.clone())
                    .unwrap_or(self.inp.clone()),
            );
            slice.insert_block_at(start, blk);
            self.slices.push(slice);
            return;
        }

        // Otherwise commutes until insertion point
        let range = ((insert_at + 1)..self.slices.len()).rev();
        let mut start = start;
        for i in range {
            start = self.slices[i].commutes_with_block(start, &blk).unwrap();
        }

        // Finally insert block
        self.slices[insert_at].insert_block_at(start, blk);
    }

    // Append a slice by appending blocks one by one. Does not update outp nor inp
    fn append_slice(&mut self, offset: usize, slice: Slice) {
        for (start, _, blk) in slice.blocks {
            self.append_block(start + offset, blk);
        }
    }

    // An atomic equality
    pub fn atomic(cat: u64, left: Morphism, right: Morphism, eq: u64) -> Self {
        let blk = Block {
            inp: left.clone(),
            outp: right.clone(),
            data: BlockData::Direct(eq),
        };
        let slice = Slice {
            inp: left.clone(),
            outp: right.clone(),
            blocks: vec![(0, 0, blk)],
        };
        Eq {
            cat,
            inp: left,
            outp: right,
            slices: vec![slice],
        }
    }

    // Append an equality to the current equality. The only condition is that
    // the inp of the appended equality is a subpath of the outp of self
    // starting at offset.
    pub fn append_at(&mut self, offset: usize, eq: Eq) {
        assert!(self.outp.comps.len() >= offset + eq.inp.comps.len());
        assert_eq!(
            &eq.inp.comps[..],
            &self.outp.comps[offset..(offset + eq.inp.comps.len())]
        );

        for slice in eq.slices {
            self.append_slice(offset, slice);
        }
        if let Some(lst) = self.slices.last() {
            self.outp = lst.outp.clone();
        }
    }

    pub fn append(&mut self, eq: Eq) {
        self.append_at(0, eq);
    }

    pub fn inv(&mut self) {
        std::mem::swap(&mut self.inp, &mut self.outp);
        self.slices.reverse();
        self.slices.iter_mut().for_each(|slice| slice.inv());
    }

    // Inefficient
    pub fn lap(&mut self, p: &Morphism) {
        self.inp.src = p.src;
        self.inp.comps = p
            .comps
            .iter()
            .copied()
            .chain(self.inp.comps.iter().copied())
            .collect();
        self.outp.src = p.src;
        self.outp.comps = p
            .comps
            .iter()
            .copied()
            .chain(self.outp.comps.iter().copied())
            .collect();
        self.slices.iter_mut().for_each(|slice| slice.lap(p));
    }

    pub fn rap(&mut self, p: &Morphism) {
        self.inp.dst = p.dst;
        self.inp.comps.extend(p.comps.iter().copied());
        self.outp.dst = p.dst;
        self.outp.comps.extend(p.comps.iter().copied());
        self.slices.iter_mut().for_each(|slice| slice.rap(p));
    }

    pub fn compose(&mut self, eq: Eq) {
        use itertools::EitherOrBoth::*;

        let mut slices = Vec::new();
        std::mem::swap(&mut slices, &mut self.slices);
        self.slices = slices
            .into_iter()
            .zip_longest(eq.slices.into_iter())
            .map(|zp| match zp {
                Both(mut left, right) => {
                    left.compose(right);
                    left
                }
                Left(mut left) => {
                    left.rap(&eq.outp);
                    left
                }
                Right(mut right) => {
                    right.lap(&self.outp);
                    right
                }
            })
            .collect();

        self.inp.comps.extend(eq.inp.comps.into_iter());
        self.outp.comps.extend(eq.outp.comps.into_iter());
    }
}

// TODO simplify concatenation of inverse equalities
impl Slice {
    fn refl(p: Morphism) -> Slice {
        Slice {
            inp: p.clone(),
            outp: p,
            blocks: Vec::new(),
        }
    }

    // Try to insert a block. start is the index to insert it at on the outputs.
    // Return None on success, and give back the block on failure
    fn insert_block_at(&mut self, start: usize, blk: Block) -> Option<Block> {
        if !self.block_compatible(start, &blk) {
            return Some(blk);
        }

        // inp doesn't change, since we insert on a passthrough place.
        // However output changes
        let start_input = self.output_source(start).unwrap();
        let output_range = start..(start + blk.inp.comps.len());
        self.outp
            .comps
            .splice(output_range, blk.outp.comps.iter().copied());

        let r = self
            .blocks
            .binary_search_by_key(&start, |path| path.0)
            .unwrap_err();
        self.blocks.insert(r, (start_input, start, blk));
        None
    }

    // Commutes slice with block (assuming it is compatible). start is relative
    // to the output. On success, return the index of the block on the input
    // range.
    fn commutes_with_block(&mut self, start: usize, blk: &Block) -> Option<usize> {
        if !self.block_compatible(start, blk) {
            return None;
        }

        // Update input
        let start_input = self.output_source(start).unwrap();
        let input_range = start_input..(start_input + blk.inp.comps.len());
        self.inp
            .comps
            .splice(input_range, blk.outp.comps.iter().copied());

        // Update output
        let output_range = start..(start + blk.inp.comps.len());
        self.outp
            .comps
            .splice(output_range, blk.outp.comps.iter().copied());

        // Update blocks
        let offset = (blk.outp.comps.len() as isize) - (blk.inp.comps.len() as isize);
        self.blocks
            .iter_mut()
            .filter(|(_, st, _)| *st > start)
            .for_each(|b| {
                b.0 = b.0.saturating_add_signed(offset);
                b.1 = b.1.saturating_add_signed(offset);
            });

        Some(start_input)
    }

    // Test if the block is compatible with the slice (assumes well typedness).
    // start is assumed to be relative to output.
    fn block_compatible(&self, start: usize, blk: &Block) -> bool {
        (0..blk.inp.comps.len())
            .map(|x| x + start)
            .all(|output| self.output_source(output).is_ok())
    }

    // Given an input index, indicates which output it connects to or if it goes
    // into a block (giving the block index and the index of the input of the
    // block).
    pub fn input_target(&self, input: usize) -> Result<usize, (usize, usize)> {
        let r = self.blocks.binary_search_by(|(src, _, blk)| {
            if *src > input {
                Ordering::Greater
            } else if src + blk.inp.comps.len() <= input {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });
        match r {
            Ok(blkid) => Err((blkid, input - self.blocks[blkid].0)),
            Err(0) => Ok(input),
            Err(blkid) => {
                let blkid = blkid - 1;
                let offset = input - self.blocks[blkid].0 - self.blocks[blkid].2.inp.comps.len();
                Ok(self.blocks[blkid].1 + self.blocks[blkid].2.outp.comps.len() + offset)
            }
        }
    }

    // Same in the other direction: given an output give the connected input or
    // block output
    pub fn output_source(&self, output: usize) -> Result<usize, (usize, usize)> {
        let r = self.blocks.binary_search_by(|(_, src, blk)| {
            if *src > output {
                Ordering::Greater
            } else if src + blk.outp.comps.len() <= output {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });
        match r {
            Ok(blkid) => Err((blkid, output - self.blocks[blkid].1)),
            Err(0) => Ok(output),
            Err(blkid) => {
                let blkid = blkid - 1;
                let offset = output - self.blocks[blkid].1 - self.blocks[blkid].2.outp.comps.len();
                Ok(self.blocks[blkid].0 + self.blocks[blkid].2.inp.comps.len() + offset)
            }
        }
    }

    fn inv(&mut self) {
        std::mem::swap(&mut self.inp, &mut self.outp);
        self.blocks.iter_mut().for_each(|(_, _, blk)| blk.inv());
    }

    // Inefficient, avoid as much as possible
    fn lap(&mut self, p: &Morphism) {
        self.blocks.iter_mut().for_each(|blk| {
            blk.0 += p.comps.len();
            blk.1 += p.comps.len();
        });
        self.inp.src = p.src;
        self.inp.comps = p
            .comps
            .iter()
            .copied()
            .chain(self.inp.comps.iter().copied())
            .collect();
        self.outp.src = p.src;
        self.outp.comps = p
            .comps
            .iter()
            .copied()
            .chain(self.outp.comps.iter().copied())
            .collect();
    }

    fn rap(&mut self, p: &Morphism) {
        self.inp.comps.extend(p.comps.iter().copied());
        self.outp.comps.extend(p.comps.iter().copied());
    }

    fn compose(&mut self, o: Slice) {
        let offset_in = self.inp.comps.len();
        let offset_out = self.outp.comps.len();
        self.inp.comps.extend(o.inp.comps.into_iter());
        self.outp.comps.extend(o.outp.comps.into_iter());
        self.blocks.extend(
            o.blocks
                .into_iter()
                .map(|(off_in, off_out, blk)| (off_in + offset_in, off_out + offset_out, blk)),
        );
    }
}

impl Block {
    fn inv(&mut self) {
        use BlockData::*;
        std::mem::swap(&mut self.inp, &mut self.outp);
        self.data = match &self.data {
            Direct(eq) => Inv(eq.clone()),
            Inv(eq) => Direct(eq.clone()),
            Funct(f, eq) => {
                let mut eq = eq.clone();
                eq.inv();
                Funct(f.clone(), eq)
            }
            Split => Split,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::graph::eq::definition::{Block, BlockData, Eq, Morphism, Slice};

    fn loops(n: usize, mph: u64) -> Morphism {
        Morphism {
            src: 0,
            dst: 0,
            comps: vec![(0, 0, mph); n],
        }
    }

    fn test_slice() -> Slice {
        let b31 = Block {
            inp: loops(3, 1),
            outp: loops(1, 1),
            data: BlockData::Direct(42),
        };
        let b14 = Block {
            inp: loops(1, 1),
            outp: loops(4, 1),
            data: BlockData::Direct(43),
        };
        let b25 = Block {
            inp: loops(2, 1),
            outp: loops(5, 1),
            data: BlockData::Direct(44),
        };
        Slice {
            inp: loops(10, 1),
            outp: loops(14, 1),
            blocks: vec![(1, 1, b31), (5, 3, b25), (9, 10, b14)],
        }
    }

    #[test]
    fn slice_input_output() {
        let slice = test_slice();

        assert_eq!(slice.input_target(0), Ok(0));
        assert_eq!(slice.input_target(2), Err((0, 1)));
        assert_eq!(slice.input_target(4), Ok(2));
        assert_eq!(slice.input_target(8), Ok(9));
        assert_eq!(slice.input_target(9), Err((2, 0)));

        assert_eq!(slice.output_source(0), Ok(0));
        assert_eq!(slice.output_source(1), Err((0, 0)));
        assert_eq!(slice.output_source(2), Ok(4));
        assert_eq!(slice.output_source(4), Err((1, 1)));
        assert_eq!(slice.output_source(6), Err((1, 3)));
        assert_eq!(slice.output_source(8), Ok(7));
        assert_eq!(slice.output_source(12), Err((2, 2)));
    }

    #[test]
    fn slice_insert_block() {
        let b25 = Block {
            inp: loops(2, 1),
            outp: loops(5, 17),
            data: BlockData::Direct(45),
        };
        let mut slice = test_slice();

        let ins1 = slice.insert_block_at(1, b25.clone());
        // Check nothing's changed
        assert!(ins1.is_some());
        assert_eq!(slice.inp.comps.len(), 10);
        assert_eq!(slice.outp.comps.len(), 14);

        let ins2 = slice.insert_block_at(8, b25);
        assert!(ins2.is_none());
        assert_eq!(slice.inp.comps.len(), 10);
        assert_eq!(slice.outp.comps.len(), 17);
        assert_eq!(slice.outp.comps[8], (0, 0, 17));
        assert_eq!(slice.outp.comps[12], (0, 0, 17));
    }

    #[test]
    fn slice_commutes_block() {
        let b25 = Block {
            inp: loops(2, 1),
            outp: loops(5, 17),
            data: BlockData::Direct(47),
        };
        let mut slice = test_slice();

        let ins1 = slice.commutes_with_block(1, &b25);
        // Check nothing's changed
        assert!(ins1.is_none());
        assert_eq!(slice.inp.comps.len(), 10);
        assert_eq!(slice.outp.comps.len(), 14);

        let ins2 = slice.commutes_with_block(8, &b25);
        assert_eq!(ins2, Some(7));
        assert_eq!(slice.inp.comps.len(), 13);
        assert_eq!(slice.inp.comps[7], (0, 0, 17));
        assert_eq!(slice.inp.comps[11], (0, 0, 17));
        assert_eq!(slice.outp.comps.len(), 17);
        assert_eq!(slice.outp.comps[8], (0, 0, 17));
        assert_eq!(slice.outp.comps[12], (0, 0, 17));
        assert_eq!(slice.blocks[2].0, 12);
        assert_eq!(slice.blocks[2].1, 13);
    }

    #[test]
    fn eq_append_block() {
        let mut eq = Eq::refl(0, loops(10, 1));
        let b25 = Block {
            inp: loops(2, 1),
            outp: loops(5, 17),
            data: BlockData::Direct(48),
        };

        eq.append_block(1, b25.clone());
        assert_eq!(eq.slices.len(), 1);
        assert_eq!(eq.inp.comps.len(), 10);

        eq.append_block(10, b25.clone());
        assert_eq!(eq.slices.len(), 1);

        eq.append_block(5, b25.clone());
        assert_eq!(eq.slices.len(), 2);
        assert_eq!(eq.slices[0].outp, eq.slices[1].inp);
    }

    #[test]
    fn eq_append_slice() {
        let mut eq = Eq::refl(0, loops(7, 1));
        let b25 = Block {
            inp: loops(2, 1),
            outp: loops(5, 17),
            data: BlockData::Direct(49),
        };
        eq.append_block(2, b25);
        eq.append_slice(0, test_slice());

        assert_eq!(eq.slices.len(), 2);
        assert_eq!(eq.slices[0].blocks.len(), 2);
        assert_eq!(eq.slices[1].blocks.len(), 2);
    }

    #[test]
    fn eq_append() {
        let mut eq1 = Eq::refl(0, loops(7, 1));
        let b25 = Block {
            inp: loops(2, 1),
            outp: loops(5, 1),
            data: BlockData::Direct(50),
        };
        eq1.append_block(2, b25.clone());
        eq1.outp = eq1.slices[0].outp.clone();

        let mut eq2 = Eq::refl(0, loops(7, 1));
        eq2.append_block(5, b25.clone());

        eq1.append_at(2, eq2);
        assert_eq!(eq1.outp.comps.len(), 13);
        assert_eq!(eq1.slices.len(), 1);
    }
}
