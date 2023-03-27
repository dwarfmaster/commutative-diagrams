use crate::data::{EqualityData, Functor};
use std::cmp::Ordering;

type Path = (usize, Vec<(usize, usize)>);

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct Eq {
    pub slices: Vec<Slice>,
    pub inp: Path,
    pub outp: Path,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct Slice {
    pub inp: Path,
    pub outp: Path,
    // Assumes blocks are sorted by position. The first usize in the position in
    // the input and the second the position in the outputs. Relative positions
    // of blocks are the same in inputs and outputs.
    pub blocks: Vec<(usize, usize, Block)>,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct Block {
    pub inp: Path,
    pub outp: Path,
    pub data: BlockData,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum BlockData {
    Direct(EqualityData),
    Inv(EqualityData),
    Funct(Functor, Eq),
}

// TODO handle functors

impl Eq {
    pub fn refl(p: Path) -> Eq {
        Eq {
            inp: p.clone(),
            outp: p,
            slices: Vec::new(),
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
}

// TODO simplify concatenation of inverse equalities
impl Slice {
    fn refl(p: Path) -> Slice {
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
        let output_range = start..(start + blk.inp.1.len());
        self.outp.1.splice(output_range, blk.outp.1.iter().copied());

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
        let input_range = start_input..(start_input + blk.inp.1.len());
        self.inp.1.splice(input_range, blk.outp.1.iter().copied());

        // Update output
        let output_range = start..(start + blk.inp.1.len());
        self.outp.1.splice(output_range, blk.outp.1.iter().copied());

        // Update blocks
        let offset = (blk.outp.1.len() as isize) - (blk.inp.1.len() as isize);
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
        (0..blk.inp.1.len())
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
            } else if src + blk.inp.1.len() <= input {
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
                let offset = input - self.blocks[blkid].0 - self.blocks[blkid].2.inp.1.len();
                Ok(self.blocks[blkid].1 + self.blocks[blkid].2.outp.1.len() + offset)
            }
        }
    }

    // Same in the other direction: given an output give the connected input or
    // block output
    pub fn output_source(&self, output: usize) -> Result<usize, (usize, usize)> {
        let r = self.blocks.binary_search_by(|(_, src, blk)| {
            if *src > output {
                Ordering::Greater
            } else if src + blk.outp.1.len() <= output {
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
                let offset = output - self.blocks[blkid].1 - self.blocks[blkid].2.outp.1.len();
                Ok(self.blocks[blkid].0 + self.blocks[blkid].2.inp.1.len() + offset)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::data::{ActualProofObject, Context, EqualityData};
    use crate::dsl::{cat, mph, obj};
    use crate::graph::eq::{Block, BlockData, Eq, Slice};

    fn dummy_data() -> BlockData {
        let ctx = Context::new();
        let cat = cat!(ctx, :0);
        let x = obj!(ctx, (:1) in cat);
        let m = mph!(ctx, (:2) : x -> x);
        let eqdata = EqualityData {
            category: cat,
            src: x.clone(),
            dst: x,
            left: m.clone(),
            right: m,
            pobj: ctx.mk(ActualProofObject::Term(3)),
        };
        BlockData::Direct(eqdata)
    }

    fn test_slice() -> Slice {
        let b31 = Block {
            inp: (0, vec![(0, 0); 3]),
            outp: (0, vec![(0, 0); 1]),
            data: dummy_data(),
        };
        let b14 = Block {
            inp: (0, vec![(0, 0); 1]),
            outp: (0, vec![(0, 0); 4]),
            data: dummy_data(),
        };
        let b25 = Block {
            inp: (0, vec![(0, 0); 2]),
            outp: (0, vec![(0, 0); 5]),
            data: dummy_data(),
        };
        Slice {
            inp: (0, vec![(0, 0); 10]),
            outp: (0, vec![(0, 0); 14]),
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
            inp: (0, vec![(0, 0); 2]),
            outp: (0, vec![(1, 1); 5]),
            data: dummy_data(),
        };
        let mut slice = test_slice();

        let ins1 = slice.insert_block_at(1, b25.clone());
        // Check nothing's changed
        assert!(ins1.is_some());
        assert_eq!(slice.inp.1.len(), 10);
        assert_eq!(slice.outp.1.len(), 14);

        let ins2 = slice.insert_block_at(8, b25);
        assert!(ins2.is_none());
        assert_eq!(slice.inp.1.len(), 10);
        assert_eq!(slice.outp.1.len(), 17);
        assert_eq!(slice.outp.1[8], (1, 1));
        assert_eq!(slice.outp.1[12], (1, 1));
    }

    #[test]
    fn slice_commutes_block() {
        let b25 = Block {
            inp: (0, vec![(0, 0); 2]),
            outp: (0, vec![(1, 1); 5]),
            data: dummy_data(),
        };
        let mut slice = test_slice();

        let ins1 = slice.commutes_with_block(1, &b25);
        // Check nothing's changed
        assert!(ins1.is_none());
        assert_eq!(slice.inp.1.len(), 10);
        assert_eq!(slice.outp.1.len(), 14);

        let ins2 = slice.commutes_with_block(8, &b25);
        assert_eq!(ins2, Some(7));
        assert_eq!(slice.inp.1.len(), 13);
        assert_eq!(slice.inp.1[7], (1, 1));
        assert_eq!(slice.inp.1[11], (1, 1));
        assert_eq!(slice.outp.1.len(), 17);
        assert_eq!(slice.outp.1[8], (1, 1));
        assert_eq!(slice.outp.1[12], (1, 1));
        assert_eq!(slice.blocks[2].0, 12);
        assert_eq!(slice.blocks[2].1, 13);
    }

    #[test]
    fn eq_append_block() {
        let mut eq = Eq::refl((0, vec![(0, 0); 10]));
        let b25 = Block {
            inp: (0, vec![(0, 0); 2]),
            outp: (0, vec![(1, 1); 5]),
            data: dummy_data(),
        };

        eq.append_block(1, b25.clone());
        assert_eq!(eq.slices.len(), 1);
        assert_eq!(eq.inp.1.len(), 10);

        eq.append_block(10, b25.clone());
        assert_eq!(eq.slices.len(), 1);

        eq.append_block(5, b25.clone());
        assert_eq!(eq.slices.len(), 2);
        assert_eq!(eq.slices[0].outp, eq.slices[1].inp);
    }

    #[test]
    fn eq_append_slice() {
        let mut eq = Eq::refl((0, vec![(0, 0); 7]));
        let b25 = Block {
            inp: (0, vec![(0, 0); 2]),
            outp: (0, vec![(1, 1); 5]),
            data: dummy_data(),
        };
        eq.append_block(2, b25);
        eq.append_slice(0, test_slice());

        assert_eq!(eq.slices.len(), 2);
        assert_eq!(eq.slices[0].blocks.len(), 2);
        assert_eq!(eq.slices[1].blocks.len(), 2);
    }
}
