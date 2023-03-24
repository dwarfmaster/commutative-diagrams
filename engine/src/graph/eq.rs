
use crate::data::{EqualityData,Functor};
use std::cmp::Ordering;

type Path = (usize, Vec<(usize, usize)>);

#[derive(Eq,PartialEq,Hash,Debug,Clone)]
pub struct Eq {
    pub slices: Vec<Slice>,
    pub inp: Path,
    pub outp: Path,
}

#[derive(Eq,PartialEq,Hash,Debug,Clone)]
pub struct Slice {
    pub inp: Path,
    pub outp: Path,
    // Assumes blocks are sorted by position. The first usize in the position in
    // the input and the second the position in the outputs. Relative positions
    // of blocks are the same in inputs and outputs.
    pub blocks: Vec<(usize, usize, Block)>,
}

#[derive(Eq,PartialEq,Hash,Debug,Clone)]
pub struct Block {
    pub inp: Path,
    pub outp: Path,
    pub data: BlockData,
}

#[derive(Eq,PartialEq,Hash,Debug,Clone)]
pub enum BlockData {
    Direct(EqualityData),
    Inv(EqualityData),
    Funct(Functor, Eq),
}

impl Eq {
    fn append_block(&mut self, blk: Block) {
    }
}

impl Slice {
    // Try to insert a block. Return None on success, and give back the block on failure
    fn insert_block_at(&mut self, start: usize, blk: Block) -> Option<Block> {
        // if !self.block_compatible(start, &blk) {
        //     return Some(blk)
        // }
        //
        // let r = self.blocks.binary_search_by_key(&start, |path| path.0).unwrap_err();
        // TODO handle functors
        // self.blocks.insert(r, (start,blk));

        // inp doesn't change, since we insert on a passthrough place
        // However output changes
        todo!()
    }

    // Test if the block is compatible with the slice (assumes well typedness)
    fn block_compatible(&self, start: usize, blk: &Block) -> bool {
        // for (st,b) in &self.blocks {
        //     if start < st + b.outp.1.len() && *st < start + blk.inp.1.len() {
        //         return false;
        //     }
        // }
        // return true;
        todo!()
    }

    // Given an input index, indicates which output it connects to or if it goes
    // into a block (giving the block index and the index of the input of the
    // block).
    pub fn input_target(&self, input: usize) -> Result<usize,(usize,usize)> {
        let r = self.blocks.binary_search_by(|(src,_,blk)| {
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
    pub fn output_source(&self, output: usize) -> Result<usize,(usize,usize)> {
        let r = self.blocks.binary_search_by(|(_,src,blk)| {
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
    use crate::graph::eq::{Slice,Block,BlockData};
    use crate::data::{Context,EqualityData,ActualProofObject};
    use crate::dsl::{cat,obj,mph};

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

    #[test]
    fn slice_input_output() {
        let b31 = Block {
            inp: (0, vec![(0,0); 3]),
            outp: (0, vec![(0,0); 1]),
            data: dummy_data(), 
        };
        let b14 = Block {
            inp: (0, vec![(0,0); 1]),
            outp: (0, vec![(0,0); 4]),
            data: dummy_data(), 
        };
        let b25 = Block {
            inp: (0, vec![(0,0); 2]),
            outp: (0, vec![(0,0); 5]),
            data: dummy_data(), 
        };
        let slice = Slice {
            inp: (0, vec![(0,0); 10]),
            outp: (0, vec![(0,0); 14]),
            blocks: vec![ (1,1,b31), (5,3,b25), (9,10,b14) ],
        };

        assert_eq!(slice.input_target(0), Ok(0));
        assert_eq!(slice.input_target(2), Err((0,1)));
        assert_eq!(slice.input_target(4), Ok(2));
        assert_eq!(slice.input_target(8), Ok(9));
        assert_eq!(slice.input_target(9), Err((2,0)));

        assert_eq!(slice.output_source(0), Ok(0));
        assert_eq!(slice.output_source(1), Err((0,0)));
        assert_eq!(slice.output_source(2), Ok(4));
        assert_eq!(slice.output_source(4), Err((1,1)));
        assert_eq!(slice.output_source(6), Err((1,3)));
        assert_eq!(slice.output_source(8), Ok(7));
        assert_eq!(slice.output_source(12), Err((2,2)));
    }
}
