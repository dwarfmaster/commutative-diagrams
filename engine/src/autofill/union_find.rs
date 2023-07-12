use super::enumeration::Enum;
use crate::graph::eq::{Eq, Morphism};
use crate::remote::TermEngine;

/// parent is the index of the representative of the cell equivalence class,
/// rank is a score used for optimisating the algorithm (it has no relevance to
/// its correctness). eq is an equality between the morphism of the cell and the
/// morphism of the path of the parent. Remember which morphism is where is left
/// for the user of the union_find.
struct Cell {
    parent: usize,
    rank: usize,
    eq: Eq,
}

impl Cell {
    fn new(id: usize, cat: u64, mph: Morphism) -> Cell {
        Cell {
            parent: id,
            rank: 1,
            eq: Eq::refl(cat, mph),
        }
    }
}

pub struct UF<R: TermEngine> {
    cells: Vec<Cell>,
    hooks: Vec<Box<dyn Fn(&mut R, Eq, &mut Vec<Eq>)>>,
}

impl<R: TermEngine> UF<R> {
    pub fn new(e: &Vec<(u64, Morphism)>) -> Self {
        let cells: Vec<_> = e
            .iter()
            .enumerate()
            .map(|(i, (cat, mph))| Cell::new(i, *cat, mph.clone()))
            .collect();
        UF {
            cells,
            hooks: Vec::new(),
        }
    }

    #[allow(dead_code)]
    pub fn debug_log(&self) {
        for (id, cell) in self.cells.iter().enumerate() {
            log::trace!("{} -> {}", id, cell.parent);
        }
    }

    pub fn register_hook<F>(&mut self, f: F)
    where
        F: Fn(&mut R, Eq, &mut Vec<Eq>) + 'static,
    {
        self.hooks.push(Box::new(f))
    }

    /// Implements the find operation of the union find, ie return the id of the
    /// parent, with an equality to it, while shortcutting all intermediate
    /// links
    fn find(&mut self, id: usize) -> (usize, Eq) {
        if self.cells[id].parent == id {
            (id, self.cells[id].eq.clone())
        } else {
            let (pid, peq) = self.find(self.cells[id].parent);
            if pid == self.cells[id].parent {
                return (pid, self.cells[id].eq.clone());
            } else {
                let mut eq = self.cells[id].eq.clone();
                eq.append(peq);
                self.cells[id].parent = pid;
                self.cells[id].eq = eq.clone();
                (pid, eq)
            }
        }
    }

    /// Return an equality if two morphism are in the same equivalence class, or
    /// nothing otherwise
    pub fn query(&mut self, id1: usize, id2: usize) -> Option<Eq> {
        log::trace!("Querying if path {} is connected to path {}", id1, id2);
        let (pid1, mut peq1) = self.find(id1);
        let (pid2, mut peq2) = self.find(id2);
        if pid1 == pid2 {
            peq2.inv();
            peq1.append(peq2);
            Some(peq1)
        } else {
            None
        }
    }

    /// Returns true if the cells id1 and id2 where disjoint before
    fn union(&mut self, id1: usize, id2: usize, eq: Eq) -> bool {
        let (p1, mut peq1) = self.find(id1);
        let (p2, peq2) = self.find(id2);
        if p1 == p2 {
            false
        } else {
            peq1.inv();
            peq1.append(eq);
            peq1.append(peq2);
            if self.cells[p1].rank < self.cells[p2].rank {
                self.cells[p2].parent = p1;
                peq1.inv();
                self.cells[p2].eq = peq1;
                self.cells[p1].rank += self.cells[p2].rank;
            } else {
                self.cells[p1].parent = p2;
                self.cells[p1].eq = peq1;
                self.cells[p2].rank += self.cells[p1].rank;
            }
            true
        }
    }

    /// Apply union on all equalities in vector, applying hooks each time
    pub fn connect(&mut self, rm: &mut R, enm: &Enum, eq: Eq) -> bool {
        let mut eqs = vec![eq];
        let mut ret = false;
        while let Some(eq) = eqs.pop() {
            let cat = eq.cat;
            let left = &eq.inp;
            let right = &eq.outp;
            let id1 = enm.get(cat, left);
            let id2 = enm.get(cat, right);
            match (id1, id2) {
                (Some(id1), Some(id2)) => {
                    let b = self.union(id1, id2, eq.clone());
                    if b {
                        ret = true;
                        self.hooks
                            .iter()
                            .for_each(|hk| hk(rm, eq.clone(), &mut eqs))
                    }
                }
                _ => {
                    if id1.is_none() {
                        log::trace!("Couldn't find in enumeration: {:#?}", left);
                    }
                    if id2.is_none() {
                        log::trace!("Couldn't find in enumeration: {:#?}", right);
                    }
                }
            }
        }
        ret
    }
}

#[cfg(test)]
mod tests {
    use crate::autofill::UF;
    use crate::data::EvarStatus::Grounded;
    use crate::data::Feature;
    use crate::graph::eq::{Eq, Morphism};
    use crate::graph::Graph;
    use crate::remote::Mock;
    use crate::vm::Context;

    #[test]
    fn basic_union() {
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
        let eq12 = ctx.new_term("H12".to_string(), None, Grounded);
        ctx.add_feat(
            eq12,
            Feature::Equality {
                cat,
                src: x,
                dst: y,
                left: m1,
                right: m2,
            },
        );
        let eq13 = ctx.new_term("H13".to_string(), None, Grounded);
        ctx.add_feat(
            eq13,
            Feature::Equality {
                cat,
                src: x,
                dst: y,
                left: m1,
                right: m3,
            },
        );
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

        let mph1 = Morphism::atom(x, y, m1);
        let mph2 = Morphism::atom(x, y, m2);
        let mph3 = Morphism::atom(x, y, m3);
        let mph4 = Morphism::atom(x, y, m4);
        let mph5 = Morphism::atom(x, y, m5);
        let eq12 = Eq::atomic(cat, mph1.clone(), mph2.clone(), eq12);
        let eq13 = Eq::atomic(cat, mph1.clone(), mph3.clone(), eq13);
        let eq45 = Eq::atomic(cat, mph4.clone(), mph5.clone(), eq45);

        let gr: Graph<(), (), ()> = Graph {
            nodes: vec![(x, cat, ()), (y, cat, ())],
            edges: vec![
                vec![
                    (1, (), m1.clone(), mph1.clone()),
                    (1, (), m2.clone(), mph2.clone()),
                    (1, (), m3.clone(), mph3.clone()),
                    (1, (), m4.clone(), mph4.clone()),
                    (1, (), m5.clone(), mph5.clone()),
                ],
                Vec::new(),
            ],
            faces: Vec::new(),
        };

        let e = gr.enumerate(1);
        let id1 = e.get(cat, &mph1).unwrap();
        let id2 = e.get(cat, &mph2).unwrap();
        let id3 = e.get(cat, &mph3).unwrap();
        let id4 = e.get(cat, &mph4).unwrap();
        let id5 = e.get(cat, &mph5).unwrap();
        let mut ctx = Context::new(ctx);
        let mut uf = UF::new(&e.paths);
        let c1 = uf.connect(&mut ctx, &e, eq12);
        let c2 = uf.connect(&mut ctx, &e, eq13);
        let c3 = uf.connect(&mut ctx, &e, eq45);

        assert!(c1, "m1 and m2 were not connected");
        assert!(c2, "m1 and m3 were not connected");
        assert!(c3, "m4 and m5 were not connected");

        let q23 = uf.query(id2, id3);
        assert!(q23.is_some(), "m2 and m3 should be connected");

        let q54 = uf.query(id5, id4);
        assert!(q54.is_some(), "m5 and m4 should be connected");

        let q15 = uf.query(id1, id5);
        assert!(q15.is_none(), "m1 and m5 should not be connected");
    }
}
