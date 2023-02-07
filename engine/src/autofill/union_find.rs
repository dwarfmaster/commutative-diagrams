use crate::data::{ActualEquality, Context, Equality, Morphism};
use crate::graph::{Enum, Path};

/// parent is the index of the representative of the cell equivalence class,
/// rank is a score used for optimisating the algorithm (it has no relevance to
/// its correctness). eq is an equality between the morphism of the cell and the
/// morphism of the path of the parent. Remember which morphism is where is left
/// for the user of the union_find.
struct Cell {
    parent: usize,
    rank: usize,
    eq: Equality,
}

impl Cell {
    fn new(ctx: &mut Context, id: usize, mph: Morphism) -> Cell {
        Cell {
            parent: id,
            rank: 1,
            eq: ctx.mk(ActualEquality::Refl(mph)),
        }
    }
}

pub struct UF {
    cells: Vec<Cell>,
    hooks: Vec<Box<dyn Fn(&mut Context, Equality, &mut Vec<Equality>)>>,
}

impl UF {
    pub fn new(ctx: &mut Context, e: &Vec<Path>) -> UF {
        let cells: Vec<_> = e
            .iter()
            .enumerate()
            .map(|(i, p)| Cell::new(ctx, i, p.mph.clone()))
            .collect();
        UF {
            cells,
            hooks: Vec::new(),
        }
    }

    pub fn register_hook<F>(&mut self, f: F)
    where
        F: Fn(&mut Context, Equality, &mut Vec<Equality>) + 'static,
    {
        self.hooks.push(Box::new(f))
    }

    /// Implements the find operation of the union find, ie return the id of the
    /// parent, with an equality to it, while shortcutting all intermediate
    /// links
    fn find(&mut self, ctx: &mut Context, id: usize) -> (usize, Equality) {
        if self.cells[id].parent == id {
            (id, self.cells[id].eq.clone())
        } else {
            let (pid, peq) = self.find(ctx, self.cells[id].parent);
            if pid == self.cells[id].parent {
                return (pid, self.cells[id].eq.clone());
            } else {
                let eq = ctx.mk(ActualEquality::Concat(self.cells[id].eq.clone(), peq));
                self.cells[id].parent = pid;
                self.cells[id].eq = eq.clone();
                (pid, eq)
            }
        }
    }

    /// Return an equality if two morphism are in the same equivalence class, or
    /// nothing otherwise
    pub fn query(&mut self, ctx: &mut Context, id1: usize, id2: usize) -> Option<Equality> {
        log::trace!("Querying if path {} is connected to path {}", id1, id2);
        let (pid1, peq1) = self.find(ctx, id1);
        let (pid2, peq2) = self.find(ctx, id2);
        if pid1 == pid2 {
            let eq = ctx.mk(ActualEquality::Inv(peq2));
            let eq = ctx.mk(ActualEquality::Concat(peq1, eq));
            Some(eq)
        } else {
            None
        }
    }

    /// Returns true if the cells id1 and id2 where disjoint before
    fn union(&mut self, ctx: &mut Context, id1: usize, id2: usize, eq: Equality) -> bool {
        let (p1, peq1) = self.find(ctx, id1);
        let (p2, peq2) = self.find(ctx, id2);
        if p1 == p2 {
            false
        } else {
            let eq = ctx.mk(ActualEquality::Concat(eq, peq2));
            let eq = ctx.mk(ActualEquality::Concat(
                ctx.mk(ActualEquality::Inv(peq1)),
                eq,
            ));
            if self.cells[p1].rank < self.cells[p2].rank {
                self.cells[p2].parent = p1;
                self.cells[p2].eq = ctx.mk(ActualEquality::Inv(eq));
                self.cells[p1].rank += self.cells[p2].rank;
            } else {
                self.cells[p1].parent = p2;
                self.cells[p1].eq = eq;
                self.cells[p2].rank += self.cells[p1].rank;
            }
            true
        }
    }

    /// Apply union on all equalities in vector, applying hooks each time
    pub fn connect(&mut self, ctx: &mut Context, enm: &Enum, eq: Equality) -> bool {
        let mut eqs = vec![eq];
        let mut ret = false;
        while let Some(eq) = eqs.pop() {
            let left = eq.left(ctx);
            let right = eq.right(ctx);
            let id1 = enm.get(&left);
            let id2 = enm.get(&right);
            match (id1, id2) {
                (Some(id1), Some(id2)) => {
                    let b = self.union(ctx, id1, id2, eq.clone());
                    if b {
                        ret = true;
                        self.hooks
                            .iter()
                            .for_each(|hk| hk(ctx, eq.clone(), &mut eqs))
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
    use crate::data::{ActualCategory, ActualMorphism, ActualObject};
    use crate::data::{ActualEquality, EqualityData};
    use crate::data::{CategoryData, MorphismData, ObjectData};
    use crate::data::{Context, ProofObject};
    use crate::dsl::{cat, eq, mph, obj};
    use crate::graph::Graph;

    #[test]
    fn basic_union() {
        let mut ctx = Context::new();
        let cat = cat!(ctx, :0);
        let x = obj!(ctx, (:1) in cat);
        let y = obj!(ctx, (:2) in cat);
        let m1 = mph!(ctx, (:3) : x -> y);
        let m2 = mph!(ctx, (:4) : x -> y);
        let m3 = mph!(ctx, (:5) : x -> y);
        let m4 = mph!(ctx, (:6) : x -> y);
        let m5 = mph!(ctx, (:7) : x -> y);
        let eq12 = eq!(ctx, (:8) : m1 == m2);
        let eq13 = eq!(ctx, (:9) : m1 == m3);
        let eq45 = eq!(ctx, (:10) : m4 == m5);

        let gr: Graph<(), (), ()> = Graph {
            nodes: vec![(x, ()), (y, ())],
            edges: vec![
                vec![
                    (1, (), m1.clone()),
                    (1, (), m2.clone()),
                    (1, (), m3.clone()),
                    (1, (), m4.clone()),
                    (1, (), m5.clone()),
                ],
                Vec::new(),
            ],
            faces: Vec::new(),
        };

        let e = gr.enumerate(&mut ctx, 1);
        let id1 = e.get(&m1).unwrap();
        let id2 = e.get(&m2).unwrap();
        let id3 = e.get(&m3).unwrap();
        let id4 = e.get(&m4).unwrap();
        let id5 = e.get(&m5).unwrap();
        let mut uf = UF::new(&mut ctx, &e.paths);
        let c1 = uf.connect(&mut ctx, &e, eq12);
        let c2 = uf.connect(&mut ctx, &e, eq13);
        let c3 = uf.connect(&mut ctx, &e, eq45);

        assert!(c1, "m1 and m2 were not connected");
        assert!(c2, "m1 and m3 were not connected");
        assert!(c3, "m4 and m5 were not connected");

        let q23 = uf.query(&mut ctx, id2, id3);
        assert!(q23.is_some(), "m2 and m3 should be connected");
        assert!(
            q23.unwrap().check(&ctx),
            "Equality between m2 and m3 invalid"
        );

        let q54 = uf.query(&mut ctx, id5, id4);
        assert!(q54.is_some(), "m5 and m4 should be connected");
        assert!(
            q54.unwrap().check(&ctx),
            "Equality between m5 and m4 invalid"
        );

        let q15 = uf.query(&mut ctx, id1, id5);
        assert!(q15.is_none(), "m1 and m5 should not be connected");
    }
}
