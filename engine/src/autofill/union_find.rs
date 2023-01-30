use crate::data::{ActualEquality, Context, Equality, Morphism};
use crate::graph::Path;

/// parent is the index of the representative of the cell equivalence class,
/// rank is a score used for optimisating the algorithm (it has no relevance to
/// its correctness). path is the path represented by the cell, and eq is an
/// equality between the morphism of path and the morphism of the path of the
/// parent.
struct Cell {
    parent: usize,
    rank: usize,
    path: Path,
    eq: Equality,
}

impl Cell {
    fn new(ctx: &mut Context, id: usize, path: Path) -> Cell {
        let mph = path.mph.clone();
        Cell {
            parent: id,
            rank: 1,
            path,
            eq: ctx.mk(ActualEquality::Refl(mph)),
        }
    }
}

/// Cells are sorted according to the hconsed id of the morphism of the path,
/// allowing logarithmic lookup of a cell index by its morphism.
pub struct UF {
    cells: Vec<Cell>,
    hooks: Vec<Box<dyn Fn(Equality, &mut Vec<Equality>)>>,
}

impl UF {
    pub fn new(ctx: &mut Context, mut e: Vec<Path>) -> UF {
        e.sort_unstable_by(|p1, p2| std::cmp::Ord::cmp(&p1.mph.uid(), &p2.mph.uid()));
        let cells = e
            .into_iter()
            .enumerate()
            .map(|(i, p)| Cell::new(ctx, i, p))
            .collect();
        UF {
            cells,
            hooks: Vec::new(),
        }
    }

    pub fn register_hook<F>(&mut self, f: F)
    where
        F: Fn(Equality, &mut Vec<Equality>) + 'static,
    {
        self.hooks.push(Box::new(f))
    }

    /// Look for morphism in cellules
    fn lookup(&self, m: &Morphism) -> Option<usize> {
        let r = self
            .cells
            .binary_search_by_key(&m.uid(), |c| c.path.mph.uid());
        r.ok()
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
    pub fn query(&mut self, ctx: &mut Context, m1: &Morphism, m2: &Morphism) -> Option<Equality> {
        let id1 = self.lookup(m1)?;
        let id2 = self.lookup(m2)?;
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
    pub fn connect(&mut self, ctx: &mut Context, eq: Equality) -> bool {
        let mut eqs = vec![eq];
        let mut ret = false;
        while let Some(eq) = eqs.pop() {
            let m1 = eq.left(ctx);
            let m2 = eq.right(ctx);
            if let Some(id1) = self.lookup(&m1) {
                if let Some(id2) = self.lookup(&m2) {
                    let b = self.union(ctx, id1, id2, eq.clone());
                    if b {
                        ret = true;
                        self.hooks.iter().for_each(|hk| hk(eq.clone(), &mut eqs))
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
    use crate::dsl::{cat, mph, obj};
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
        let eq12 = ctx.mk(ActualEquality::Atomic(EqualityData {
            pobj: ProofObject::Term(8),
            category: cat.clone(),
            src: x.clone(),
            dst: y.clone(),
            left: m1.clone(),
            right: m2.clone(),
        }));
        ctx.new_term(8,  "eq8");
        let eq13 = ctx.mk(ActualEquality::Atomic(EqualityData {
            pobj: ProofObject::Term(9),
            category: cat.clone(),
            src: x.clone(),
            dst: y.clone(),
            left: m1.clone(),
            right: m3.clone(),
        }));
        ctx.new_term(9, "eq9");
        let eq45 = ctx.mk(ActualEquality::Atomic(EqualityData {
            pobj: ProofObject::Term(10),
            category: cat.clone(),
            src: x.clone(),
            dst: y.clone(),
            left: m4.clone(),
            right: m5.clone(),
        }));
        ctx.new_term(10, "eq10");

        let gr: Graph = Graph {
            nodes: vec![x, y],
            edges: vec![
                vec![(1, m1.clone()), (1, m2.clone()), (1, m3.clone()), (1, m4.clone()), (1, m5.clone())],
                Vec::new(),
            ],
            faces: Vec::new(),
        };

        let e = gr.enumerate(&mut ctx, 1);
        let mut uf = UF::new(&mut ctx, e);
        let c1 = uf.connect(&mut ctx, eq12);
        let c2 = uf.connect(&mut ctx, eq13);
        let c3 = uf.connect(&mut ctx, eq45);

        assert!(c1, "m1 and m2 were not connected");
        assert!(c2, "m1 and m3 were not connected");
        assert!(c3, "m4 and m5 were not connected");

        let q23 = uf.query(&mut ctx, &m2, &m3);
        assert!(q23.is_some(), "m2 and m3 should be connected");
        assert!(q23.unwrap().check(&ctx), "Equality between m2 and m3 invalid");

        let q54 = uf.query(&mut ctx, &m5, &m4);
        assert!(q54.is_some(), "m5 and m4 should be connected");
        assert!(q54.unwrap().check(&ctx), "Equality between m5 and m4 invalid");

        let q15 = uf.query(&mut ctx, &m1, &m5);
        assert!(q15.is_none(), "m1 and m5 should not be connected");
    }
}
