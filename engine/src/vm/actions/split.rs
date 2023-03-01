use crate::data::Morphism;
use crate::graph::Face;
use crate::normalize;
use crate::vm::asm;
use crate::vm::graph::FaceLabel;
use crate::vm::VM;
use std::ops::Deref;

type Ins = asm::Instruction;

impl VM {
    pub fn split(&mut self, src: usize, mph: usize) {
        if let Some(fce) = self.split_norm(src, mph) {
            let render = self.graph.faces[fce].eq.render(&mut self.ctx, 100);
            self.register_instruction(Ins::UpdateFaceLabel(
                fce,
                asm::Updater {
                    direct: Box::new(move |label: &mut FaceLabel| {
                        label.label = render.clone();
                        label.hidden = true;
                    }),
                    reverse: Box::new(|label: &mut FaceLabel| {
                        // We don't other restoring to previous values since the face will
                        // be removed just after
                        label.label.clear();
                        label.hidden = false;
                    }),
                },
            ));
            self.hide_and_replace_morphism(src, mph, fce)
        }
    }

    /// Normalize a morphism of the graph, then split it along composition,
    /// introduce the components as edges, add a face between the source morphism
    /// and the new path, and return the index of the new face or None is the
    /// morphism is already normalized and split
    fn split_norm(&mut self, src: usize, mph: usize) -> Option<usize> {
        assert!(src < self.graph.nodes.len(), "src out of bounds");
        assert!(mph < self.graph.edges[src].len(), "mph out of bounds");
        let (norm, eqnorm) =
            normalize::morphism(&mut self.ctx, self.graph.edges[src][mph].2.clone());
        let is_norm = norm == self.graph.edges[src][mph].2;
        let (mut path, dst) = self.insert_split_at(src, norm);
        if is_norm && path.len() == 1 {
            return None;
        }
        path.reverse();
        let fce = Face {
            start: src,
            end: dst,
            left: vec![mph],
            right: path,
            eq: eqnorm,
            label: Default::default(),
        };
        self.register_instruction(Ins::InsertFace(fce));
        Some(self.graph.faces.len() - 1)
    }

    /// Split a morphism along compositions, add the components to the graph, and
    /// add a reflexivity face between the source morphism and the new path. Returns
    /// the id of the new face or None if the morphism is already fully split
    fn split_mph(&mut self, src: usize, mph: usize) -> Option<usize> {
        assert!(src < self.graph.nodes.len(), "src out of bounds");
        assert!(mph < self.graph.edges[src].len(), "mph out of bounds");
        let (mut path, dst) = self.insert_split_at(src, self.graph.edges[src][mph].2.clone());
        if path.len() == 1 {
            return None;
        }
        path.reverse();
        let fce = Face {
            start: src,
            end: dst,
            left: vec![mph],
            right: path.clone(),
            eq: self.ctx.mk(crate::data::ActualEquality::Refl(
                self.graph.edges[src][mph].2.clone(),
            )),
            label: Default::default(),
        };
        self.register_instruction(Ins::InsertFace(fce));
        Some(self.graph.faces.len() - 1)
    }

    /// Split a morphism along composition and add its components to the graph. The
    /// returned vector is in reverse order.
    fn insert_split_at(&mut self, src: usize, mph: Morphism) -> (Vec<usize>, usize) {
        use crate::data::ActualMorphism::Comp;
        match mph.deref() {
            Comp(l, r) => {
                let (mph_id, dst) = self.insert_mph_at(src, l.clone());
                let (mut res, ndst) = self.insert_split_at(dst, r.clone());
                res.push(mph_id);
                (res, ndst)
            }
            _ => {
                let (mph_id, ndst) = self.insert_mph_at(src, mph);
                (vec![mph_id], ndst)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::data::{ActualCategory, ActualMorphism, ActualObject};
    use crate::data::{CategoryData, MorphismData, ObjectData};
    use crate::data::{Context, ProofObject};
    use crate::dsl::{cat, mph, obj};
    use crate::vm::{Graph, VM};
    use std::default::Default;

    #[test]
    fn split() {
        let ctx = Context::new();
        let cat = cat!(ctx, :0);
        let v = obj!(ctx, (:1) in cat);
        let w = obj!(ctx, (:2) in cat);
        let x = obj!(ctx, (:3) in cat);
        let y = obj!(ctx, (:4) in cat);
        let z = obj!(ctx, (:5) in cat);
        let m1 = mph!(ctx, (:6) : v -> w);
        let m2 = mph!(ctx, (:7) : w -> x);
        let m3 = mph!(ctx, (:8) : x -> y);
        let m4 = mph!(ctx, (:9) : y -> z);

        let m = mph!(ctx, m1 >> (m2 >> (m3 >> m4)));
        assert!(m.check(&ctx), "m should be a valid morphism");

        let gr = Graph {
            nodes: vec![(v, Default::default()), (z, Default::default())],
            edges: vec![vec![(1, Default::default(), m)], vec![]],
            faces: vec![],
        };
        let mut vm = VM::new(ctx, gr);
        vm.split(0, 0);

        assert!(vm.graph.check(&vm.ctx), "Graph is not valid after split");
        assert_eq!(vm.graph.nodes.len(), 5, "There should be 5 nodes now");
        assert_eq!(
            vm.graph.edges[0].len(),
            2,
            "There should be two outgoing edges from first node"
        );
        assert_eq!(vm.graph.faces.len(), 1, "There should be a face");
        assert_ne!(
            vm.instructions.len(),
            0,
            "There should be at least one instruction"
        );
    }

    #[test]
    fn split_norm() {
        let ctx = Context::new();
        let cat = cat!(ctx, :0);
        let v = obj!(ctx, (:1) in cat);
        let w = obj!(ctx, (:2) in cat);
        let x = obj!(ctx, (:3) in cat);
        let y = obj!(ctx, (:4) in cat);
        let z = obj!(ctx, (:5) in cat);
        let m1 = mph!(ctx, (:6) : v -> w);
        let m2 = mph!(ctx, (:7) : w -> x);
        let m3 = mph!(ctx, (:8) : x -> y);
        let m4 = mph!(ctx, (:9) : y -> z);

        let m = mph!(ctx, (m1 >> m2) >> (m3 >> m4));
        assert!(m.check(&ctx), "m should be a valid morphism");

        let gr = Graph {
            nodes: vec![(v, Default::default()), (z, Default::default())],
            edges: vec![vec![(1, Default::default(), m)], vec![]],
            faces: vec![],
        };
        let mut vm = VM::new(ctx, gr);
        vm.split_norm(0, 0);

        assert!(vm.graph.check(&vm.ctx), "Graph is not valid after split");
        assert_eq!(vm.graph.nodes.len(), 5, "There should be 5 nodes now");
        assert_eq!(
            vm.graph.edges[0].len(),
            2,
            "There should be two outgoing edges from first node"
        );
        assert_eq!(vm.graph.faces.len(), 1, "There should be a face");
        assert_ne!(
            vm.instructions.len(),
            0,
            "The should be at least one instruction"
        );
    }
}
