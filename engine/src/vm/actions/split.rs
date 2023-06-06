use crate::data::{ActualEquality, ActualMorphism, Morphism};
use crate::graph::{Face, GraphId};
use crate::normalize;
use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::{Interactive, VM};
use std::ops::Deref;

type Ins = asm::Instruction;

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    pub fn split(&mut self, src: usize, mph: usize) {
        if let Some(fce) = self.split_norm(src, mph) {
            self.extend_face_in_eqs(fce);
        }
    }

    /// Normalize a morphism of the graph, then split it along composition,
    /// introduce the components as edges, add a face between the source morphism
    /// and the new path, and return the index of the new face or None is the
    /// morphism is already normalized and split
    pub fn split_norm(&mut self, src: usize, mph: usize) -> Option<usize> {
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

    // fce must be a face from a single morphism. Its equality is then inserted
    // in all equalities that have on side that go through this morphism. Then
    // it hides the morphism and the face.
    fn extend_face_in_eqs(&mut self, fce: usize) {
        use ActualEquality::*;
        assert_eq!(
            self.graph.faces[fce].left.len(),
            1,
            "extend_face_in_eqs expect a face from a single morphism"
        );
        let src = self.graph.faces[fce].start;
        let mph = self.graph.faces[fce].left[0];

        for face in 0..self.graph.faces.len() {
            if face == fce || self.graph.faces[face].label.hidden {
                continue;
            }
            let mut changed = false;

            // Build equality on left side
            let mut node = self.graph.faces[face].start;
            let mut lefteq_vec = Vec::new();
            for nxt in &self.graph.faces[face].left {
                if node == src && *nxt == mph {
                    let neq = self.graph.faces[fce].eq.clone();
                    changed = true;
                    lefteq_vec.push(neq);
                } else {
                    let neq = self.ctx.mk(Refl(self.graph.edges[node][*nxt].2.clone()));
                    lefteq_vec.push(neq);
                }
                node = self.graph.edges[node][*nxt].0;
            }
            let lefteq = lefteq_vec
                .into_iter()
                .rev()
                .reduce(|eq1, eq2| self.ctx.mk(Compose(eq2, eq1)))
                .unwrap_or(self.ctx.mk(Refl(self.graph.faces[fce].eq.left(&self.ctx))));
            let lefteq_right = lefteq.right(&self.ctx);
            let (_, norm_left) = normalize::morphism(&mut self.ctx, lefteq_right);
            let lefteq = self.ctx.mk(Concat(lefteq, norm_left));
            assert!(lefteq.check(&self.ctx));
            assert_eq!(
                lefteq.left(&self.ctx),
                self.graph.faces[face].eq.left(&self.ctx)
            );

            // Build equality on right side
            let mut node = self.graph.faces[face].start;
            let mut righteq_vec = Vec::new();
            for nxt in &self.graph.faces[face].right {
                if node == src && *nxt == mph {
                    let neq = self.graph.faces[fce].eq.clone();
                    changed = true;
                    righteq_vec.push(neq);
                } else {
                    let neq = self.ctx.mk(Refl(self.graph.edges[node][*nxt].2.clone()));
                    righteq_vec.push(neq);
                }
                node = self.graph.edges[node][*nxt].0;
            }
            let righteq = righteq_vec
                .into_iter()
                .rev()
                .reduce(|eq1, eq2| self.ctx.mk(Compose(eq2, eq1)))
                .unwrap_or(self.ctx.mk(Refl(self.graph.faces[fce].eq.right(&self.ctx))));
            let righteq_right = righteq.right(&self.ctx);
            let (_, norm_right) = normalize::morphism(&mut self.ctx, righteq_right);
            let righteq = self.ctx.mk(Concat(righteq, norm_right));
            assert!(righteq.check(&self.ctx));
            assert_eq!(
                righteq.left(&self.ctx),
                self.graph.faces[face].eq.right(&self.ctx)
            );

            if !changed {
                continue;
            }

            // Update left and right paths
            let rep = self.graph.faces[fce].right.clone();
            let replace =
                |node: &mut usize, nxt: &usize| -> Option<Box<dyn Iterator<Item = usize>>> {
                    let (dst, _, _) = &self.graph.edges[*node][*nxt];
                    let prev = *node;
                    *node = *dst;
                    if prev == src && *nxt == mph {
                        Some(Box::new(rep.iter().map(|i| *i)))
                    } else {
                        Some(Box::new(std::iter::once(*nxt)))
                    }
                };

            let nxt_mph = |src: &mut usize, mph: usize| -> Option<Morphism> {
                let prev = *src;
                *src = self.graph.edges[*src][mph].0;
                Some(self.graph.edges[prev][mph].2.clone())
            };
            let start = self.graph.faces[face].start;

            let left = self.graph.faces[face]
                .left
                .iter()
                .scan(start, replace)
                .flatten()
                .collect::<Vec<_>>();
            let left_mph = left
                .iter()
                .copied()
                .scan(start, nxt_mph)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .reduce(|mph2, mph1| self.ctx.mk(ActualMorphism::Comp(mph1, mph2)))
                .unwrap_or(
                    self.ctx
                        .mk(ActualMorphism::Identity(self.graph.nodes[start].0.clone())),
                );
            let (_, left_mph_eq) = normalize::morphism(&mut self.ctx, left_mph);
            let lefteq = self.ctx.mk(Concat(lefteq, self.ctx.mk(Inv(left_mph_eq))));

            let right = self.graph.faces[face]
                .right
                .iter()
                .scan(start, replace)
                .flatten()
                .collect::<Vec<_>>();
            let right_mph = right
                .iter()
                .copied()
                .scan(start, nxt_mph)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .reduce(|mph2, mph1| self.ctx.mk(ActualMorphism::Comp(mph1, mph2)))
                .unwrap_or(
                    self.ctx
                        .mk(ActualMorphism::Identity(self.graph.nodes[start].0.clone())),
                );
            let (_, right_mph_eq) = normalize::morphism(&mut self.ctx, right_mph);
            let righteq = self.ctx.mk(Concat(righteq, self.ctx.mk(Inv(right_mph_eq))));

            self.conjugate_face(face, lefteq, righteq, left, right);
        }

        self.hide(GraphId::Face(fce));
        self.hide(GraphId::Morphism(src, mph));
        self.relabel();
    }

    /// Split a morphism along compositions, add the components to the graph, and
    /// add a reflexivity face between the source morphism and the new path. Returns
    /// the id of the new face or None if the morphism is already fully split
    #[allow(dead_code)]
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
    use crate::data::Context;
    use crate::dsl::{cat, mph, obj};
    use crate::remote::Mock;
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
        let mut vm = VM::<Mock, ()>::new(ctx, gr, Vec::new(), Vec::new());
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
        let mut vm = VM::<Mock, ()>::new(ctx, gr, Vec::new(), Vec::new());
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
