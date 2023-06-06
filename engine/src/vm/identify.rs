use crate::anyterm::{AnyTerm, IsTerm};
use crate::data::{ActualCategory, ActualEquality, ActualFunctor, ActualMorphism, ActualObject};
use crate::data::{ActualProofObject, Context};
use crate::data::{Category, Equality, Functor, Morphism, Object};
use crate::data::{CategoryData, EqualityData, FunctorData, MorphismData, ObjectData};
use crate::graph::GraphId;
use crate::remote::Remote;
use crate::substitution::Substitutable;
use crate::unification::{unify, UnifOpts, UnifState};
use crate::vm::ast::{Annot, Id, TermDescr};
use crate::vm::{Interactive, VM};

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    fn realize_descr_as_cat(
        &mut self,
        _unif: &mut UnifState,
        descr: &TermDescr,
    ) -> Option<Category> {
        use ActualCategory::*;
        use TermDescr::*;
        match descr {
            Ref(_) => None, // Only nodes, edges and faces can have names
            Hole => {
                let ex = self.ctx.new_existential();
                Some(self.ctx.mk(Atomic(CategoryData {
                    pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
                })))
            }
        }
    }

    pub fn descr_as_cat(&mut self, descr: &TermDescr) -> Option<Category> {
        let mut unif = UnifState::new();
        let cat = self.realize_descr_as_cat(&mut unif, descr)?;
        let sigma = unif.solve()?;
        Some(cat.subst(&self.ctx, &sigma))
    }

    fn realize_descr_as_functor(
        &mut self,
        _unif: &mut UnifState,
        descr: &TermDescr,
        src: Category,
        dst: Category,
    ) -> Option<Functor> {
        use ActualFunctor::*;
        use TermDescr::*;
        match descr {
            Ref(_) => None, // Only nodes, edges and faces can have names
            Hole => {
                let ex = self.ctx.new_existential();
                Some(self.ctx.mk(Atomic(FunctorData {
                    pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
                    src,
                    dst,
                })))
            }
        }
    }

    pub fn descr_as_functor(&mut self, descr: &TermDescr) -> Option<Functor> {
        let mut unif = UnifState::new();

        // src
        let ex = self.ctx.new_existential();
        let src = self.ctx.mk(ActualCategory::Atomic(CategoryData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
        }));

        // dst
        let ex = self.ctx.new_existential();
        let dst = self.ctx.mk(ActualCategory::Atomic(CategoryData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
        }));

        let funct = self.realize_descr_as_functor(&mut unif, descr, src, dst)?;
        let sigma = unif.solve()?;
        Some(funct.subst(&self.ctx, &sigma))
    }

    fn realize_descr_as_object(
        &mut self,
        unif: &mut UnifState,
        descr: &TermDescr,
        cat: Category,
    ) -> Option<Object> {
        use ActualObject::*;
        use TermDescr::*;
        let mut register = |obj: Object| -> Object {
            let ncat = obj.cat(&self.ctx);
            unif.add(&self.ctx, cat.clone().term());
            unif.add(&self.ctx, ncat.clone().term());
            unif.add_goal(cat.clone().term(), ncat.term());
            obj
        };
        match descr {
            Ref(Annot {
                value: Id::Id(id),
                range: _,
            }) => {
                if *id < self.graph.nodes.len() {
                    Some(register(self.graph.nodes[*id].0.clone()))
                } else {
                    None
                }
            }
            Ref(Annot {
                value: Id::Name(name),
                range: _,
            }) => {
                if let Some(GraphId::Node(id)) = self.names.get(name) {
                    Some(register(self.graph.nodes[*id].0.clone()))
                } else {
                    None
                }
            }
            Hole => {
                let ex = self.ctx.new_existential();
                Some(self.ctx.mk(Atomic(ObjectData {
                    pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
                    category: cat,
                })))
            }
        }
    }

    pub fn descr_as_object(&mut self, descr: &TermDescr) -> Option<Object> {
        let mut unif = UnifState::new();

        // cat
        let ex = self.ctx.new_existential();
        let cat = self.ctx.mk(ActualCategory::Atomic(CategoryData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
        }));

        let obj = self.realize_descr_as_object(&mut unif, descr, cat)?;
        let sigma = unif.solve()?;
        Some(obj.subst(&self.ctx, &sigma))
    }

    pub fn identify_node(&mut self, descr: &TermDescr) -> Option<usize> {
        // Shortcut for refs
        if let TermDescr::Ref(id) = descr {
            match &id.value {
                Id::Name(name) => {
                    let id = self.names.get(name)?;
                    match id {
                        GraphId::Node(nd) => {
                            if self.graph.nodes[*nd].1.hidden {
                                return None;
                            } else {
                                return Some(*nd);
                            }
                        }
                        _ => return None,
                    }
                }
                Id::Id(id) => {
                    if *id < self.graph.nodes.len() && !self.graph.nodes[*id].1.hidden {
                        return Some(*id);
                    } else {
                        return None;
                    }
                }
            }
        }

        // Build termdescr and unify
        let obj = self.descr_as_object(descr)?;
        let obj = obj.term();
        for i in 0..self.graph.nodes.len() {
            if self.graph.nodes[i].1.hidden {
                continue;
            }
            if match_term(&self.ctx, obj.clone(), self.graph.nodes[i].0.clone().term()) {
                return Some(i);
            }
        }
        None
    }

    fn realize_descr_as_morphism(
        &mut self,
        unif: &mut UnifState,
        descr: &TermDescr,
        src: Object,
        dst: Object,
    ) -> Option<Morphism> {
        use ActualMorphism::*;
        use TermDescr::*;
        let category = src.cat(&self.ctx);
        let mut register = |mph: Morphism| -> Morphism {
            let ncat = mph.cat(&self.ctx);
            unif.add(&self.ctx, category.clone().term());
            unif.add(&self.ctx, ncat.clone().term());
            unif.add_goal(category.clone().term(), ncat.term());
            let nsrc = mph.src(&self.ctx);
            unif.add(&self.ctx, src.clone().term());
            unif.add(&self.ctx, nsrc.clone().term());
            unif.add_goal(src.clone().term(), nsrc.term());
            let ndst = mph.dst(&self.ctx);
            unif.add(&self.ctx, dst.clone().term());
            unif.add(&self.ctx, ndst.clone().term());
            unif.add_goal(dst.clone().term(), ndst.term());
            mph
        };
        match descr {
            Ref(Annot {
                value: Id::Id(id),
                range: _,
            }) => {
                let (src, mph) = self.graph.edge_by_id(*id)?;
                Some(register(self.graph.edges[src][mph].2.clone()))
            }
            Ref(Annot {
                value: Id::Name(name),
                range: _,
            }) => {
                if let Some(GraphId::Morphism(src, mph)) = self.names.get(name) {
                    Some(register(self.graph.edges[*src][*mph].2.clone()))
                } else {
                    None
                }
            }
            Hole => {
                let ex = self.ctx.new_existential();
                Some(self.ctx.mk(Atomic(MorphismData {
                    pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
                    src,
                    dst,
                    category,
                })))
            }
        }
    }

    pub fn descr_as_morphism(&mut self, descr: &TermDescr) -> Option<Morphism> {
        let mut unif = UnifState::new();

        // Cat
        let ex = self.ctx.new_existential();
        let cat = self.ctx.mk(ActualCategory::Atomic(CategoryData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
        }));

        // Src
        let ex = self.ctx.new_existential();
        let src = self.ctx.mk(ActualObject::Atomic(ObjectData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
            category: cat.clone(),
        }));

        // Dst
        let ex = self.ctx.new_existential();
        let dst = self.ctx.mk(ActualObject::Atomic(ObjectData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
            category: cat,
        }));

        let mph = self.realize_descr_as_morphism(&mut unif, descr, src, dst)?;
        let sigma = unif.solve()?;
        Some(mph.subst(&self.ctx, &sigma))
    }

    pub fn identify_edge(&mut self, descr: &TermDescr) -> Option<(usize, usize)> {
        // Shortcut for refs
        if let TermDescr::Ref(id) = descr {
            match &id.value {
                Id::Name(name) => {
                    let id = self.names.get(name)?;
                    match id {
                        GraphId::Morphism(src, mph) => {
                            if self.graph.edges[*src][*mph].1.hidden {
                                return None;
                            } else {
                                return Some((*src, *mph));
                            }
                        }
                        _ => return None,
                    }
                }
                Id::Id(id) => {
                    let (src, mph) = self.graph.edge_by_id(*id)?;
                    if self.graph.edges[src][mph].1.hidden {
                        return None;
                    } else {
                        return Some((src, mph));
                    }
                }
            }
        }

        // Build termdescr and unify
        let mph = self.descr_as_morphism(descr)?;
        let mph = mph.term();
        for src in 0..self.graph.edges.len() {
            for m in 0..self.graph.edges[src].len() {
                if self.graph.edges[src][m].1.hidden {
                    continue;
                }
                if match_term(
                    &self.ctx,
                    mph.clone(),
                    self.graph.edges[src][m].2.clone().term(),
                ) {
                    return Some((src, m));
                }
            }
        }
        None
    }

    fn realize_descr_as_equality(
        &mut self,
        unif: &mut UnifState,
        descr: &TermDescr,
        left: Morphism,
        right: Morphism,
    ) -> Option<Equality> {
        use ActualEquality::*;
        use TermDescr::*;
        let category = left.cat(&self.ctx);
        let src = left.src(&self.ctx);
        let dst = right.dst(&self.ctx);
        let mut register = |eq: Equality| -> Equality {
            let ncat = eq.cat(&self.ctx);
            unif.add(&self.ctx, category.clone().term());
            unif.add(&self.ctx, ncat.clone().term());
            unif.add_goal(category.clone().term(), ncat.term());
            let nsrc = eq.src(&self.ctx);
            unif.add(&self.ctx, src.clone().term());
            unif.add(&self.ctx, nsrc.clone().term());
            unif.add_goal(src.clone().term(), nsrc.term());
            let ndst = eq.dst(&self.ctx);
            unif.add(&self.ctx, dst.clone().term());
            unif.add(&self.ctx, ndst.clone().term());
            unif.add_goal(dst.clone().term(), ndst.term());
            let nleft = eq.left(&self.ctx);
            unif.add(&self.ctx, left.clone().term());
            unif.add(&self.ctx, nleft.clone().term());
            unif.add_goal(left.clone().term(), nleft.term());
            let nright = eq.right(&self.ctx);
            unif.add(&self.ctx, right.clone().term());
            unif.add(&self.ctx, nright.clone().term());
            unif.add_goal(right.clone().term(), nright.term());
            eq
        };
        match descr {
            Ref(Annot {
                value: Id::Id(id),
                range: _,
            }) => {
                if *id < self.graph.faces.len() {
                    Some(register(self.graph.faces[*id].eq.clone()))
                } else {
                    None
                }
            }
            Ref(Annot {
                value: Id::Name(name),
                range: _,
            }) => {
                if let Some(GraphId::Face(id)) = self.names.get(name) {
                    Some(register(self.graph.faces[*id].eq.clone()))
                } else {
                    None
                }
            }
            Hole => {
                let ex = self.ctx.new_existential();
                Some(self.ctx.mk(Atomic(EqualityData {
                    pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
                    src,
                    dst,
                    category,
                    left,
                    right,
                })))
            }
        }
    }

    pub fn descr_as_equality(&mut self, descr: &TermDescr) -> Option<Equality> {
        let mut unif = UnifState::new();

        // Cat
        let ex = self.ctx.new_existential();
        let cat = self.ctx.mk(ActualCategory::Atomic(CategoryData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
        }));

        // Src
        let ex = self.ctx.new_existential();
        let src = self.ctx.mk(ActualObject::Atomic(ObjectData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
            category: cat.clone(),
        }));

        // Dst
        let ex = self.ctx.new_existential();
        let dst = self.ctx.mk(ActualObject::Atomic(ObjectData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
            category: cat.clone(),
        }));

        // Left
        let ex = self.ctx.new_existential();
        let left = self.ctx.mk(ActualMorphism::Atomic(MorphismData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
            category: cat.clone(),
            src: src.clone(),
            dst: dst.clone(),
        }));

        // Right
        let ex = self.ctx.new_existential();
        let right = self.ctx.mk(ActualMorphism::Atomic(MorphismData {
            pobj: self.ctx.mk(ActualProofObject::Existential(ex)),
            category: cat,
            src,
            dst,
        }));

        let eq = self.realize_descr_as_equality(&mut unif, descr, left, right)?;
        let sigma = unif.solve()?;
        Some(eq.subst(&self.ctx, &sigma))
    }

    pub fn identify_face(&mut self, descr: &TermDescr) -> Option<usize> {
        // Shortcut for refs
        if let TermDescr::Ref(id) = descr {
            match &id.value {
                Id::Name(name) => {
                    let id = self.names.get(name)?;
                    match id {
                        GraphId::Face(fce) => {
                            if self.graph.faces[*fce].label.hidden {
                                return None;
                            } else {
                                return Some(*fce);
                            }
                        }
                        _ => return None,
                    }
                }
                Id::Id(id) => {
                    if *id < self.graph.faces.len() && !self.graph.faces[*id].label.hidden {
                        return Some(*id);
                    } else {
                        return None;
                    }
                }
            }
        }

        // Build termdescr and unify
        let eq = self.descr_as_equality(descr)?;
        let eq = eq.term();
        for fce in 0..self.graph.faces.len() {
            if self.graph.faces[fce].label.hidden {
                continue;
            }
            if match_term(
                &self.ctx,
                eq.clone(),
                self.graph.faces[fce].eq.clone().term(),
            ) {
                return Some(fce);
            }
        }
        None
    }
}

fn match_term(ctx: &Context, pattern: AnyTerm, term: AnyTerm) -> bool {
    unify(
        ctx,
        pattern,
        term,
        UnifOpts {
            right_ground: true,
            ..Default::default()
        },
    )
    .is_some()
}

#[cfg(test)]
mod tests {
    use crate::anyterm::IsTerm;
    use crate::data::Context;
    use crate::dsl::{cat, obj};
    use crate::remote::Mock;
    use crate::vm::ast::{Annot, Id, TermDescr};
    use crate::vm::identify;
    use crate::vm::VM;
    use crate::vm::{Graph, NodeLabel};

    #[test]
    fn realize() {
        let ctx = Context::new();
        let cat = cat!(ctx, (:0));
        let x = obj!(ctx, (:1) in cat);

        let gr = Graph {
            nodes: vec![(x, NodeLabel::new())],
            edges: vec![vec![]],
            faces: vec![],
        };
        let mut vm = VM::<Mock, ()>::new(ctx, gr, Vec::new(), Vec::new());

        let descr = TermDescr::Ref(Annot {
            value: Id::Id(0),
            range: std::ops::Range { start: 0, end: 1 },
        });
        let obj = vm.descr_as_object(&descr).unwrap();
        assert!(obj.check(&vm.ctx), "Realization is invalid");
    }

    #[test]
    fn match_term() {
        let ctx = Context::new();
        let cat = cat!(ctx, (:0));
        let x = obj!(ctx, (?0) in cat).term();
        let y = obj!(ctx, (:1) in cat).term();

        assert!(
            identify::match_term(&ctx, x.clone(), y.clone()),
            "Match in one direction"
        );

        assert!(
            !identify::match_term(&ctx, y.clone(), x.clone()),
            "Match in the other direction"
        );
    }

    #[test]
    fn identification_id() {
        let ctx = Context::new();
        let cat = cat!(ctx, (:0));
        let x = obj!(ctx, (:1) in cat);
        let y = obj!(ctx, (:2) in cat);

        let gr = Graph {
            nodes: vec![(x, NodeLabel::new()), (y, NodeLabel::new())],
            edges: vec![vec![], vec![]],
            faces: vec![],
        };
        let mut vm = VM::<Mock, ()>::new(ctx, gr, Vec::new(), Vec::new());

        let annot = |v: Id| -> Annot<Id> {
            Annot {
                value: v,
                range: std::ops::Range { start: 0, end: 1 },
            }
        };
        let id0 = vm.identify_node(&TermDescr::Ref(annot(Id::Id(0)))).unwrap();
        assert_eq!(id0, 0, "Found node by id 0");
        let id1 = vm.identify_node(&TermDescr::Ref(annot(Id::Id(1)))).unwrap();
        assert_eq!(id1, 1, "Found node by id 0");
        let id2 = vm.identify_node(&TermDescr::Ref(annot(Id::Id(2))));
        assert!(id2.is_none(), "Succeeded in finding inexisting node");
    }
}
