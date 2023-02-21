use crate::data::{ActualCategory, ActualEquality, ActualFunctor, ActualMorphism, ActualObject};
use crate::data::{Category, Equality, Functor, Morphism, Object};
use crate::data::{CategoryData, EqualityData, FunctorData, MorphismData, ObjectData, ProofObject};
use crate::vm::ast::{Id, TermDescr};
use crate::vm::vm::GraphId;
use crate::vm::VM;
use std::collections::HashSet;

impl VM {
    fn realize_descr_as_cat(
        &mut self,
        exs: &mut HashSet<u64>,
        descr: &TermDescr,
    ) -> Option<Category> {
        use ActualCategory::*;
        use TermDescr::*;
        match descr {
            Ref(_) => None, // Only nodes, edges and faces can have names
            Hole => {
                let ex = self.ctx.new_existential();
                exs.insert(ex);
                Some(self.ctx.mk(Atomic(CategoryData {
                    pobj: ProofObject::Existential(ex),
                })))
            }
        }
    }

    pub fn descr_as_cat(&mut self, descr: &TermDescr) -> Option<(Category, HashSet<u64>)> {
        let mut hash = HashSet::new();
        let cat = self.realize_descr_as_cat(&mut hash, descr)?;
        Some((cat, hash))
    }

    fn realize_descr_as_functor(
        &mut self,
        exs: &mut HashSet<u64>,
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
                exs.insert(ex);
                Some(self.ctx.mk(Atomic(FunctorData {
                    pobj: ProofObject::Existential(ex),
                    src,
                    dst,
                })))
            }
        }
    }

    pub fn descr_as_functor(&mut self, descr: &TermDescr) -> Option<(Functor, HashSet<u64>)> {
        let mut hash = HashSet::new();
        let ex = self.ctx.new_existential();
        hash.insert(ex);
        let src = self.ctx.mk(ActualCategory::Atomic(CategoryData {
            pobj: ProofObject::Existential(ex),
        }));
        let ex = self.ctx.new_existential();
        hash.insert(ex);
        let dst = self.ctx.mk(ActualCategory::Atomic(CategoryData {
            pobj: ProofObject::Existential(ex),
        }));
        let funct = self.realize_descr_as_functor(&mut hash, descr, src, dst)?;
        Some((funct, hash))
    }

    fn realize_descr_as_object(
        &mut self,
        exs: &mut HashSet<u64>,
        descr: &TermDescr,
        cat: Category,
    ) -> Option<Object> {
        use ActualObject::*;
        use TermDescr::*;
        match descr {
            Ref(Id::Id(id)) => {
                if *id < self.graph.nodes.len() {
                    Some(self.graph.nodes[*id].0.clone())
                } else {
                    None
                }
            }
            Ref(Id::Name(name)) => {
                if let Some(GraphId::Node(id)) = self.names.get(name) {
                    Some(self.graph.nodes[*id].0.clone())
                } else {
                    None
                }
            }
            Hole => {
                let ex = self.ctx.new_existential();
                exs.insert(ex);
                Some(self.ctx.mk(Atomic(ObjectData {
                    pobj: ProofObject::Existential(ex),
                    category: cat,
                })))
            }
        }
    }

    pub fn descr_as_object(&mut self, descr: &TermDescr) -> Option<(Object, HashSet<u64>)> {
        let mut hash = HashSet::new();
        let ex = self.ctx.new_existential();
        hash.insert(ex);
        let cat = self.ctx.mk(ActualCategory::Atomic(CategoryData {
            pobj: ProofObject::Existential(ex),
        }));
        let obj = self.realize_descr_as_object(&mut hash, descr, cat)?;
        Some((obj, hash))
    }

    fn realize_descr_as_morphism(
        &mut self,
        exs: &mut HashSet<u64>,
        descr: &TermDescr,
        src: Object,
        dst: Object,
    ) -> Option<Morphism> {
        use ActualMorphism::*;
        use TermDescr::*;
        let category = src.cat(&self.ctx);
        match descr {
            Ref(Id::Id(id)) => {
                let (src, mph) = self.graph.edge_by_id(*id)?;
                Some(self.graph.edges[src][mph].2.clone())
            }
            Ref(Id::Name(name)) => {
                if let Some(GraphId::Morphism(src, mph)) = self.names.get(name) {
                    Some(self.graph.edges[*src][*mph].2.clone())
                } else {
                    None
                }
            }
            Hole => {
                let ex = self.ctx.new_existential();
                exs.insert(ex);
                Some(self.ctx.mk(Atomic(MorphismData {
                    pobj: ProofObject::Existential(ex),
                    src,
                    dst,
                    category,
                })))
            }
        }
    }

    pub fn descr_as_morphism(&mut self, descr: &TermDescr) -> Option<(Morphism, HashSet<u64>)> {
        let mut hash = HashSet::new();
        let ex = self.ctx.new_existential();
        hash.insert(ex);
        let cat = self.ctx.mk(ActualCategory::Atomic(CategoryData {
            pobj: ProofObject::Existential(ex),
        }));
        let ex = self.ctx.new_existential();
        hash.insert(ex);
        let src = self.ctx.mk(ActualObject::Atomic(ObjectData {
            pobj: ProofObject::Existential(ex),
            category: cat.clone(),
        }));
        let ex = self.ctx.new_existential();
        hash.insert(ex);
        let dst = self.ctx.mk(ActualObject::Atomic(ObjectData {
            pobj: ProofObject::Existential(ex),
            category: cat,
        }));
        let mph = self.realize_descr_as_morphism(&mut hash, descr, src, dst)?;
        Some((mph, hash))
    }

    fn realize_descr_as_equality(
        &mut self,
        exs: &mut HashSet<u64>,
        descr: &TermDescr,
        left: Morphism,
        right: Morphism,
    ) -> Option<Equality> {
        use ActualEquality::*;
        use TermDescr::*;
        let category = left.cat(&self.ctx);
        let src = left.src(&self.ctx);
        let dst = right.dst(&self.ctx);
        match descr {
            Ref(Id::Id(id)) => {
                if *id < self.graph.faces.len() {
                    Some(self.graph.faces[*id].eq.clone())
                } else {
                    None
                }
            }
            Ref(Id::Name(name)) => {
                if let Some(GraphId::Face(id)) = self.names.get(name) {
                    Some(self.graph.faces[*id].eq.clone())
                } else {
                    None
                }
            }
            Hole => {
                let ex = self.ctx.new_existential();
                exs.insert(ex);
                Some(self.ctx.mk(Atomic(EqualityData {
                    pobj: ProofObject::Existential(ex),
                    src,
                    dst,
                    category,
                    left,
                    right,
                })))
            }
        }
    }

    pub fn descr_as_equality(&mut self, descr: &TermDescr) -> Option<(Equality, HashSet<u64>)> {
        let mut hash = HashSet::new();
        let ex = self.ctx.new_existential();
        hash.insert(ex);
        let cat = self.ctx.mk(ActualCategory::Atomic(CategoryData {
            pobj: ProofObject::Existential(ex),
        }));
        let ex = self.ctx.new_existential();
        hash.insert(ex);
        let src = self.ctx.mk(ActualObject::Atomic(ObjectData {
            pobj: ProofObject::Existential(ex),
            category: cat.clone(),
        }));
        let ex = self.ctx.new_existential();
        hash.insert(ex);
        let dst = self.ctx.mk(ActualObject::Atomic(ObjectData {
            pobj: ProofObject::Existential(ex),
            category: cat.clone(),
        }));
        let ex = self.ctx.new_existential();
        hash.insert(ex);
        let left = self.ctx.mk(ActualMorphism::Atomic(MorphismData {
            pobj: ProofObject::Existential(ex),
            category: cat.clone(),
            src: src.clone(),
            dst: dst.clone(),
        }));
        let ex = self.ctx.new_existential();
        hash.insert(ex);
        let right = self.ctx.mk(ActualMorphism::Atomic(MorphismData {
            pobj: ProofObject::Existential(ex),
            category: cat,
            src,
            dst,
        }));
        let eq = self.realize_descr_as_equality(&mut hash, descr, left, right)?;
        Some((eq, hash))
    }
}
