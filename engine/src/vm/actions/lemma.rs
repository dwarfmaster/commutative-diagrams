use crate::anyterm::IsTerm;
use crate::data::ActualProofObject;
use crate::graph::GraphId;
use crate::substitution::SubstitutableInPlace;
use crate::unification::UnifState;
use crate::vm::{Graph, Interactive, VM};
use std::collections::HashMap;

type Mapping = HashMap<GraphId, Vec<GraphId>>;

impl<I: Interactive + Sync + Send> VM<I> {
    // Returns true on success and false on failure
    pub fn apply_lemma(&mut self, lemma: usize, matching: &[(GraphId, GraphId)]) -> bool {
        let mut pattern = self.prepare_lemma_graph(lemma);
        let mut direct = HashMap::new();
        let mut reverse = HashMap::new();
        let mut unif = UnifState::new();
        for (lem, goal) in matching {
            if !self.lemma_add_matching(&pattern, *lem, *goal, &mut unif) {
                return false;
            }
            self.lemma_connect_sides(&pattern, *lem, *goal, &mut direct, &mut reverse);
        }

        if let Some(sigma) = unif.solve() {
            pattern.subst_in_place(&self.ctx, &sigma);
            self.refine(sigma);
            self.pushout(&pattern, &direct);
            true
        } else {
            false
        }
    }

    pub fn find_lemma(&self, name: &str) -> Option<usize> {
        for lem in 0..self.lemmas.len() {
            if self.lemmas[lem].name == name {
                return Some(lem);
            }
        }
        return None;
    }

    // Allocate fresh evars for the lemma. Does not relabel the graph !
    pub fn prepare_lemma_graph(&self, lemma: usize) -> Graph {
        let mut sigma = Vec::new();
        for ex in &self.lemmas[lemma].existentials {
            let nex = self.ctx.new_existential();
            let term = self.ctx.mk(ActualProofObject::Existential(nex)).term();
            sigma.push((*ex, term));
        }
        let mut gr = self.lemmas[lemma].pattern.clone();
        gr.subst_in_place(&self.ctx, &sigma);
        gr
    }

    // Returns false if the matched object are not of the same nature
    pub fn lemma_add_matching(
        &self,
        pattern: &Graph,
        lem: GraphId,
        goal: GraphId,
        unif: &mut UnifState,
    ) -> bool {
        use GraphId::*;
        match (lem, goal) {
            (Node(lnd), Node(gnd)) => {
                let lelem = pattern.nodes[lnd].0.clone();
                let gelem = self.graph.nodes[gnd].0.clone();
                unif.add(&self.ctx, lelem.clone().term());
                unif.add(&self.ctx, gelem.clone().term());
                unif.add_goal(lelem.term(), gelem.term());
            }
            (Morphism(lsrc, lmph), Morphism(gsrc, gmph)) => {
                let lmorph = pattern.edges[lsrc][lmph].2.clone();
                let gmorph = self.graph.edges[gsrc][gmph].2.clone();
                unif.add(&self.ctx, lmorph.clone().term());
                unif.add(&self.ctx, gmorph.clone().term());
                unif.add_goal(lmorph.term(), gmorph.term());
            }
            (Face(lfce), Face(gfce)) => {
                let leq = pattern.faces[lfce].eq.clone();
                let geq = self.graph.faces[gfce].eq.clone();
                unif.add(&self.ctx, leq.clone().term());
                unif.add(&self.ctx, geq.clone().term());
                unif.add_goal(leq.term(), geq.term());
            }
            _ => return false,
        }
        true
    }

    fn lemma_match_connect(
        direct: &mut Mapping,
        reverse: &mut Mapping,
        lem: GraphId,
        goal: GraphId,
    ) {
        if direct.get(&lem).map(|v| v.contains(&goal)).unwrap_or(false) {
            return;
        }
        direct.entry(lem).or_default().push(goal);
        reverse.entry(goal).or_default().push(lem);
    }

    pub fn lemma_connect_sides(
        &self,
        pattern: &Graph,
        lem: GraphId,
        goal: GraphId,
        direct: &mut Mapping,
        reverse: &mut Mapping,
    ) {
        use GraphId::*;
        Self::lemma_match_connect(direct, reverse, lem, goal);
        match (lem, goal) {
            (Node(_), Node(_)) => (),
            (Morphism(lsrc, lmph), Morphism(gsrc, gmph)) => {
                Self::lemma_match_connect(direct, reverse, Node(lsrc), Node(gsrc));
                let ldst = pattern.edges[lsrc][lmph].0;
                let gdst = self.graph.edges[gsrc][gmph].0;
                Self::lemma_match_connect(direct, reverse, Node(ldst), Node(gdst));
            }
            (Face(lfce), Face(gfce)) => {
                let gface = &self.graph.faces[gfce];
                // Connect source and destination
                Self::lemma_match_connect(
                    direct,
                    reverse,
                    Node(pattern.faces[lfce].start),
                    Node(gface.start),
                );
                Self::lemma_match_connect(
                    direct,
                    reverse,
                    Node(pattern.faces[lfce].end),
                    Node(gface.end),
                );
                // Connect left side
                let mut lsrc = pattern.faces[lfce].start;
                let mut gsrc = gface.start;
                for nxt in 0..pattern.faces[lfce].left.len().min(gface.left.len()) {
                    Self::lemma_match_connect(direct, reverse, Node(lsrc), Node(gsrc));
                    let lmph = pattern.faces[lfce].left[nxt];
                    let gmph = gface.left[nxt];
                    Self::lemma_match_connect(
                        direct,
                        reverse,
                        Morphism(lsrc, lmph),
                        Morphism(gsrc, gmph),
                    );
                    lsrc = pattern.edges[lsrc][lmph].0;
                    gsrc = self.graph.edges[gsrc][gmph].0;
                }
                // Connect right side
                let mut lsrc = pattern.faces[lfce].start;
                let mut gsrc = gface.start;
                for nxt in 0..pattern.faces[lfce].right.len().min(gface.right.len()) {
                    Self::lemma_match_connect(direct, reverse, Node(lsrc), Node(gsrc));
                    let lmph = pattern.faces[lfce].right[nxt];
                    let gmph = gface.right[nxt];
                    Self::lemma_match_connect(
                        direct,
                        reverse,
                        Morphism(lsrc, lmph),
                        Morphism(gsrc, gmph),
                    );
                    lsrc = pattern.edges[lsrc][lmph].0;
                    gsrc = self.graph.edges[gsrc][gmph].0;
                }
            }
            _ => unreachable!(),
        }
    }
}
