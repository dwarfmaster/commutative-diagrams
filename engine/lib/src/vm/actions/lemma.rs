use crate::graph::GraphId;
use crate::normalizer::ensure_graph_invariant;
use crate::remote::Remote;
use crate::vm::{Graph, Interactive, VM};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnifyPair {
    Nodes(usize,usize),
    Morphisms((usize,usize),(usize,usize)),
    PathVM((usize,usize), usize, Vec::<usize>),
    PathLemma(usize, Vec::<usize>, (usize,usize)),
    Faces(usize, usize),
}

impl UnifyPair {
    pub fn goal_id(&self) -> Option<GraphId> {
        use UnifyPair::*;
        use GraphId::*;
        match self {
            Nodes(_,nd) => Some(Node(*nd)),
            Morphisms(_,(s,m)) => Some(Morphism(*s, *m)),
            Faces(_,f) => Some(Face(*f)),
            _ => None,
        }
    }
}

type Mapping = HashMap<GraphId, Vec<UnifyPair>>;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    // Returns true on success and false on failure
    pub fn apply_lemma(&mut self, lemma: usize, matching: &[(GraphId, GraphId)]) -> bool {
        let mut pattern = self.lemmas.lemmas[lemma].instantiate(&mut self.ctx, &self.config, true);
        let matchings = match self.lemma_complete_matchings(&pattern, matching) {
            Some(matching) => matching,
            None => {
                self.code.error_msg = "Couldn't complete matching".to_string();
                return false;
            }
        };

        let r = self.lemma_unify_matching(&mut pattern, &matchings);
        if let Some(errmsg) = r {
            self.code.error_msg = errmsg;
            return false;
        }

        let mut direct = HashMap::new();
        let mut reverse = HashMap::new();
        Self::lemma_extend_hash_matching(&matchings, &mut direct, &mut reverse, &pattern, &self.graph.graph);

        self.pushout(&pattern, &direct);
        true
    }

    // Complete a partial matching (ie if morphisms are matched, match the source and
    // destination...)
    pub fn lemma_complete_matchings(
        &mut self,
        pattern: &Graph,
        matching: &[(GraphId, GraphId)],
    ) -> Option<Vec<UnifyPair>> {
        let mut matchings = Vec::new();
        for (lem, goal) in matching {
            if !self.lemma_complete_matching(&pattern, *lem, *goal, &mut matchings) {
                return None;
            }
        }
        Some(matchings)
    }

    pub fn lemma_extend_hash_matching(
        matching: &[UnifyPair],
        direct: &mut Mapping,
        reverse: &mut Mapping,
        pattern: &Graph,
        goal: &Graph,
    ) {
        for pair in matching {
            use UnifyPair :: *;
            match pair {
                Nodes(n1,n2) => Self::lemma_match_connect(direct, reverse, GraphId::Node(*n1), GraphId::Node(*n2), pair),
                Morphisms((s1,m1),(s2,m2)) => Self::lemma_match_connect(direct, reverse, GraphId::Morphism(*s1,*m1), GraphId::Morphism(*s2,*m2), pair),
                Faces(f1, f2) => Self::lemma_match_connect(direct, reverse, GraphId::Face(*f1), GraphId::Face(*f2), pair),
                PathVM((s1,m1), s2, mphs) => {
                    Self::lemma_connect(direct, GraphId::Morphism(*s1, *m1), pair);
                    let mut s = *s2;
                    for m in mphs {
                        Self::goal_connect(reverse, GraphId::Morphism(s, *m), pair);
                        s = goal.edges[s][*m].0;
                    }
                }
                PathLemma(s1, mphs, (s2,m2)) => {
                    Self::goal_connect(reverse, GraphId::Morphism(*s2, *m2), pair);
                    let mut s = *s1;
                    for m in mphs {
                        Self::lemma_connect(direct, GraphId::Morphism(s, *m), pair);
                        s = pattern.edges[s][*m].0;
                    }
                }
            }
        }
    }

    // Returns None on success or an error message on failure
    pub fn lemma_unify_matching(
        &mut self,
        pattern: &mut Graph,
        matching: &[UnifyPair],
    ) -> Option<String> {
        // Unify nodes and morphisms
        let get_value = |glem: &Graph, gvm: &Graph, pair: &UnifyPair| -> Option<(u64,u64)> {
            use UnifyPair::*;
            match pair {
                Nodes(n1, n2) => Some((glem.nodes[*n1].0, gvm.nodes[*n2].0)),
                Morphisms((s1,m1), (s2,m2)) => Some((glem.edges[*s1][*m1].2, gvm.edges[*s2][*m2].2)),
                PathVM(_, _, _) => None,
                PathLemma(_, _, _) => None,
                Faces(_, _) => None,
            }
        };
        let to_unify = matching
            .iter()
            .filter_map(|pair| get_value(&pattern, &self.graph.graph, pair))
            .collect::<Vec<_>>();
        let success = self.ctx.remote.unify(to_unify.into_iter()).unwrap();
        if !success {
            return Some("Unification failed".to_string());
        }

        // Unify equalities
        self.ctx.save_state(); // Necessary to clear cached representations
        let eqs = matching
            .iter()
            .filter_map(|pair| match pair {
                UnifyPair::Faces(f1, f2) => Some((
                    pattern.faces[*f1].eq.clone(),
                    self.graph.graph.faces[*f2].eq.clone(),
                )),
                _ => None,
            })
            .collect::<Vec<_>>();
        for (eq1, eq2) in eqs {
            if !self.unify_eq_unsafe(eq1.cat, &eq1, &eq2, false) {
                return Some("Unification of equalities failed".to_string());
            }
        }

        // Normalize all morphisms
        self.ensure_morphisms_invariant();
        ensure_graph_invariant(&mut self.ctx, pattern);

        None
    }

    pub fn find_lemma(&self, name: &str) -> Option<usize> {
        for lem in 0..self.lemmas.lemmas.len() {
            let parts = name.split('.');
            if parts.eq(self.lemmas.lemmas[lem]
                .namespace
                .iter()
                .chain(std::iter::once(&self.lemmas.lemmas[lem].name)))
            {
                return Some(lem);
            }
        }
        return None;
    }

    // Returns false if the matched object are not of the same nature
    pub fn lemma_complete_matching(
        &mut self,
        pattern: &Graph,
        lem: GraphId,
        goal: GraphId,
        matching: &mut Vec<UnifyPair>,
    ) -> bool {
        use GraphId::*;
        match (lem, goal) {
            (Node(lnd), Node(gnd)) => {
                matching.push(UnifyPair::Nodes(lnd, gnd));
            }
            (Morphism(lsrc, lmph), Morphism(gsrc, gmph)) => {
                matching.push(UnifyPair::Morphisms((lsrc,lmph), (gsrc,gmph)));
                matching.push(UnifyPair::Nodes(lsrc, gsrc));
                matching.push(UnifyPair::Nodes(pattern.edges[lsrc][lmph].0,self.graph.graph.edges[gsrc][gmph].0));
            }
            (Face(lfce), Face(gfce)) => {
                matching.push(UnifyPair::Faces(lfce, gfce));
                matching.push(UnifyPair::Nodes(pattern.faces[lfce].end,self.graph.graph.faces[gfce].end));

                // Connect left side
                let mut lsrc = pattern.faces[lfce].start;
                let mut gsrc = self.graph.graph.faces[gfce].start;
                if pattern.faces[lfce].left.len() == self.graph.graph.faces[gfce].left.len() {
                    for nxt in 0..pattern.faces[lfce]
                        .left
                        .len()
                        .min(self.graph.graph.faces[gfce].left.len())
                    {
                        matching.push(UnifyPair::Nodes(lsrc,gsrc));
                        let lmph = pattern.faces[lfce].left[nxt];
                        let gmph = self.graph.graph.faces[gfce].left[nxt];
                        matching.push(UnifyPair::Morphisms((lsrc,lmph), (gsrc,gmph)));
                        lsrc = pattern.edges[lsrc][lmph].0;
                        gsrc = self.graph.graph.edges[gsrc][gmph].0;
                    }
                } else if pattern.faces[lfce].left.len() == 1 {
                    let mph_gr = self.graph.graph.faces[gfce].left.clone();
                    matching.push(UnifyPair::PathVM((lsrc, pattern.faces[lfce].left[0]), gsrc, mph_gr));
                } else if self.graph.graph.faces[gfce].left.len() == 1 {
                    let mph_lem = pattern.faces[lfce].left.clone();
                    matching.push(UnifyPair::PathLemma(lsrc, mph_lem, (gsrc, self.graph.graph.faces[gfce].left[0])));
                }

                // Connect right side
                let mut lsrc = pattern.faces[lfce].start;
                let mut gsrc = self.graph.graph.faces[gfce].start;
                if pattern.faces[lfce].right.len() == self.graph.graph.faces[gfce].right.len() {
                    for nxt in 0..pattern.faces[lfce]
                        .right
                        .len()
                        .min(self.graph.graph.faces[gfce].right.len())
                    {
                        matching.push(UnifyPair::Nodes(lsrc,gsrc));
                        let lmph = pattern.faces[lfce].right[nxt];
                        let gmph = self.graph.graph.faces[gfce].right[nxt];
                        matching.push(UnifyPair::Morphisms((lsrc,lmph), (gsrc,gmph)));
                        lsrc = pattern.edges[lsrc][lmph].0;
                        gsrc = self.graph.graph.edges[gsrc][gmph].0;
                    }
                } else if pattern.faces[lfce].right.len() == 1 {
                    let mph_gr = self.graph.graph.faces[gfce].right.clone();
                    matching.push(UnifyPair::PathVM((lsrc, pattern.faces[lfce].right[0]), gsrc, mph_gr));
                } else if self.graph.graph.faces[gfce].right.len() == 1 {
                    let mph_lem = pattern.faces[lfce].right.clone();
                    matching.push(UnifyPair::PathLemma(lsrc, mph_lem, (gsrc, self.graph.graph.faces[gfce].right[0])));
                }
            }
            _ => return false,
        }
        true
    }

    fn lemma_connect(
        direct: &mut Mapping,
        lem: GraphId,
        pair: &UnifyPair
    ) {
        if !direct.get(&lem).map(|v| v.contains(&pair)).unwrap_or(false) {
            direct.entry(lem).or_default().push(pair.clone());
        }
    }

    fn goal_connect(
        reverse: &mut Mapping,
        goal: GraphId,
        pair: &UnifyPair
    ) {
        if !reverse.get(&goal).map(|v| v.contains(&pair)).unwrap_or(false) {
            reverse.entry(goal).or_default().push(pair.clone());
        }
    }

    fn lemma_match_connect(
        direct: &mut Mapping,
        reverse: &mut Mapping,
        lem: GraphId,
        goal: GraphId,
        pair: &UnifyPair,
    ) {
        Self::lemma_connect(direct, lem, pair);
        Self::goal_connect(reverse, goal, pair);
    }
}
