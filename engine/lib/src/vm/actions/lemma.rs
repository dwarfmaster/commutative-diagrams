use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::{Graph, Interactive, VM};
use std::collections::HashMap;

type Mapping = HashMap<GraphId, Vec<GraphId>>;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    // Returns true on success and false on failure
    pub fn apply_lemma(&mut self, lemma: usize, matching: &[(GraphId, GraphId)]) -> bool {
        let pattern = self.lemmas[lemma].instantiate(&mut self.ctx, &self.config, true);
        let matchings = match self.lemma_complete_matchings(&pattern, matching) {
            Some(matching) => matching,
            None => {
                self.error_msg = "Couldn't complete matching".to_string();
                return false;
            }
        };

        let mut direct = HashMap::new();
        let mut reverse = HashMap::new();
        Self::lemma_extend_hash_matching(&matchings, &mut direct, &mut reverse);

        let r = self.lemma_unify_matching(&pattern, &matchings);
        if let Some(errmsg) = r {
            self.error_msg = errmsg;
            return false;
        }

        self.pushout(&pattern, &direct);
        true
    }

    // Complete a partial matching (ie if morphisms are matched, match the source and
    // destination...)
    pub fn lemma_complete_matchings(
        &self,
        pattern: &Graph,
        matching: &[(GraphId, GraphId)],
    ) -> Option<Vec<(GraphId, GraphId)>> {
        let mut matchings = Vec::new();
        for (lem, goal) in matching {
            if !self.lemma_complete_matching(&pattern, *lem, *goal, &mut matchings) {
                return None;
            }
        }
        Some(matchings)
    }

    pub fn lemma_extend_hash_matching(
        matching: &[(GraphId, GraphId)],
        direct: &mut Mapping,
        reverse: &mut Mapping,
    ) {
        for (lem, goal) in matching {
            Self::lemma_match_connect(direct, reverse, *lem, *goal);
        }
    }

    // Returns None on success or an error message on failure
    pub fn lemma_unify_matching(
        &mut self,
        pattern: &Graph,
        matching: &[(GraphId, GraphId)],
    ) -> Option<String> {
        use GraphId::*;

        // Unify nodes and morphisms
        let get_value = |graph: &Graph, id: GraphId| -> Option<u64> {
            match id {
                Node(n) => Some(graph.nodes[n].0),
                Morphism(s, m) => Some(graph.edges[s][m].2),
                Face(_) => None,
            }
        };
        let to_unify = matching
            .iter()
            .filter_map(|(id1, id2)| {
                match (get_value(&pattern, *id1), get_value(&self.graph, *id2)) {
                    (Some(n1), Some(n2)) => Some((n1, n2)),
                    _ => None,
                }
            })
            .collect::<Vec<_>>();
        let success = self.ctx.remote.unify(to_unify.into_iter()).unwrap();
        if !success {
            return Some("Unification failed".to_string());
        }

        // Unify equalities
        self.ctx.save_state(); // Necessary to clear cached representations
        let eqs = matching
            .iter()
            .filter_map(|(id1, id2)| match (id1, id2) {
                (Face(f1), Face(f2)) => Some((
                    pattern.faces[*f1].eq.clone(),
                    self.graph.faces[*f2].eq.clone(),
                )),
                _ => None,
            })
            .collect::<Vec<_>>();
        for (eq1, eq2) in eqs {
            if !self.unify_eq(eq1.cat, &eq1, &eq2) {
                return Some("Unification of equalities failed".to_string());
            }
        }

        None
    }

    pub fn find_lemma(&self, name: &str) -> Option<usize> {
        for lem in 0..self.lemmas.len() {
            let parts = name.split('.');
            if parts.eq(self.lemmas[lem]
                .namespace
                .iter()
                .chain(std::iter::once(&self.lemmas[lem].name)))
            {
                return Some(lem);
            }
        }
        return None;
    }

    // Returns false if the matched object are not of the same nature
    pub fn lemma_complete_matching(
        &self,
        pattern: &Graph,
        lem: GraphId,
        goal: GraphId,
        matching: &mut Vec<(GraphId, GraphId)>,
    ) -> bool {
        use GraphId::*;
        match (lem, goal) {
            (Node(lnd), Node(gnd)) => {
                matching.push((Node(lnd), Node(gnd)));
            }
            (Morphism(lsrc, lmph), Morphism(gsrc, gmph)) => {
                matching.push((Morphism(lsrc, lmph), Morphism(gsrc, gmph)));
                matching.push((Node(lsrc), Node(gsrc)));
                matching.push((
                    Node(pattern.edges[lsrc][lmph].0),
                    Node(self.graph.edges[gsrc][gmph].0),
                ));
            }
            (Face(lfce), Face(gfce)) => {
                matching.push((Face(lfce), Face(gfce)));
                matching.push((
                    Node(pattern.faces[lfce].end),
                    Node(self.graph.faces[gfce].end),
                ));

                // Connect left side
                let mut lsrc = pattern.faces[lfce].start;
                let mut gsrc = self.graph.faces[gfce].start;
                for nxt in 0..pattern.faces[lfce]
                    .left
                    .len()
                    .min(self.graph.faces[gfce].left.len())
                {
                    matching.push((Node(lsrc), Node(gsrc)));
                    let lmph = pattern.faces[lfce].left[nxt];
                    let gmph = self.graph.faces[gfce].left[nxt];
                    matching.push((Morphism(lsrc, lmph), Morphism(gsrc, gmph)));
                    lsrc = pattern.edges[lsrc][lmph].0;
                    gsrc = self.graph.edges[gsrc][gmph].0;
                }

                // Connect right side
                let mut lsrc = pattern.faces[lfce].start;
                let mut gsrc = self.graph.faces[gfce].start;
                for nxt in 0..pattern.faces[lfce]
                    .right
                    .len()
                    .min(self.graph.faces[gfce].right.len())
                {
                    matching.push((Node(lsrc), Node(gsrc)));
                    let lmph = pattern.faces[lfce].right[nxt];
                    let gmph = self.graph.faces[gfce].right[nxt];
                    matching.push((Morphism(lsrc, lmph), Morphism(gsrc, gmph)));
                    lsrc = pattern.edges[lsrc][lmph].0;
                    gsrc = self.graph.edges[gsrc][gmph].0;
                }
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
}
