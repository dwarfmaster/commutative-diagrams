use super::Remote;
use crate::data::{EvarStatus, Feature, Tag};
use crate::graph::{FaceParsed, GraphParsed};

struct Term {
    label: String,
    name: Option<String>,
    status: EvarStatus,
    feats: Vec<Feature>,
}

pub struct Mock {
    terms: Vec<Term>,
    goal: GraphParsed<(), (), ()>,
}

impl Mock {
    pub fn new() -> Self {
        Mock {
            terms: Vec::new(),
            goal: GraphParsed::new(),
        }
    }

    pub fn set_graph(&mut self, gr: GraphParsed<(), (), ()>) {
        self.goal = gr;
    }

    pub fn new_term(&mut self, label: String, name: Option<String>, status: EvarStatus) -> u64 {
        let tm = Term {
            label,
            name,
            status,
            feats: Vec::new(),
        };
        let id = self.terms.len();
        self.terms.push(tm);
        id as u64
    }

    pub fn add_feat(&mut self, term: u64, feat: Feature) {
        self.terms[term as usize].feats.push(feat)
    }
}

impl Remote for Mock {
    type Error = ();
    fn goal<NL, EL, FL>(&mut self) -> Result<GraphParsed<NL, EL, FL>, ()>
    where
        NL: Default,
        EL: Default,
        FL: Default,
    {
        Ok(GraphParsed {
            nodes: self
                .goal
                .nodes
                .iter()
                .map(|(nd, cat, ())| (*nd, *cat, Default::default()))
                .collect(),
            edges: self
                .goal
                .edges
                .iter()
                .map(|v| {
                    v.iter()
                        .map(|(dst, (), m)| (*dst, Default::default(), m.clone()))
                        .collect()
                })
                .collect(),
            faces: self
                .goal
                .faces
                .iter()
                .map(|f| FaceParsed {
                    start: f.start,
                    end: f.end,
                    left: f.left.clone(),
                    right: f.right.clone(),
                    eq: f.eq.clone(),
                    label: Default::default(),
                })
                .collect(),
        })
    }

    fn info(&mut self, obj: u64) -> Result<(String, Option<String>, EvarStatus), ()> {
        let id = obj as usize;
        Ok((
            self.terms[id].label.clone(),
            self.terms[id].name.clone(),
            self.terms[id].status,
        ))
    }

    fn unify<I>(&mut self, mut pairs: I) -> Result<bool, Self::Error>
    where
        I: Iterator<Item = (u64, u64)> + ExactSizeIterator + Clone,
    {
        Ok(pairs.all(|(x, y)| x == y))
    }

    fn equalify(&mut self, obj1: u64, obj2: u64) -> Result<bool, ()> {
        Ok(obj1 == obj2)
    }

    fn lemmas(&mut self) -> Result<Vec<(u64, String, String)>, ()> {
        Ok(Vec::new())
    }

    fn instantiate<NL, EL, FL>(&mut self, _lem: u64) -> Result<GraphParsed<NL, EL, FL>, ()>
    where
        NL: Default,
        EL: Default,
        FL: Default,
    {
        Err(())
    }

    fn query(&mut self, obj: u64, tag: Tag) -> Result<Vec<Feature>, ()> {
        Ok(self.terms[obj as usize]
            .feats
            .iter()
            .filter(|f| f.tag() == tag)
            .cloned()
            .collect())
    }

    fn build(&mut self, feat: Feature) -> Result<u64, ()> {
        let tm = self.new_term(format!(":{}", self.terms.len()), None, EvarStatus::Grounded);
        self.add_feat(tm, feat);
        Ok(tm)
    }

    fn parse(&mut self, _str: String) -> Result<Result<u64, String>, ()> {
        Ok(Err("Cannot mock parsing".to_string()))
    }

    fn save_state(&mut self) -> Result<u64, ()> {
        Ok(0)
    }

    fn restore_state(&mut self, _state: u64) -> Result<(), ()> {
        Ok(())
    }

    fn finish(&mut self, _success: bool) -> Result<(), ()> {
        Ok(())
    }
}
