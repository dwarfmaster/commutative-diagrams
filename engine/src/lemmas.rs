use crate::graph::Graph;

pub struct Lemma<GR, NL, EL, FL> {
    pub id: u64,
    pub name: String,
    pub namespace: Vec<String>,
    pub pattern: Option<Graph<NL, EL, FL>>,
    pub graphical_state: GR,
}

impl<GR: Default, NL, EL, FL> Lemma<GR, NL, EL, FL> {
    pub fn new(id: u64, name: String, namespace: Vec<String>) -> Self {
        Self {
            id,
            name,
            namespace,
            pattern: None,
            graphical_state: Default::default(),
        }
    }
}
