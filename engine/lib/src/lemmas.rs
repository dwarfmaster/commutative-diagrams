use crate::graph::Graph;

pub struct Lemma<GR, NL, EL, FL> {
    pub id: u64,
    pub name: String,
    pub namespace: Vec<String>,
    pub complete_name: String,
    pub pattern: Option<Graph<NL, EL, FL>>,
    pub graphical_state: GR,
}

impl<GR: Default, NL, EL, FL> Lemma<GR, NL, EL, FL> {
    pub fn new(id: u64, name: String, namespace: Vec<String>) -> Self {
        let complete = itertools::Itertools::intersperse(
            namespace
                .iter()
                .map(|s| s.as_str())
                .chain(std::iter::once(name.as_str())),
            ">",
        )
        .collect();
        Self {
            id,
            name,
            namespace,
            complete_name: complete,
            pattern: None,
            graphical_state: Default::default(),
        }
    }
}
