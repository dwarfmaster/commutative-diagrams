use super::Lemma;

#[derive(Debug)]
pub enum LemmaTree {
    Node(String, Vec<Box<LemmaTree>>),
    Leaf(usize),
}

impl LemmaTree {
    pub fn new(lemmas: &[Lemma]) -> Vec<Box<LemmaTree>> {
        let mut tree = Vec::new();
        for lem in 0..lemmas.len() {
            Self::insert_into(&mut tree, lem, 0, lemmas)
        }
        tree
    }

    fn insert_into(tree: &mut Vec<Box<LemmaTree>>, lem: usize, depth: usize, lemmas: &[Lemma]) {
        use LemmaTree::*;
        let rec: bool = depth < lemmas[lem].namespace.len();
        let key: &str = if rec {
            &lemmas[lem].namespace[depth]
        } else {
            &lemmas[lem].name
        };
        let insert_point = tree.binary_search_by_key(&(rec, key), |node| match node.as_ref() {
            Node(name, _) => (true, name),
            Leaf(lem) => (false, &lemmas[*lem].name),
        });

        match (insert_point, rec) {
            (Ok(id), true) => match tree[id].as_mut() {
                Node(_, sub) => Self::insert_into(sub, lem, depth + 1, lemmas),
                _ => unreachable!(),
            },
            (Ok(id), false) => match tree[id].as_ref() {
                Leaf(olem) => assert_eq!(*olem, lem),
                _ => unreachable!(),
            },
            (Err(id), true) => {
                let mut sub = Vec::new();
                Self::insert_into(&mut sub, lem, depth + 1, lemmas);
                let node = Node(lemmas[lem].namespace[depth].clone(), sub);
                tree.insert(id, Box::new(node));
            }
            (Err(id), false) => {
                tree.insert(id, Box::new(Leaf(lem)));
            }
        }
    }
}
