use core::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annot<T> {
    pub value: T,
    pub range: Range<usize>,
}

impl<T> Annot<T> {
    pub fn map<U, F>(self, f: F) -> Annot<U>
    where
        F: FnOnce(T) -> U,
    {
        Annot {
            value: f(self.value),
            range: self.range,
        }
    }
}

pub type AST = Vec<Annot<Action>>;

// An intermediate step of a planar decomposition of an equality
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecomposeStep {
    pub start: Vec<Annot<String>>,
    pub middle_left: Vec<Annot<String>>,
    pub middle_right: Vec<Annot<String>>,
    pub end: Vec<Annot<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    // Interpret the term as an object, and try to insert it into the graph
    InsertNode(Annot<String>),
    // Interpret the term as a morphism, and try to insert it into the graph
    InsertMorphism(Annot<String>),
    // Insert the second argument as a morphism rooted at the first
    InsertMorphismAt(Annot<String>, Annot<String>),
    // Normalise and split an edge in the graph
    Split(Annot<String>),
    // Hide/reveal a node
    HideNode(Annot<String>),
    RevealNode(Annot<String>),
    // Hide/reveal a morphism
    HideMorphism(Annot<String>),
    RevealMorphism(Annot<String>),
    // Hide/reveal a face
    HideFace(Annot<String>),
    RevealFace(Annot<String>),
    // Try to solve a face using the automatic solver
    Solve(Option<Annot<usize>>, Annot<String>),
    // If too sides of a face are the same at the end, simplify it. None means
    // by as much as possible.
    PullFace(Annot<String>, Option<usize>),
    // Same but for the beggining
    PushFace(Annot<String>, Option<usize>),
    // Combined action
    ShrinkFace(Annot<String>),
    // Apply lemma
    Lemma(Annot<String>, Vec<(Annot<String>, Annot<String>)>),
    // Decompose a lemma into sub-faces
    Decompose(Annot<String>, Vec<DecomposeStep>),
    // End the interface with a success
    Succeed,
    // End the interface with a failure
    Fail,
}
