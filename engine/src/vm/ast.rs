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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    // Interpret the term as an object, and try to insert it into the graph
    InsertNode(Annot<TermDescr>),
    // Interpret the term as a morphism, and try to insert it into the graph
    InsertMorphism(Annot<TermDescr>),
    // Insert the second argument as a morphism rooted at the first
    InsertMorphismAt(Annot<usize>, Annot<TermDescr>),
    // Normalise and split an edge in the graph
    Split(Annot<TermDescr>),
    // Hide/reveal a node
    HideNode(Annot<TermDescr>),
    RevealNode(Annot<TermDescr>),
    // Hide/reveal a morphism
    HideMorphism(Annot<TermDescr>),
    RevealMorphism(Annot<TermDescr>),
    // Hide/reveal a face
    HideFace(Annot<TermDescr>),
    RevealFace(Annot<TermDescr>),
    // Try to solve a face using the automatic solver
    Solve(Option<Annot<usize>>, Annot<TermDescr>),
    // If too sides of a face are the same at the end, simplify it. None means
    // by as much as possible.
    PullFace(Annot<TermDescr>, Option<usize>),
    // Same but for the beggining
    PushFace(Annot<TermDescr>, Option<usize>),
    // Combined action
    ShrinkFace(Annot<TermDescr>),
    // Apply lemma
    Lemma(Annot<String>, Vec<(Annot<Id>, Annot<Id>)>),
    // Unify the two terms as equalities
    Refine(Annot<TermDescr>, Annot<TermDescr>),
    // End the interface with a success
    Succeed,
    // End the interface with a failure
    Fail,
}

// A generic term that can describes/construct a term, ie either
// a category, a morphism, a functor, an object or an equality.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TermDescr {
    Ref(Annot<Id>),
    Hole,
    // TODO add other constructors
}

// An identifier. It can either be a name, or a numerical id into
// the graph.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Id {
    Name(String),
    Id(usize),
}
