pub type AST = Vec<Action>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    // Interpret the term as an object, and try to insert it into the graph
    InsertNode(TermDescr),
    // Interpret the term as a morphism, and try to insert it into the graph
    InsertMorphism(TermDescr),
    // Insert the second argument as a morphism rooted at the first
    InsertMorphismAt(usize, TermDescr),
    // Normalise and split an edge in the graph
    Split(TermDescr),
    // Hide/reveal a node
    HideNode(TermDescr),
    RevealNode(TermDescr),
    // Hide/reveal a morphism
    HideMorphism(TermDescr),
    RevealMorphism(TermDescr),
    // Hide/reveal a face
    HideFace(TermDescr),
    RevealFace(TermDescr),
    // Try to solve a face using the automatic solver
    Solve(TermDescr),
    // Unify the two terms as equalities
    Refine(TermDescr, TermDescr),
    // End the interface with a success
    Succeed,
    // End the interface with a failure
    Fail,
}

// A generic term that can describes/construct a term, ie either
// a category, a morphism, a functor, an object or an equality.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TermDescr {
    Ref(Id),
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
