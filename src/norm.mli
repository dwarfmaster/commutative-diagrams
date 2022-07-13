
type preCategoryShape =
  | Cat of EConstr.t
type preCategory =
  { shape : preCategoryShape }
type preElemShape =
  | Elem of EConstr.t
type preElem =
  { shape    : preElemShape
  ; category : preCategory
  }
type preMorphismType =
  { src : preElem
  ; dst : preElem
  ; category : preCategory
  ; obj : EConstr.t
  }
type preMorphismShape =
  | Id   of preElem
  | Comp of preMorphism * preMorphism
  | Inv  of preMorphism
  | Mph  of EConstr.t
and preMorphism =
  { shape : preMorphismShape
  ; tp : preMorphismType
  }
type preIso =
  { mph : preMorphism
  ; inv : preMorphism
  ; iso : EConstr.t
  }
type preMono =
  { mph : preMorphism
  ; mono : EConstr.t 
  }
type preEpi =
  { mph : preMorphism
  ; epi : EConstr.t
  }
type 'a preEqShape =
  | Refl of 'a
  | Concat of 'a preEq * 'a preEq
  | Inv of 'a preEq
  | Eq of EConstr.t
and 'a preEq =
  { shape : 'a preEqShape
  ; src : 'a
  ; dst : 'a 
  ; tp  : EConstr.t
  }
type preFace =
  { tp  : preMorphismType
  ; obj : preMorphism preEq
  }
