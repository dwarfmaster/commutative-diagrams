
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

val normCategory : preCategory -> Data.category Monad.m
val normElem : preElem -> Data.elem Monad.m
val normMorphismT : preMorphismType -> Data.morphismT Monad.m
val normMorphism : preMorphism -> Data.path Monad.m
val normIso : preIso -> Data.isoData Monad.m
val normEpi : preEpi -> Data.epiData Monad.m
val normMono : preMono -> Data.monoData Monad.m
val normEq : preMorphism preEq -> Data.eq Monad.m
val normFace : preFace -> Data.face Monad.m
