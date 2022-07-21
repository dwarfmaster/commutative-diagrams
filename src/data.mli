
type kind = (EConstr.t,EConstr.t,EConstr.ESorts.t,EConstr.EInstance.t) Constr.kind_of_term

type cat_id  = int
type elem_id = int
type mph_id  = int
type face_id = int

type category =
  { obj : EConstr.t
  ; id  : cat_id
  }
val comp_category : (EConstr.t -> EConstr.t -> bool) -> category -> category -> bool
type elem =
  { obj      : EConstr.t
  ; category : category
  ; id       : elem_id
  }
val comp_elem : (EConstr.t -> EConstr.t -> bool) -> elem -> elem -> bool
type morphismT =
  { category : category
  ; src      : elem
  ; dst      : elem
  ; obj      : EConstr.t
  }
val comp_morphismT : (EConstr.t -> EConstr.t -> bool) -> morphismT -> morphismT -> bool
type morphismData =
  { obj : EConstr.t
  ; tp  : morphismT
  }
val comp_morphismData : (EConstr.t -> EConstr.t -> bool) -> morphismData -> morphismData -> bool
type morphismBase =
  { data : morphismData
  ; id   : mph_id
  }
val comp_morphismBase : (EConstr.t -> EConstr.t -> bool) -> morphismBase -> morphismBase -> bool
type isoData =
  { obj : EConstr.t
  ; mph : morphismBase
  ; inv : morphismBase
  }
val comp_isoData : (EConstr.t -> EConstr.t -> bool) -> isoData -> isoData -> bool
type epiData =
  { obj : EConstr.t
  ; mph : morphismBase
  }
val comp_epiData : (EConstr.t -> EConstr.t -> bool) -> epiData -> epiData -> bool
type monoData =
  { obj : EConstr.t 
  ; mph : morphismBase 
  }
val comp_monoData : (EConstr.t -> EConstr.t -> bool) -> monoData -> monoData -> bool
type morphismShape =
  | Base of morphismBase
val comp_morphismShape : (EConstr.t -> EConstr.t -> bool) -> morphismShape -> morphismShape -> bool
type morphism =
  { data  : morphismData
  ; shape : morphismShape
  }
val comp_morphism : (EConstr.t -> EConstr.t -> bool) -> morphism -> morphism -> bool

val fromBase : morphismBase -> morphism 

(* Equality between uninterned morphisms *)
type eqT =
  | Refl of morphismData
  | Concat of eq * eq
  | Inv of eq
  | Compose of eq * eq
  | Assoc of morphismData * morphismData * morphismData
  | LeftId of morphismData
  | RightId of morphismData
  | RAp of eq * morphismData
  | LAp of morphismData * eq
  | RInv of isoData
  | LInv of isoData
  | Mono of EConstr.t * morphismData * morphismData * eq
  | Epi of EConstr.t * morphismData * morphismData * eq
  | Atom of EConstr.t
and eq =
  { src : morphismData
  ; dst : morphismData
  ; tp  : morphismT
  ; eq  : eqT
  }
val comp_eqT : (EConstr.t -> EConstr.t -> bool) -> eqT -> eqT -> bool
val comp_eq : (EConstr.t -> EConstr.t -> bool) -> eq -> eq -> bool

(* The composed morphism of the path may not be in the context since we only keep the base *)
type path =
  { mph  : morphismData
  ; eq   : eq (* Equality from `mph` to `realize path` *)
  ; path : morphism list
  }
val comp_path : (EConstr.t -> EConstr.t -> bool) -> path -> path -> bool
type face =
  { tp    : morphismT
  ; side1 : path
  ; side2 : path
  ; obj   : eq (* Equality between side1.mph and side2.mph *)
  ; id    : face_id
  }
val comp_face : (EConstr.t -> EConstr.t -> bool) -> face -> face -> bool

