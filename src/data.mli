
type kind = (EConstr.t,EConstr.t,EConstr.ESorts.t,EConstr.EInstance.t) Constr.kind_of_term

type cat_id  = int
type elem_id = int
type mph_id  = int
type face_id = int

type category =
  { obj : EConstr.t
  ; id  : cat_id
  }
type elem =
  { obj      : EConstr.t
  ; category : category
  ; id       : elem_id
  }
type morphismT =
  { category : category
  ; src      : elem
  ; dst      : elem
  ; obj      : EConstr.t
  }
type morphismData =
  { obj : EConstr.t
  ; tp  : morphismT
  }
type isoData =
  { obj : EConstr.t
  ; mph : morphism
  ; inv : morphism
  }
and morphism =
  { data : morphismData
  ; id   : mph_id
  ; mutable mono : EConstr.t option
  ; mutable epi  : EConstr.t option
  ; mutable iso  : isoData option
  }

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

(* The composed morphism of the path may not be in the context since we only keep the base *)
type path =
  { mph  : morphismData
  ; eq   : eq (* Equality from `mph` to `realize path` *)
  ; path : morphism list
  }
type face =
  { tp    : morphismT
  ; side1 : path
  ; side2 : path
  ; obj   : eq (* Equality between side1.mph and side2.mph *)
  ; id    : face_id
  }

