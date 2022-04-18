
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
type morphism =
  { obj : EConstr.t
  ; tp  : morphismT
  ; id  : mph_id
  }

(* m1 -> m2 -> m2 o m1 *)
val compose : Evd.evar_map -> Environ.env -> morphism -> morphism -> morphism
val composeT : Evd.evar_map -> Environ.env -> morphismT -> morphismT -> morphismT
val mphT : Evd.evar_map -> Environ.env -> category -> EConstr.t -> EConstr.t -> morphismT
(* [ m1, m2, m3 ] -> (m3 o m2) o m1 *)
val realize : Evd.evar_map -> Environ.env -> morphism list -> morphism
(* a -> 1_a *)
val identity : Evd.evar_map -> Environ.env -> elem -> morphism
