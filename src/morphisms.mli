
module Make : functor (C : Utils.ConstrLike) -> sig
  type constr = C.constr
  type kind   = (constr,C.types,C.sorts,C.univs) Constr.kind_of_term

  type cat_id  = int
  type elem_id = int
  type mph_id  = int
  type face_id = int

  type category =
    { obj : constr
    ; id  : cat_id
    }
  type elem =
    { obj      : constr
    ; category : category
    ; id       : elem_id
    }
  type morphismT =
    { category : category
    ; src      : elem
    ; dst      : elem
    ; obj      : constr
    }
  type morphism =
    { obj : constr
    ; tp  : morphismT
    ; id  : mph_id
    }

  (* m1 -> m2 -> m2 o m1 *)
  val compose : Evd.evar_map -> Environ.env -> morphism -> morphism -> morphism
  val composeT : Evd.evar_map -> Environ.env -> morphismT -> morphismT -> morphismT
  (* [ m1, m2, m3 ] -> (m3 o m2) o m1 *)
  val realize : Evd.evar_map -> Environ.env -> morphism list -> morphism
end
