
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
  type morphism =
    { obj      : constr
    ; category : category
    ; src      : elem
    ; dst      : elem
    ; id       : mph_id
    }
end
