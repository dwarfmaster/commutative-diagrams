
module Make = functor (C : Utils.ConstrLike) -> struct
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

  exception Unimplemented
  let compose = fun sigma env m1 m2 -> raise Unimplemented
  let composeT = fun sigma env mT1 mT2 -> raise Unimplemented
  let realize = fun sigma env ms -> raise Unimplemented
end
