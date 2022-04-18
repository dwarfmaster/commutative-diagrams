
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

exception Unimplemented
let compose = fun sigma env m1 m2 -> raise Unimplemented
let composeT = fun sigma env mT1 mT2 -> raise Unimplemented
let mphT = fun sigma env cat e1 e2 -> raise Unimplemented
let realize = fun sigma env ms -> raise Unimplemented
let identity = fun sigma env (x : elem) ->
  { obj = raise Unimplemented
  ; tp = { category = x.category; src = x; dst = x; obj = raise Unimplemented; }
  ; id = 0; }
