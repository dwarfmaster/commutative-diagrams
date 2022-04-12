
module Make = functor (C : Utils.ConstrLike) -> struct
  type t =
    { src : EConstr.t
    ; dst : EConstr.t
    ; tp  : EConstr.t
    ; eq  : EConstr.t
    }

  exception Ill_typed
  exception Unimplemented

  let refl = fun sigma env tp x ->
    { src = C.econstr x
    ; dst = C.econstr x
    ; tp  = C.econstr tp
    ; eq  = EConstr.mkApp (Env.mk_refl (), [| C.econstr tp; C.econstr x |])
    }

  let concat = fun sigma env p1 p2 ->
    { src = p1.src
    ; dst = p2.dst
    ; tp  = p1.tp
    ; eq  = EConstr.mkApp (Env.mk_concat (),
                          [| p1.tp; p1.src; p1.dst; p2.dst; p1.eq; p2.eq |])
    }

  let compose = fun sigma env p1 p2 ->
    raise Unimplemented

  let assoc = fun sigma env m1 m2 m3 ->
    raise Unimplemented

  let left_id = fun sigma env m ->
    raise Unimplemented

  let right_id = fun sigma env m ->
    raise Unimplemented

  let realize = fun sigma env ms ->
    raise Unimplemented

  let normalize = fun sigma env m ->
    raise Unimplemented
end
