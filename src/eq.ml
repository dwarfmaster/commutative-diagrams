
module T = Morphisms.Make(Utils.CLEConstr)

type t =
  { src : T.morphism
  ; dst : T.morphism
  ; tp  : T.morphismT
  ; eq  : EConstr.t
  }

exception Ill_typed
exception Unimplemented

let refl = fun sigma env m ->
  { src = m
  ; dst = m
  ; tp  = m.tp
  ; eq  = EConstr.mkApp (Env.mk_refl (), [| m.tp.obj; m.obj |])
  }

let concat = fun sigma env p1 p2 ->
  { src = p1.src
  ; dst = p2.dst
  ; tp  = p1.tp
  ; eq  = EConstr.mkApp (Env.mk_concat (),
                        [| p1.tp.obj; p1.src.obj; p1.dst.obj; p2.dst.obj; p1.eq; p2.eq |])
  }

let compose = fun sigma env p1 p2 ->
  { src = T.compose sigma env p1.src p2.src
  ; dst = T.compose sigma env p1.dst p2.dst
  ; tp  = T.composeT sigma env p1.tp p2.tp
  ; eq  = EConstr.mkApp (Env.mk_compose_eq (),
                         [| p1.tp.category.obj; p1.src.tp.src.obj; p1.src.tp.dst.obj; p2.src.tp.dst.obj
                          ; p1.src.obj; p1.dst.obj; p2.src.obj; p2.dst.obj; p1.eq; p2.eq |])
  }

let assoc = fun sigma env m1 m2 m3 ->
  raise Unimplemented

let left_id = fun sigma env m ->
  raise Unimplemented

let right_id = fun sigma env m ->
  raise Unimplemented

let normalize = fun sigma env m ->
  raise Unimplemented
