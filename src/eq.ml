
module T = Morphisms.Make(Utils.CLEConstr)

type t =
  { src : T.morphism
  ; dst : T.morphism
  ; tp  : T.morphismT
  ; eq  : EConstr.t
  }

exception Ill_typed

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

let inv = fun sigma env p ->
  { src = p.dst
  ; dst = p.src
  ; tp  = p.tp
  ; eq  = EConstr.mkApp (Env.mk_inv (),
                        [| p.tp.obj; p.src.obj; p.dst.obj; p.eq |])
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
  { src = T.compose sigma env m1 (T.compose sigma env m2 m3)
  ; dst = T.compose sigma env (T.compose sigma env m1 m2) m3
  ; tp  = T.composeT sigma env (T.composeT sigma env m1.tp m2.tp) m3.tp
  ; eq  = EConstr.mkApp (Env.mk_assoc (),
                         [| m1.tp.category.obj
                          ; m1.tp.src.obj; m2.tp.src.obj; m3.tp.src.obj; m3.tp.dst.obj
                          ; m1.obj; m2.obj; m3.obj |])
  }

let left_id = fun sigma env (m : T.morphism) ->
  { src = T.compose sigma env (T.identity sigma env m.tp.dst) m
  ; dst = m
  ; tp  = m.tp
  ; eq  = EConstr.mkApp (Env.mk_left_id (),
                         [| m.tp.category.obj; m.tp.src.obj; m.tp.dst.obj; m.obj |])
  }

let right_id = fun sigma env (m : T.morphism) ->
  { src = T.compose sigma env m (T.identity sigma env m.tp.dst)
  ; dst = m
  ; tp  = m.tp
  ; eq  = EConstr.mkApp (Env.mk_right_id (),
                         [| m.tp.category.obj; m.tp.src.obj; m.tp.dst.obj; m.obj |])
  }

(* a = b -> [ m1 m2 ] -> a o m1 o m2 = b o m1 o m2 *)
let rec lift_eq : Evd.evar_map -> Environ.env -> t -> T.morphism list -> t =
  fun sigma env p mphs ->
  match mphs with
  | [ ] -> p
  | m :: mphs -> lift_eq sigma env (compose sigma env p (refl sigma env m)) mphs

(* left -> [ m1 m2 ] -> right -> left o (m1 o m2 o right) = left o m1 o m2 o right  *)
let rec repeat_assoc : Evd.evar_map -> Environ.env -> T.morphism -> T.morphism list -> T.morphism list -> t =
  fun sigma env left mphs right ->
  match List.rev mphs with
  | [ ] -> refl sigma env (T.compose sigma env left (T.realize sigma env right))
  | m :: mphs ->
    let mphs = List.rev mphs in
    concat sigma env
      (lift_eq sigma env (assoc sigma env left (T.realize sigma env mphs) m) right)
      (repeat_assoc sigma env left mphs (m :: right))

let rec normalize = fun sigma env (m : T.morphism) ->
  match EConstr.kind sigma m.obj with
  | App (cmp, [| src; int; dst; mid; msi |]) ->
    begin match EConstr.kind sigma cmp with
      | Proj (cmp,_) when Env.is_projection cmp Env.is_cat "compose" ->
        let (d1,p1) = normalize sigma env
            { obj = msi
            ; tp = T.mphT sigma env m.tp.category src int
            ; id = 0 } in
        let (d2,p2) = normalize sigma env
            { obj = mid
            ; tp  = T.mphT sigma env m.tp.category int dst
            ; id  = 0 } in
        let p = compose sigma env p1 p2 in
        let p' = repeat_assoc sigma env (T.realize sigma env d1) d2 [ ] in
        (List.append d1 d2, concat sigma env p p')
      | _ -> ([m], refl sigma env m)
    end
  | _ -> ([m], refl sigma env m)
