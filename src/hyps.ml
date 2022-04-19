
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
type morphism =
  { obj : EConstr.t
  ; tp  : morphismT
  ; id  : mph_id
  }

type eq =
  { src : EConstr.t
  ; dst : EConstr.t
  ; tp  : morphismT
  ; eq  : EConstr.t
  }

type path =
  { mph  : EConstr.t
  ; tp   : morphismT
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
type t =
  { categories : category array
  ; elems      : elem array
  ; morphisms  : morphism array
  ; faces      : face array
  }

exception Ill_typed
exception Unimplemented

(*   ____            _            _    *)
(*  / ___|___  _ __ | |_ _____  _| |_  *)
(* | |   / _ \| '_ \| __/ _ \ \/ / __| *)
(* | |__| (_) | | | | ||  __/>  <| |_  *)
(*  \____\___/|_| |_|\__\___/_/\_\\__| *)

let array_find_id : ('a -> bool) -> 'a array -> int option = fun pred arr ->
  let result : int option ref = ref None in
  for i = 0 to Array.length arr - 1 do
    if pred arr.(i) then result := Some i else ()
  done;
  !result

let empty_context =
  { categories = [| |]
  ; elems      = [| |]
  ; morphisms  = [| |]
  ; faces      = [| |] }

let get_cat  = fun sigma (cat : EConstr.t) store ->
  let id = array_find_id (fun(c : category) -> EConstr.eq_constr sigma cat c.obj) store.categories in
  match id with
  | Some id -> (id,store)
  | None -> let nid = Array.length store.categories in
    (nid,
     { categories = Array.append store.categories [| { obj = cat; id = Array.length store.categories } |]
     ; elems = store.elems
     ; morphisms = store.morphisms
     ; faces = store.faces })

let get_elem = fun sigma (cat : EConstr.t) elm store ->
  let (cid,store) = get_cat sigma cat store in
  let cat = store.categories.(cid) in
  let id = array_find_id (fun(e : elem) -> EConstr.eq_constr sigma elm e.obj) store.elems in
  match id with
  | Some id -> (id,store)
  | None -> let nid = Array.length store.elems in
    (nid,
     { categories = store.categories
     ; elems = Array.append store.elems [| { obj = elm; id = nid; category = cat } |]
     ; morphisms = store.morphisms
     ; faces = store.faces })

let get_mph  = fun sigma (cat : EConstr.t) src dst mph store ->
  let (cid,store) = get_cat sigma cat store in
  let cat = store.categories.(cid) in
  let (src_id,store) = get_elem sigma cat.obj src store in
  let src = store.elems.(src_id) in
  let (dst_id,store) = get_elem sigma cat.obj dst store in
  let dst = store.elems.(dst_id) in
  let id = array_find_id (fun(m : morphism) -> EConstr.eq_constr sigma mph m.obj) store.morphisms in
  match id with
  | Some id -> (id,store)
  | None ->
    let nid = Array.length store.morphisms in
    let tp = EConstr.mkApp (Env.mk_mphT (),
                            [| cat.obj; src.obj; dst.obj |]) in
    (nid,
     { categories = store.categories
     ; elems = store.elems
     ; morphisms = Array.append store.morphisms
           [| { obj = mph; tp = { category = cat; src = src; dst = dst; obj = tp }; id = nid } |]
     ; faces = store.faces })

let get_face = fun sigma cat src dst mph1 mph2 fce store ->
  let (cid,store) = get_cat sigma cat store in
  let cat = store.categories.(cid) in
  let (src_id,store) = get_elem sigma cat.obj src store in
  let src = store.elems.(src_id) in
  let (dst_id,store) = get_elem sigma cat.obj dst store in
  let dst = store.elems.(dst_id) in
  let id = array_find_id (fun(f : face) -> EConstr.eq_constr sigma fce f.obj.eq) store.faces in
  match id with
  | Some id -> (id,store)
  | None ->
    let nid = Array.length store.faces in
    let tp = EConstr.mkApp (Env.mk_mphT (),
                            [| cat.obj; src.obj; dst.obj |]) in
    let tp = { category = cat; src = src; dst = dst; obj = tp } in
    (nid,
     { categories = store.categories
     ; elems = store.elems
     ; morphisms = store.morphisms
     ; faces = Array.append store.faces
           [| { tp = tp
              ; side1 = { mph = mph1; eq = raise Unimplemented; path = raise Unimplemented; tp = tp }
              ; side2 = { mph = mph2; eq = raise Unimplemented; path = raise Unimplemented; tp = tp }
              ; obj = { src = mph1; dst = mph2; tp = tp; eq = fce }
              ; id = nid } |]
     })



(*  __  __                  _     _ *)
(* |  \/  | ___  _ __ _ __ | |__ (_)___ _ __ ___  ___ *)
(* | |\/| |/ _ \| '__| '_ \| '_ \| / __| '_ ` _ \/ __| *)
(* | |  | | (_) | |  | |_) | | | | \__ \ | | | | \__ \ *)
(* |_|  |_|\___/|_|  | .__/|_| |_|_|___/_| |_| |_|___/ *)
(*                   |_| *)

let mphT = fun sigma env (cat : category) e1 e2 ->
  { category = cat
  ; src = { obj = e1; category = cat; id = 0; }
  ; dst = { obj = e2; category = cat; id = 0; }
  ; obj = EConstr.mkApp (Env.mk_mphT (),
                         [| cat.obj; e1; e2 |])
  }
let compose = fun sigma env (m1 : morphism) (m2 : morphism) ->
  { obj = EConstr.mkApp (Env.mk_comp (), [| m1.tp.category.obj
                                          ; m1.tp.src.obj; m1.tp.dst.obj; m2.tp.dst.obj
                                          ; m1.obj; m2.obj |])
  ; tp = mphT sigma env m1.tp.category m1.tp.src.obj m2.tp.dst.obj
  ; id = 0
  }
let composeT = fun sigma env mT1 (mT2 : morphismT) -> mphT sigma env mT1.category mT1.src.obj mT2.dst.obj
let identity = fun sigma env (x : elem) ->
  { obj = EConstr.mkApp (Env.mk_id (), [| x.category.obj; x.obj |])
  ; tp = mphT sigma env x.category x.obj x.obj
  ; id = 0; }
let rec realize = fun sigma env (ms : morphism list) ->
  match ms with
  | [] -> raise Ill_typed
  | [ m ] -> m
  | m :: ms -> compose sigma env m (realize sigma env ms)




(*  _____                  _ _ _          *)
(* | ____|__ _ _   _  __ _| (_) |_ _   _  *)
(* |  _| / _` | | | |/ _` | | | __| | | | *)
(* | |__| (_| | |_| | (_| | | | |_| |_| | *)
(* |_____\__, |\__,_|\__,_|_|_|\__|\__, | *)
(*          |_|                    |___/  *)

let refl = fun sigma env (m : morphism) ->
  { src = m.obj
  ; dst = m.obj
  ; tp  = m.tp
  ; eq  = EConstr.mkApp (Env.mk_refl (), [| m.tp.obj; m.obj |])
  }

let concat = fun sigma env (p1 : eq) (p2 : eq) ->
  { src = p1.src
  ; dst = p2.dst
  ; tp  = p1.tp
  ; eq  = EConstr.mkApp (Env.mk_concat (),
                        [| p1.tp.obj; p1.src; p1.dst; p2.dst; p1.eq; p2.eq |])
  }

let inv = fun sigma env p ->
  { src = p.dst
  ; dst = p.src
  ; tp  = p.tp
  ; eq  = EConstr.mkApp (Env.mk_inv (),
                        [| p.tp.obj; p.src; p.dst; p.eq |])
  }

let composeP = fun sigma env p1 p2 ->
  raise Unimplemented
  (* { src = compose sigma env p1.src p2.src *)
  (* ; dst = compose sigma env p1.dst p2.dst *)
  (* ; tp  = composeT sigma env p1.tp p2.tp *)
  (* ; eq  = EConstr.mkApp (Env.mk_compose_eq (), *)
  (*                        [| p1.tp.category.obj; p1.src.tp.src.obj; p1.src.tp.dst.obj; p2.src.tp.dst.obj *)
  (*                         ; p1.src.obj; p1.dst.obj; p2.src.obj; p2.dst.obj; p1.eq; p2.eq |]) *)
  (* } *)

let assoc = fun sigma env m1 m2 m3 ->
  raise Unimplemented
  (* { src = compose sigma env m1 (compose sigma env m2 m3) *)
  (* ; dst = compose sigma env (compose sigma env m1 m2) m3 *)
  (* ; tp  = composeT sigma env (composeT sigma env m1.tp m2.tp) m3.tp *)
  (* ; eq  = EConstr.mkApp (Env.mk_assoc (), *)
  (*                        [| m1.tp.category.obj *)
  (*                         ; m1.tp.src.obj; m2.tp.src.obj; m3.tp.src.obj; m3.tp.dst.obj *)
  (*                         ; m1.obj; m2.obj; m3.obj |]) *)
  (* } *)

let left_id = fun sigma env (m : morphism) ->
  raise Unimplemented
  (* { src = compose sigma env (identity sigma env m.tp.dst) m *)
  (* ; dst = m *)
  (* ; tp  = m.tp *)
  (* ; eq  = EConstr.mkApp (Env.mk_left_id (), *)
  (*                        [| m.tp.category.obj; m.tp.src.obj; m.tp.dst.obj; m.obj |]) *)
  (* } *)

let right_id = fun sigma env (m : morphism) ->
  raise Unimplemented
  (* { src = compose sigma env m (identity sigma env m.tp.dst) *)
  (* ; dst = m *)
  (* ; tp  = m.tp *)
  (* ; eq  = EConstr.mkApp (Env.mk_right_id (), *)
  (*                        [| m.tp.category.obj; m.tp.src.obj; m.tp.dst.obj; m.obj |]) *)
  (* } *)

(* a = b -> [ m1 m2 ] -> a o m1 o m2 = b o m1 o m2 *)
let rec lift_eq : Evd.evar_map -> Environ.env -> eq -> morphism list -> eq =
  fun sigma env p mphs ->
  match mphs with
  | [ ] -> p
  | m :: mphs -> lift_eq sigma env (composeP sigma env p (refl sigma env m)) mphs

(* left -> [ m1 m2 ] -> right -> left o (m1 o m2 o right) = left o m1 o m2 o right  *)
let rec repeat_assoc : Evd.evar_map -> Environ.env -> morphism -> morphism list -> morphism list -> eq =
  fun sigma env left mphs right ->
  match List.rev mphs with
  | [ ] -> refl sigma env (compose sigma env left (realize sigma env right))
  | m :: mphs ->
    let mphs = List.rev mphs in
    concat sigma env
      (lift_eq sigma env (assoc sigma env left (realize sigma env mphs) m) right)
      (repeat_assoc sigma env left mphs (m :: right))

let rec normalize = fun sigma env (m : morphism) ->
  match EConstr.kind sigma m.obj with
  | App (cmp, [| src; int; dst; mid; msi |]) ->
    begin match EConstr.kind sigma cmp with
      | Proj (cmp,_) when Env.is_projection cmp Env.is_cat "compose" ->
        let (d1,p1) = normalize sigma env
            { obj = msi
            ; tp = mphT sigma env m.tp.category src int
            ; id = 0 } in
        let (d2,p2) = normalize sigma env
            { obj = mid
            ; tp  = mphT sigma env m.tp.category int dst
            ; id  = 0 } in
        let p = composeP sigma env p1 p2 in
        let p' = repeat_assoc sigma env (realize sigma env d1) d2 [ ] in
        (List.append d1 d2, concat sigma env p p')
      | _ -> ([m], refl sigma env m)
    end
  | _ -> ([m], refl sigma env m)




(*  ____                _              *)
(* |  _ \ __ _ _ __ ___(_)_ __   __ _  *)
(* | |_) / _` | '__/ __| | '_ \ / _` | *)
(* |  __/ (_| | |  \__ \ | | | | (_| | *)
(* |_|   \__,_|_|  |___/_|_| |_|\__, | *)
(*                              |___/  *)
let parse_cat  = fun sigma env cat store -> raise Unimplemented
let parse_elem = fun sigma env elm store -> raise Unimplemented
let parse_mph  = fun sigma env mph store -> raise Unimplemented
let parse_face = fun sigma env fce store -> raise Unimplemented
