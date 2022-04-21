
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
type morphism =
  { data : morphismData
  ; id   : mph_id
  }
let extract : morphism list -> morphismData list = List.map (fun m -> m.data)


type eq =
  { src : morphismData
  ; dst : morphismData
  ; tp  : morphismT
  ; eq  : EConstr.t
  }

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
type t =
  { categories : category array
  ; elems      : elem array
  ; morphisms  : morphism array
  ; faces      : face array
  }

exception Ill_typed


(*  __  __                  _     _ *)
(* |  \/  | ___  _ __ _ __ | |__ (_)___ _ __ ___  ___ *)
(* | |\/| |/ _ \| '__| '_ \| '_ \| / __| '_ ` _ \/ __| *)
(* | |  | | (_) | |  | |_) | | | | \__ \ | | | | \__ \ *)
(* |_|  |_|\___/|_|  | .__/|_| |_|_|___/_| |_| |_|___/ *)
(*                   |_| *)

let mphT = fun sigma cat e1 e2 ->
  EConstr.mkApp (Env.mk_mphT (), [| cat; e1; e2 |])
let composeT = fun sigma (mT1 : morphismT) (mT2 : morphismT) ->
  { category = mT1.category
  ; src = mT1.src
  ; dst = mT2.dst
  ; obj = mphT sigma mT1.category.obj mT1.src.obj mT2.dst.obj
  }
let compose = fun sigma (m1 : morphismData) (m2 : morphismData) ->
  { obj = EConstr.mkApp (Env.mk_comp (), [| m1.tp.category.obj
                                          ; m1.tp.src.obj; m1.tp.dst.obj; m2.tp.dst.obj
                                          ; m1.obj; m2.obj |])
  ; tp = composeT sigma m1.tp m2.tp }
let identity = fun sigma (x : elem) ->
  { obj = EConstr.mkApp (Env.mk_id (), [| x.category.obj; x.obj |])
  ; tp = { category = x.category
         ; src = x
         ; dst = x
         ; obj = mphT sigma x.category.obj x.obj x.obj }
  }
let rec realize' = fun sigma src (ms : morphismData list) ->
  match ms with
  | [] -> identity sigma src
  | [ m ] -> m
  | m :: ms -> compose sigma (realize' sigma src ms) m
let realize = fun sigma src ms -> realize' sigma src (List.rev ms)
let rpath = fun sigma pth -> realize sigma pth.mph.tp.src (extract pth.path)




(*  _____                  _ _ _          *)
(* | ____|__ _ _   _  __ _| (_) |_ _   _  *)
(* |  _| / _` | | | |/ _` | | | __| | | | *)
(* | |__| (_| | |_| | (_| | | | |_| |_| | *)
(* |_____\__, |\__,_|\__,_|_|_|\__|\__, | *)
(*          |_|                    |___/  *)

let eqT = fun sigma (m1 : morphismData) (m2 : morphismData) ->
  EConstr.mkApp (Env.mk_eq (), [| m1.tp.obj; m1.obj; m2.obj |])

let refl = fun sigma (m : morphismData) ->
  { src = m
  ; dst = m
  ; tp  = m.tp
  ; eq  = EConstr.mkApp (Env.mk_refl (), [| m.tp.obj; m.obj |])
  }

let concat = fun sigma (p1 : eq) (p2 : eq) ->
  { src = p1.src
  ; dst = p2.dst
  ; tp  = p1.tp
  ; eq  = EConstr.mkApp (Env.mk_concat (),
                        [| p1.tp.obj; p1.src.obj; p1.dst.obj; p2.dst.obj; p1.eq; p2.eq |])
  }

let inv = fun sigma p ->
  { src = p.dst
  ; dst = p.src
  ; tp  = p.tp
  ; eq  = EConstr.mkApp (Env.mk_inv (),
                        [| p.tp.obj; p.src.obj; p.dst.obj; p.eq |])
  }

let composeP = fun sigma p1 p2 ->
  { src = compose sigma p1.src p2.src
  ; dst = compose sigma p1.dst p2.dst
  ; tp  = composeT sigma p1.tp p2.tp
  ; eq  = EConstr.mkApp (Env.mk_compose_eq (),
                         [| p1.tp.category.obj; p1.src.tp.src.obj; p1.src.tp.dst.obj; p2.src.tp.dst.obj
                          ; p1.src.obj; p1.dst.obj; p2.src.obj; p2.dst.obj; p1.eq; p2.eq |])
  }

let assoc = fun sigma m1 m2 m3 ->
  { src = compose sigma m1 (compose sigma m2 m3)
  ; dst = compose sigma (compose sigma m1 m2) m3
  ; tp  = composeT sigma (composeT sigma m1.tp m2.tp) m3.tp
  ; eq  = EConstr.mkApp (Env.mk_assoc (),
                         [| m1.tp.category.obj
                          ; m1.tp.src.obj; m2.tp.src.obj; m3.tp.src.obj; m3.tp.dst.obj
                          ; m1.obj; m2.obj; m3.obj |])
  }

let left_id = fun sigma (m : morphismData) ->
  { src = compose sigma (identity sigma m.tp.dst) m
  ; dst = m
  ; tp  = m.tp
  ; eq  = EConstr.mkApp (Env.mk_left_id (),
                         [| m.tp.category.obj; m.tp.src.obj; m.tp.dst.obj; m.obj |])
  }

let right_id = fun sigma (m : morphismData) ->
  { src = compose sigma m (identity sigma m.tp.dst)
  ; dst = m
  ; tp  = m.tp
  ; eq  = EConstr.mkApp (Env.mk_right_id (),
                         [| m.tp.category.obj; m.tp.src.obj; m.tp.dst.obj; m.obj |])
  }




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

let get_mph  = fun sigma (mph : morphismData) store ->
  let id = array_find_id (fun(m : morphism) -> EConstr.eq_constr sigma mph.obj m.data.obj) store.morphisms in
  match id with
  | Some id -> (id,store)
  | None ->
    let nid = Array.length store.morphisms in
    (nid,
     { categories = store.categories
     ; elems = store.elems
     ; morphisms = Array.append store.morphisms [| { data = mph; id = nid } |]
     ; faces = store.faces })


(*  _   _                            _ _           _   _              *)
(* | \ | | ___  _ __ _ __ ___   __ _| (_)___  __ _| |_(_) ___  _ __   *)
(* |  \| |/ _ \| '__| '_ ` _ \ / _` | | / __|/ _` | __| |/ _ \| '_ \  *)
(* | |\  | (_) | |  | | | | | | (_| | | \__ \ (_| | |_| | (_) | | | | *)
(* |_| \_|\___/|_|  |_| |_| |_|\__,_|_|_|___/\__,_|\__|_|\___/|_| |_| *)


(* a = b -> [ m1 m2 ] -> a o m1 o m2 = b o m1 o m2 *)
let rec lift_eq : Evd.evar_map -> eq -> morphismData list -> eq =
  fun sigma p mphs ->
  match mphs with
  | [ ] -> p
  | m :: mphs -> lift_eq sigma (composeP sigma p (refl sigma m)) mphs

(* left -> [ m1 m2 ] -> right -> left o (m1 o m2 o right) = left o m1 o m2 o right  *)
let rec repeat_assoc : Evd.evar_map -> morphismData -> morphismData list -> morphismData list -> eq =
  fun sigma left mphs right ->
  match List.rev mphs with
  | [ ] -> (match right with
      | [ ] -> refl sigma left
      | _ -> refl sigma (compose sigma left (realize sigma left.tp.dst right)))
  | m :: [ ] -> (match right with
      | [ ] -> refl sigma (compose sigma left m)
      | _ -> assoc sigma left m (realize sigma m.tp.dst right))
  | m :: mphs ->
    let mphs = List.rev mphs in
    concat sigma
      (lift_eq sigma (assoc sigma left (realize sigma left.tp.dst mphs) m) right)
      (repeat_assoc sigma left mphs (m :: right))

let isIdentity : Evd.evar_map -> EConstr.t -> bool =
  fun sigma id ->
  match EConstr.kind sigma id with
  | App (id, [| cat; elem |]) ->
    begin match EConstr.kind sigma id with
      | Const (name,_) -> Env.is_id name
      | _ -> false
    end
  | App (id, [| elem |]) ->
    begin match EConstr.kind sigma id with
      | Proj (id,_) -> Env.is_projection id Env.is_cat "identity"
      | _ -> false
    end
  | _ -> false

let rec normalize = fun sigma (m : morphismData) store ->
  match EConstr.kind sigma m.obj with
  | App (cmp, [| src; int; dst; mid; msi |]) ->
    begin match EConstr.kind sigma cmp with
      | Proj (cmp,cat) when Env.is_projection cmp Env.is_cat "compose" ->
        let (catId,store) = get_cat sigma cat store in
        let (intId,store) = get_elem sigma cat int store in
        let msi =
            { obj = msi
            ; tp = { category = m.tp.category
                   ; src = m.tp.src
                   ; dst = store.elems.(intId)
                   ; obj = mphT sigma m.tp.category.obj src int }
            } in
        let mid =
            { obj = mid
            ; tp  = { category = m.tp.category
                    ; src = store.elems.(intId)
                    ; dst = m.tp.dst
                    ; obj = mphT sigma m.tp.category.obj int dst }
            } in
        let (d1,p1,store) = normalize sigma msi store in
        let (d2,p2,store) = normalize sigma mid store in
        (match d1,d2 with
         | [], _ -> (d2, concat sigma (right_id sigma mid) p1, store)
         | _, [] -> (d1, concat sigma (left_id sigma msi) p2, store)
         | _, _ ->
           let p = composeP sigma p1 p2 in
           let p' = repeat_assoc sigma (realize sigma m.tp.src (extract d1)) (extract d2) [ ] in
           (List.append d1 d2, concat sigma p p', store))
      | _ ->
        let (mId,store) = get_mph sigma m store in
        ([store.morphisms.(mId)], refl sigma m, store)
    end
  | _ when isIdentity sigma m.obj -> ([], refl sigma m, store)
  | _ ->
    let (mId,store) = get_mph sigma m store in
    ([store.morphisms.(mId)], refl sigma m, store)

let get_face = fun sigma tp mph1 mph2 fce store ->
  let id = array_find_id (fun(f : face) -> EConstr.eq_constr sigma fce f.obj.eq) store.faces in
  match id with
  | Some id -> (id,store)
  | None ->
    let mph1 = { obj = mph1; tp = tp } in
    let (d1,p1,store) = normalize sigma mph1 store in
    let mph2 = { obj = mph2; tp = tp } in
    let (d2,p2,store) = normalize sigma mph2 store in
    let nid = Array.length store.faces in
    (nid,
     { categories = store.categories
     ; elems = store.elems
     ; morphisms = store.morphisms
     ; faces = Array.append store.faces
           [| { tp = tp
              ; side1 = { mph = mph1; eq = p1; path = d1 }
              ; side2 = { mph = mph2; eq = p2; path = d2 }
              ; obj = { src = mph1; dst = mph2; tp = tp; eq = fce }
              ; id = nid } |]
     })


(*  ____                _              *)
(* |  _ \ __ _ _ __ ___(_)_ __   __ _  *)
(* | |_) / _` | '__/ __| | '_ \ / _` | *)
(* |  __/ (_| | |  \__ \ | | | | (_| | *)
(* |_|   \__,_|_|  |___/_|_| |_|\__, | *)
(*                              |___/  *)

let parse_cat  = fun sigma name cat store ->
  match EConstr.kind sigma cat with
  | Ind (ind,_) when Env.is_cat ind ->
    let (id,store) = get_cat sigma (EConstr.mkVar name) store in (store,Some id)
  | _ -> (store,None)

let parse_elem = fun sigma name elm store ->
  match EConstr.kind sigma elm with
  | Proj (p,arg) when Env.is_projection p Env.is_cat "object" ->
    let (id,store) = get_elem sigma arg (EConstr.mkVar name) store in (store,Some id)
  | _ -> (store,None)

let read_mph : Evd.evar_map -> EConstr.t -> t -> t * morphismT option =
  fun sigma mph store ->
  match EConstr.kind sigma mph with
  | App (p, [| src; dst |]) ->
    begin match EConstr.kind sigma p with
      | Proj (p,arg) when Env.is_projection p Env.is_cat "morphism" ->
        let (srcId,store) = get_elem sigma arg src store in
        let (dstId,store) = get_elem sigma arg dst store in
        let src = store.elems.(srcId) in
        let dst = store.elems.(dstId) in
        let cat = src.category in
        (store, Some { category = cat; src = src; dst = dst; obj = mph })
      | _ -> (store,None)
    end
  | _ -> (store,None)

let parse_mph  = fun sigma name mph store ->
  let (store,mph) = read_mph sigma mph store in
  match mph with
  | Some tp ->
    let mph = { obj = EConstr.mkVar name; tp = tp } in
    let (id,store) = get_mph sigma mph store in (store,Some id)
  | _ -> (store,None)

let read_face = fun sigma fce store ->
  match EConstr.kind sigma fce with
  | App (eq, [| mph; f1; f2 |]) ->
    begin match EConstr.kind sigma eq with
      | Ind (eq,_) when Env.is_eq eq ->
        let (store,tp) = read_mph sigma mph store in
        begin match tp with
          | Some tp ->
            let mph1 = { obj = f1; tp = tp } in
            let (d1,p1,store) = normalize sigma mph1 store in
            let side1 = { mph = mph1; eq = p1; path = d1 } in
            let mph2 = { obj = f2; tp = tp } in
            let (d2,p2,store) = normalize sigma mph2 store in
            let side2 = { mph = mph2; eq = p2; path = d2 } in
            (store, Some (side1,side2))
          | _ -> (store,None)
        end
      | _ -> (store,None)
    end
  | _ -> (store,None)

let parse_face = fun sigma name fce store ->
  match EConstr.kind sigma fce with
  | App (eq, [| mph; f1; f2 |]) ->
    begin match EConstr.kind sigma eq with
      | Ind (eq,_) when Env.is_eq eq ->
        let (store,tp) = read_mph sigma mph store in
        begin match tp with
          | Some tp ->
            let (id,store) = get_face sigma tp f1 f2 (EConstr.mkVar name) store in (store,Some id)
          | _ -> (store,None)
        end
      | _ -> (store,None)
    end
  | _ -> (store,None)
