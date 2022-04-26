
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

type eqT =
  | Refl of morphismData
  | Concat of eq * eq
  | Inv of eq
  | Compose of eq * eq
  | Assoc of morphismData * morphismData * morphismData
  | LeftId of morphismData
  | RightId of morphismData
  | Atom of EConstr.t
and eq =
  { src : morphismData
  ; dst : morphismData
  ; tp  : morphismT
  ; eq  : eqT
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
let (let*) = Proofview.tclBIND
let (>>=) = Proofview.tclBIND
let (@<<) : ('a -> 'b Proofview.tactic) -> 'a Proofview.tactic -> 'b Proofview.tactic =
  fun f x -> Proofview.tclBIND x f
let ret = Proofview.tclUNIT


(*  __  __                  _     _ *)
(* |  \/  | ___  _ __ _ __ | |__ (_)___ _ __ ___  ___ *)
(* | |\/| |/ _ \| '__| '_ \| '_ \| / __| '_ ` _ \/ __| *)
(* | |  | | (_) | |  | |_) | | | | \__ \ | | | | \__ \ *)
(* |_|  |_|\___/|_|  | .__/|_| |_|_|___/_| |_| |_|___/ *)
(*                   |_| *)

let mphT = fun cat e1 e2 ->
  Env.app (Env.mk_mphT ()) [| cat; e1; e2 |]
let composeT = fun (mT1 : morphismT) (mT2 : morphismT) ->
  let* obj = mphT mT1.category.obj mT1.src.obj mT2.dst.obj in
  ret { category = mT1.category
      ; src = mT1.src
      ; dst = mT2.dst
      ; obj = obj
      }
let compose = fun (m1 : morphismData) (m2 : morphismData) ->
  let* obj = Env.app (Env.mk_comp ()) [| m1.tp.category.obj
                                       ; m1.tp.src.obj; m1.tp.dst.obj; m2.tp.dst.obj
                                       ; m1.obj; m2.obj |] in
  let* tp = composeT m1.tp m2.tp in
  ret { obj = obj; tp = tp }
let identity = fun (x : elem) ->
  let* obj = Env.app (Env.mk_id ()) [| x.category.obj; x.obj |] in
  let* tp = mphT x.category.obj x.obj x.obj in
  ret { obj = obj
      ; tp = { category = x.category
             ; src = x
             ; dst = x
             ; obj = tp }
      }
let rec realize' = fun src (ms : morphismData list) ->
  match ms with
  | [] -> identity src
  | [ m ] -> ret m
  | m :: ms -> compose m @<< realize' m.tp.dst ms
let realize = fun src ms -> realize' src ms
let rpath = fun pth -> realize pth.mph.tp.src (extract pth.path)




(*  _____                  _ _ _          *)
(* | ____|__ _ _   _  __ _| (_) |_ _   _  *)
(* |  _| / _` | | | |/ _` | | | __| | | | *)
(* | |__| (_| | |_| | (_| | | | |_| |_| | *)
(* |_____\__, |\__,_|\__,_|_|_|\__|\__, | *)
(*          |_|                    |___/  *)

let eqT = fun (m1 : morphismData) (m2 : morphismData) ->
  Env.app (Env.mk_eq ()) [| m1.tp.obj; m1.obj; m2.obj |]

let refl = fun (m : morphismData) ->
  ret { src = m
      ; dst = m
      ; tp  = m.tp
      ; eq  = Refl m }

let concat = fun (p1 : eq) (p2 : eq) ->
  ret { src = p1.src
      ; dst = p2.dst
      ; tp  = p1.tp
      ; eq  = Concat (p1,p2) }

let inv = fun (p : eq) ->
  ret { src = p.dst
      ; dst = p.src
      ; tp  = p.tp
      ; eq  = Inv p }

let composeP = fun p1 p2 ->
  let* src = compose p1.src p2.src in
  let* dst = compose p1.dst p2.dst in
  let* tp  = composeT p1.tp p2.tp  in
  ret { src = src; dst = dst; tp = tp; eq = Compose (p1,p2) }

let assoc = fun m1 m2 m3 ->
  let* src = compose m1 m2 >>= (fun m12 -> compose m12 m3) in
  let* dst = compose m2 m3 >>= (fun m23 -> compose m1 m23) in
  let* tp  = composeT m1.tp m2.tp >>= (fun mT12 -> composeT mT12 m3.tp) in
  ret { src = src; dst = dst; tp = tp; eq = Assoc (m1,m2,m3) }

let left_id = fun (m : morphismData) ->
  let* src = identity m.tp.dst >>= (fun id -> compose m id) in
  ret { src = src
      ; dst = m
      ; tp  = m.tp
      ; eq  = LeftId m }

let right_id = fun (m : morphismData) ->
  let* src = identity m.tp.dst >>= (fun id -> compose id m) in
  ret { src = src
      ; dst = m
      ; tp  = m.tp
      ; eq  = RightId m }

let atom_eq = fun ec -> Atom ec

let rec simplify_eqT : bool -> eq -> eqT =
  fun inv eq ->
  match eq.eq with
  | Concat (p1,p2) ->
    let p1 = simplify_eq inv p1 in
    let p2 = simplify_eq inv p2 in
    begin
      match p1.eq, p2.eq with
      | Refl _, _ -> p2.eq
      | _, Refl _ -> p1.eq
      | _, _ -> if inv then Concat (p2,p1) else Concat (p1,p2)
    end
  | Inv p -> (simplify_eq (not inv) p).eq
  | Compose (p1,p2) ->
    (* TODO add support for ap and simplify if refl on only one side *)
    let p1 = simplify_eq inv p1 in
    let p2 = simplify_eq inv p2 in
    Compose (p1,p2)
  | Refl m -> Refl m
  | _ -> if inv then Inv eq else eq.eq
and simplify_eq : bool -> eq -> eq = fun inv eq ->
  { tp  = eq.tp
  ; src = if inv then eq.dst else eq.src
  ; dst = if inv then eq.src else eq.dst
  ; eq  = simplify_eqT inv eq
  }

let rec real_eqT : eqT -> EConstr.t Proofview.tactic = function
  | Refl m -> Env.app (Env.mk_refl ()) [| m.tp.obj; m.obj |]
  | Concat (p1,p2) ->
    let* rp1 = real_eqT p1.eq in
    let* rp2 = real_eqT p2.eq in
    Env.app (Env.mk_concat ())
      [| p1.tp.obj; p1.src.obj; p1.dst.obj; p2.dst.obj; rp1; rp2 |]
  | Inv p ->
    let* rp = real_eqT p.eq in
    Env.app (Env.mk_inv ()) [| p.tp.obj; p.src.obj; p.dst.obj; rp |]
  | Compose (p1,p2) ->
    let* rp1 = real_eqT p1.eq in
    let* rp2 = real_eqT p2.eq in
    Env.app (Env.mk_compose_eq ())
      [| p1.tp.category.obj; p1.src.tp.src.obj; p1.src.tp.dst.obj; p2.src.tp.dst.obj
       ; p1.src.obj; p1.dst.obj; p2.src.obj; p2.dst.obj; rp1; rp2 |]
  | Assoc (m1,m2,m3) ->
    Env.app (Env.mk_assoc ())
      [| m1.tp.category.obj
       ; m1.tp.src.obj; m2.tp.src.obj; m3.tp.src.obj; m3.tp.dst.obj
       ; m1.obj; m2.obj; m3.obj |]
  | LeftId m ->
    Env.app (Env.mk_left_id ())
      [| m.tp.category.obj; m.tp.src.obj; m.tp.dst.obj; m.obj |]
  | RightId m ->
    Env.app (Env.mk_right_id ())
      [| m.tp.category.obj; m.tp.src.obj; m.tp.dst.obj; m.obj |]
  | Atom eq -> ret eq

let real_eq = fun (eq : eq) -> real_eqT (simplify_eq false eq).eq



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

let get_cat  = fun (cat : EConstr.t) store ->
  let* sigma = Proofview.tclEVARMAP in
  let id = array_find_id (fun(c : category) -> EConstr.eq_constr sigma cat c.obj) store.categories in
  match id with
  | Some id -> ret (id,store)
  | None -> let nid = Array.length store.categories in
    ret (nid,
         { categories =
             Array.append store.categories
               [| { obj = cat; id = Array.length store.categories } |]
         ; elems = store.elems
         ; morphisms = store.morphisms
         ; faces = store.faces })

let get_elem = fun (cat : EConstr.t) elm store ->
  let* sigma = Proofview.tclEVARMAP in
  let* (cid,store) = get_cat cat store in
  let cat = store.categories.(cid) in
  let id = array_find_id (fun(e : elem) -> EConstr.eq_constr sigma elm e.obj) store.elems in
  match id with
  | Some id -> ret (id,store)
  | None -> let nid = Array.length store.elems in
    ret (nid,
         { categories = store.categories
         ; elems = Array.append store.elems [| { obj = elm; id = nid; category = cat } |]
         ; morphisms = store.morphisms
         ; faces = store.faces })

let get_mph  = fun (mph : morphismData) store ->
  let* sigma = Proofview.tclEVARMAP in
  let id = array_find_id (fun(m : morphism) -> EConstr.eq_constr sigma mph.obj m.data.obj) store.morphisms in
  match id with
  | Some id -> ret (id,store)
  | None ->
    let nid = Array.length store.morphisms in
    ret (nid,
         { categories = store.categories
         ; elems = store.elems
         ; morphisms = Array.append store.morphisms [| { data = mph; id = nid } |]
         ; faces = store.faces })


(*  _   _                            _ _           _   _              *)
(* | \ | | ___  _ __ _ __ ___   __ _| (_)___  __ _| |_(_) ___  _ __   *)
(* |  \| |/ _ \| '__| '_ ` _ \ / _` | | / __|/ _` | __| |/ _ \| '_ \  *)
(* | |\  | (_) | |  | | | | | | (_| | | \__ \ (_| | |_| | (_) | | | | *)
(* |_| \_|\___/|_|  |_| |_| |_|\__,_|_|_|___/\__,_|\__|_|\___/|_| |_| *)


(* [ m1 m2 m3 ] -> right -> (right o ((m3 o m2) o m1)) = right o m3 o m2 o m1  *)
(* TODO(optimisation) avoid calling realize at each step *)
let rec repeat_assoc : morphismData list -> morphismData -> eq Proofview.tactic =
  fun mphs right ->
  match mphs with
  | [ ] -> refl right
  | m :: [ ] -> refl @<< compose m right
  | m :: mphs ->
    let* p = repeat_assoc mphs right in
    let* r = refl m in
    let* mphs = realize m.tp.dst mphs in
    let* extract_first = assoc m mphs right in
    concat extract_first @<< composeP r p

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

let rec normalize = fun (m : morphismData) store ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma m.obj with
  | App (cmp, [| src; int; dst; mid; msi |]) ->
    begin match EConstr.kind sigma cmp with
      | Proj (cmp,cat) when Env.is_projection cmp Env.is_cat "compose" ->
        let* (catId,store) = get_cat cat store in
        let* (intId,store) = get_elem cat int store in
        let* obj = mphT m.tp.category.obj src int in
        let msi =
            { obj = msi
            ; tp = { category = m.tp.category
                   ; src = m.tp.src
                   ; dst = store.elems.(intId)
                   ; obj = obj }
            } in
        let* obj = mphT m.tp.category.obj int dst in
        let mid =
            { obj = mid
            ; tp  = { category = m.tp.category
                    ; src = store.elems.(intId)
                    ; dst = m.tp.dst
                    ; obj = obj }
            } in
        let* (d1,p1,store) = normalize msi store in
        let* m1 = realize m.tp.src (extract d1) in
        let* (d2,p2,store) = normalize mid store in
        let* m2 = realize store.elems.(intId) (extract d2) in
        let* p = composeP p1 p2 in
        (match d1,d2 with
         | [], _ -> right_id m2 >>= fun id -> concat p id >>= fun c -> ret (d2, c, store)
         | _, [] -> left_id  m1 >>= fun id -> concat p id >>= fun c -> ret (d1, c, store)
         | _, _ ->
           let* p' = repeat_assoc (extract d1) m2 in
           concat p p' >>= fun c -> ret (List.append d1 d2, c, store))
      | _ ->
        let* (mId,store) = get_mph m store in
        refl m >>= fun eq -> ret ([store.morphisms.(mId)], eq, store)
    end
  | _ when isIdentity sigma m.obj -> refl m >>= fun eq -> ret ([], eq, store)
  | _ ->
    let* (mId,store) = get_mph m store in
    refl m >>= fun r -> ret ([store.morphisms.(mId)], r, store)

let eq_face = fun sigma fce f ->
  match f.obj.eq with
  | Atom eq -> EConstr.eq_constr sigma fce eq
  | _ -> assert false

let get_face = fun tp mph1 mph2 fce store ->
  let* sigma = Proofview.tclEVARMAP in
  let id = array_find_id (eq_face sigma fce) store.faces in
  match id with
  | Some id -> ret (id,store)
  | None ->
    let mph1 = { obj = mph1; tp = tp } in
    let* (d1,p1,store) = normalize mph1 store in
    let mph2 = { obj = mph2; tp = tp } in
    let* (d2,p2,store) = normalize mph2 store in
    let nid = Array.length store.faces in
    ret (nid,
         { categories = store.categories
         ; elems = store.elems
         ; morphisms = store.morphisms
         ; faces = Array.append store.faces
               [| { tp = tp
                  ; side1 = { mph = mph1; eq = p1; path = d1 }
                  ; side2 = { mph = mph2; eq = p2; path = d2 }
                  ; obj = { src = mph1; dst = mph2; tp = tp; eq = Atom fce }
                  ; id = nid } |]
         })


(*  ____                _              *)
(* |  _ \ __ _ _ __ ___(_)_ __   __ _  *)
(* | |_) / _` | '__/ __| | '_ \ / _` | *)
(* |  __/ (_| | |  \__ \ | | | | (_| | *)
(* |_|   \__,_|_|  |___/_|_| |_|\__, | *)
(*                              |___/  *)

let parse_cat  = fun name cat store ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma cat with
  | Ind (ind,_) when Env.is_cat ind ->
    let* (id,store) = get_cat (EConstr.mkVar name) store in ret (store,Some id)
  | _ -> ret (store,None)

let parse_elem = fun name elm store ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma elm with
  | Proj (p,arg) when Env.is_projection p Env.is_cat "object" ->
    let* (id,store) = get_elem arg (EConstr.mkVar name) store in ret (store,Some id)
  | _ -> ret (store,None)

let read_mph : EConstr.t -> t -> (t * morphismT option) Proofview.tactic =
  fun mph store ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma mph with
  | App (p, [| src; dst |]) ->
    begin match EConstr.kind sigma p with
      | Proj (p,arg) when Env.is_projection p Env.is_cat "morphism" ->
        let* (srcId,store) = get_elem arg src store in
        let* (dstId,store) = get_elem arg dst store in
        let src = store.elems.(srcId) in
        let dst = store.elems.(dstId) in
        let cat = src.category in
        ret (store, Some { category = cat; src = src; dst = dst; obj = mph })
      | _ -> ret (store,None)
    end
  | _ -> ret (store,None)

let parse_mph  = fun name mph store ->
  let* (store,mph) = read_mph mph store in
  match mph with
  | Some tp ->
    let mph = { obj = EConstr.mkVar name; tp = tp } in
    let* (id,store) = get_mph mph store in ret (store,Some id)
  | _ -> ret (store,None)

let read_face = fun fce store ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma fce with
  | App (eq, [| mph; f1; f2 |]) ->
    begin match EConstr.kind sigma eq with
      | Ind (eq,_) when Env.is_eq eq ->
        let* (store,tp) = read_mph mph store in
        begin match tp with
          | Some tp ->
            let mph1 = { obj = f1; tp = tp } in
            let* (d1,p1,store) = normalize mph1 store in
            let side1 = { mph = mph1; eq = p1; path = d1 } in
            let mph2 = { obj = f2; tp = tp } in
            let* (d2,p2,store) = normalize mph2 store in
            let side2 = { mph = mph2; eq = p2; path = d2 } in
            ret (store, Some (side1,side2))
          | _ -> ret (store,None)
        end
      | _ -> ret (store,None)
    end
  | _ -> ret (store,None)

let parse_face = fun name fce store ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma fce with
  | App (eq, [| mph; f1; f2 |]) ->
    begin match EConstr.kind sigma eq with
      | Ind (eq,_) when Env.is_eq eq ->
        let* (store,tp) = read_mph mph store in
        begin match tp with
          | Some tp ->
            let* (id,store) = get_face tp f1 f2 (EConstr.mkVar name) store in ret (store,Some id)
          | _ -> ret (store,None)
        end
      | _ -> ret (store,None)
    end
  | _ -> ret (store,None)
