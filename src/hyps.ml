
open Data

type t =
  { categories : category array
  ; elems      : elem array
  ; morphisms  : morphismBase array
  ; faces      : face array
  }

exception Ill_typed
let (let*) = Proofview.tclBIND
let (>>=) = Proofview.tclBIND
let (@<<) : ('a -> 'b Proofview.tactic) -> 'a Proofview.tactic -> 'b Proofview.tactic =
  fun f x -> Proofview.tclBIND x f
let ret = Proofview.tclUNIT

let extract : morphism list -> morphismData list =
  List.map (fun m -> m.data)

(*  __  __                  _     _ *)
(* |  \/  | ___  _ __ _ __ | |__ (_)___ _ __ ___  ___ *)
(* | |\/| |/ _ \| '__| '_ \| '_ \| / __| '_ ` _ \/ __| *)
(* | |  | | (_) | |  | |_) | | | | \__ \ | | | | \__ \ *)
(* |_|  |_|\___/|_|  | .__/|_| |_|_|___/_| |_| |_|___/ *)
(*                   |_| *)

let composeT = fun (mT1 : morphismT) (mT2 : morphismT) ->
  let* env = Proofview.tclENV in
  let* obj = Hott.morphism env mT1.category.obj mT1.src.obj mT2.dst.obj in
  ret { Data.category = mT1.category
      ; src = mT1.src
      ; dst = mT2.dst
      ; obj = obj
      }
let compose = fun (m1 : morphismData) (m2 : morphismData) ->
  let* env = Proofview.tclENV in
  let* obj =
    Hott.compose env m1.tp.category.obj
      m1.tp.src.obj m1.tp.dst.obj m2.tp.dst.obj
      m1.obj m2.obj in
  let* tp = composeT m1.tp m2.tp in
  ret { Data.obj = obj; tp = tp }
let identity = fun (x : elem) ->
  let* env = Proofview.tclENV in
  let* obj = Hott.identity env x.category.obj x.obj in
  let* tp = Hott.morphism env x.category.obj x.obj x.obj in
  ret { Data.obj = obj
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
  let* env = Proofview.tclENV in
  Hott.eq env m1.tp.obj m1.obj m2.obj

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
  let* src = identity m.tp.src >>= (fun id -> compose id m) in
  ret { src = src
      ; dst = m
      ; tp  = m.tp
      ; eq  = RightId m }

let right_inv = fun (iso : isoData) ->
  let mph = iso.mph in
  let inv = iso.inv in
  let* id = identity mph.data.tp.src  in
  let* c  = compose mph.data inv.data in
  ret { src = c
      ; dst = id
      ; tp  = id.tp
      ; eq  = RInv iso
      }

let left_inv = fun (iso : isoData) ->
  let mph = iso.mph in
  let inv = iso.inv in
  let* id = identity mph.data.tp.dst  in
  let* c  = compose inv.data mph.data in
  ret { src = c
      ; dst = id
      ; tp  = id.tp
      ; eq  = LInv iso
      }

let atom_eq = fun ec -> Atom ec

let mono_eq = fun ec m1 m2 p -> Mono (ec,m1,m2,p)
let epi_eq = fun ec m1 m2 p -> Epi (ec,m1,m2,p)

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
    let p1 = simplify_eq inv p1 in
    let p2 = simplify_eq inv p2 in
    begin
      match p1.eq, p2.eq with
      | Refl _, Refl _ -> Refl eq.src
      | Refl m, _ -> LAp (m,p2)
      | _, Refl m -> RAp (p1,m)
      | _ -> Compose (p1,p2)
    end
  | Refl m -> Refl m
  | RAp (p,m) ->
    let p = simplify_eq inv p in begin
      match p.eq with
      | Refl _ -> Refl eq.src
      | _ -> RAp (p,m)
    end
  | LAp (m,p) ->
    let p = simplify_eq inv p in begin
      match p.eq with
      | Refl _ -> Refl eq.src
      | _ -> LAp (m,p)
    end
  | Mono (ec,m1,m2,p) ->
    let p = simplify_eq inv p in
    if inv
    then Mono (ec,m2,m1,p)
    else Mono (ec,m1,m2,p)
  | Epi (ec,m1,m2,p) ->
    let p = simplify_eq inv p in
    if inv
    then Epi (ec,m2,m1,p)
    else Epi (ec,m1,m2,p)
  | _ -> if inv then Inv eq else eq.eq
and simplify_eq : bool -> eq -> eq = fun inv eq ->
  { tp  = eq.tp
  ; src = if inv then eq.dst else eq.src
  ; dst = if inv then eq.src else eq.dst
  ; eq  = simplify_eqT inv eq
  }

let simpl_eq = simplify_eq false

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

let comp_constr : Environ.env -> Evd.evar_map -> EConstr.t -> EConstr.t -> bool =
  fun env sigma e1 e2 -> Reductionops.check_conv env sigma e1 e2

let get_cat  = fun (cat : EConstr.t) store ->
  let* env = Proofview.tclENV in
  let* sigma = Proofview.tclEVARMAP in
  let id = array_find_id (fun(c : category) -> comp_constr env sigma cat c.obj) store.categories in
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
  let* env = Proofview.tclENV in
  let* sigma = Proofview.tclEVARMAP in
  let* (cid,store) = get_cat cat store in
  let cat = store.categories.(cid) in
  let id = array_find_id (fun(e : elem) -> comp_constr env sigma elm e.obj) store.elems in
  match id with
  | Some id -> ret (id,store)
  | None -> let nid = Array.length store.elems in
    ret (nid,
         { categories = store.categories
         ; elems = Array.append store.elems [| { obj = elm; id = nid; category = cat } |]
         ; morphisms = store.morphisms
         ; faces = store.faces })

let get_mph = fun (mph : morphismData) store ->
  let* env = Proofview.tclENV in
  let* sigma = Proofview.tclEVARMAP in
  let id = array_find_id (fun(m : morphismBase) -> comp_constr env sigma mph.obj m.data.obj) store.morphisms in
  match id with
  | Some id -> ret (id,store)
  | None ->
    let nid = Array.length store.morphisms in
    ret (nid,
         { categories = store.categories
         ; elems = store.elems
         ; morphisms = Array.append store.morphisms [| { data = mph; id = nid; mono = None; epi = None; iso = None } |]
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

let rec normalize = fun (m : morphismData) store ->
  let* sigma = Proofview.tclEVARMAP in
  let* env = Proofview.tclENV in
  let* mph = Hott.parse_compose env m.obj in
  match mph with
  | Some (cat,src,int,dst,msi,mid) ->
    let* (catId,store) = get_cat cat store in
    let* (intId,store) = get_elem cat int store in
    let* obj = Hott.morphism env m.tp.category.obj src int in
    let msi =
      { obj = msi
      ; tp = { category = m.tp.category
             ; src = m.tp.src
             ; dst = store.elems.(intId)
             ; obj = obj }
      } in
    let* obj = Hott.morphism env m.tp.category.obj int dst in
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
    let* id = Hott.parse_identity env m.obj in
    match id with
    | Some _ -> refl m >>= fun eq -> ret ([], eq, store)
    | _ ->
      let* (mId,store) = get_mph m store in
      refl m >>= fun r -> ret ([fromBase store.morphisms.(mId)], r, store)

let eq_face = fun env sigma fce f ->
  match f.obj.eq with
  | Atom eq -> comp_constr env sigma fce eq
  | _ -> assert false

let get_face = fun tp mph1 mph2 fce store ->
  let* env = Proofview.tclENV in
  let* sigma = Proofview.tclEVARMAP in
  let id = array_find_id (eq_face env sigma fce) store.faces in
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
  let* env = Proofview.tclENV in
  let* isc = Hott.is_cat env cat in
  if isc
  then let* (id,store) = get_cat (EConstr.mkVar name) store in ret (store,Some id)
  else ret (store,None)

let parse_elem = fun name elm store ->
  let* env = Proofview.tclENV in
  let* obj = Hott.is_object env elm in
  match obj with
  | Some arg ->
    let* (id,store) = get_elem arg (EConstr.mkVar name) store in ret (store,Some id)
  | _ -> ret (store,None)

let read_mph : EConstr.t -> t -> (t * morphismT option) Proofview.tactic =
  fun mph store ->
  let* env = Proofview.tclENV in
  let* obj = Hott.is_morphism env mph in
  match obj with
  | Some (arg,src,dst) ->
    let* (srcId,store) = get_elem arg src store in
    let* (dstId,store) = get_elem arg dst store in
    let src = store.elems.(srcId) in
    let dst = store.elems.(dstId) in
    let cat = src.category in
    ret (store, Some { category = cat; src = src; dst = dst; obj = mph })
  | _ -> ret (store,None)

let parse_mph  = fun name mph store ->
  let* (store,mph) = read_mph mph store in
  match mph with
  | Some tp ->
    let mph = { obj = EConstr.mkVar name; tp = tp } in
    let* (id,store) = get_mph mph store in ret (store,Some id)
  | _ -> ret (store,None)

let read_face = fun fce store ->
  let* env = Proofview.tclENV in
  let* eq = Hott.is_eq env fce in
  match eq with
  | Some (mph,f1,f2) ->
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

let parse_face = fun name fce store ->
  let* env = Proofview.tclENV in
  let* eq = Hott.is_eq env fce in
  match eq with
  | Some (mph,f1,f2) ->
    let* (store,tp) = read_mph mph store in
    begin match tp with
      | Some tp ->
        let* (id,store) = get_face tp f1 f2 (EConstr.mkVar name) store in ret (store,Some id)
      | _ -> ret (store,None)
    end
  | _ -> ret (store,None)

let parse_mono = fun name mono store ->
  let* env = Proofview.tclENV in
  let* mono = Hott.is_mono env mono in
  match mono with
  | Some (cat,src,dst,mph) ->
    let* (cat,store) = get_cat cat store in
    let cat = store.categories.(cat) in
    let* (src,store) = get_elem cat.obj src store in
    let src = store.elems.(src) in
    let* (dst,store) = get_elem cat.obj dst store in
    let dst = store.elems.(dst) in
    let* tp = Hott.morphism env cat.obj src.obj dst.obj in
    let* (mph,store) =
      get_mph
        { obj = mph
        ; tp = { src = src; dst = dst; category = cat; obj = tp }
        } store in
    let mph = store.morphisms.(mph) in
    mph.mono <- Some (EConstr.mkVar name);
    ret (store,Some mph.id)
  | _ -> ret (store,None)

let parse_epi = fun name epi store ->
  let* env = Proofview.tclENV in
  let* epi = Hott.is_epi env epi in
  match epi with
  | Some (cat,src,dst,mph) ->
    let* (cat,store) = get_cat cat store in
    let cat = store.categories.(cat) in
    let* (src,store) = get_elem cat.obj src store in
    let src = store.elems.(src) in
    let* (dst,store) = get_elem cat.obj dst store in
    let dst = store.elems.(dst) in
    let* tp = Hott.morphism env cat.obj src.obj dst.obj in
    let* (mph,store) =
      get_mph
        { obj = mph
        ; tp = { src = src; dst = dst; category = cat; obj = tp }
        } store in
    let mph = store.morphisms.(mph) in
    mph.epi <- Some (EConstr.mkVar name);
    ret (store,Some mph.id)
  | _ -> ret (store,None)

let parse_iso = fun name iso store ->
  let* sigma = Proofview.tclEVARMAP in
  let* env = Proofview.tclENV in
  let* iso = Hott.is_iso env iso in
  match iso with
  | Some (cat,src,dst,mph) ->
    let* (cat,store) = get_cat cat store in
    let cat = store.categories.(cat) in
    let* (src,store) = get_elem cat.obj src store in
    let src = store.elems.(src) in
    let* (dst,store) = get_elem cat.obj dst store in
    let dst = store.elems.(dst) in
    let* tp = Hott.morphism env cat.obj src.obj dst.obj in
    let* (mph,store) =
      get_mph
        { obj = mph
        ; tp = { src = src; dst = dst; category = cat; obj = tp }
        } store in
    let mph = store.morphisms.(mph) in
    let hypo = EConstr.mkVar name in
    let* inv = Hott.inverse env cat.obj src.obj dst.obj mph.data.obj hypo in
    let* tp = Hott.morphism env cat.obj dst.obj src.obj in
    let* (inv,store) =
      get_mph
        { obj = inv
        ; tp = { src = dst; dst = src; category = cat; obj = tp }
        } store in
    let data =
      { obj = hypo
      ; mph = mph
      ; inv = store.morphisms.(inv)
      } in
    begin
      mph.iso <- Some data;
      store.morphisms.(inv).iso <- Some data;
      ret (store,Some mph.id)
    end
  | _ -> ret (store,None)
