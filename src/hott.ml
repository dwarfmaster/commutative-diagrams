
type t = EConstr.t

let (let*) = Proofview.tclBIND
let ret = Proofview.tclUNIT
let (@<<) : ('a -> 'b Proofview.tactic) -> 'a Proofview.tactic -> 'b Proofview.tactic =
  fun f x -> Proofview.tclBIND x f


(*   ____      _                        _ *)
(*  / ___|__ _| |_ ___  __ _  ___  _ __(_) ___  ___ *)
(* | |   / _` | __/ _ \/ _` |/ _ \| '__| |/ _ \/ __| *)
(* | |__| (_| | ||  __/ (_| | (_) | |  | |  __/\__ \ *)
(*  \____\__,_|\__\___|\__, |\___/|_|  |_|\___||___/ *)
(*                     |___/ *)
(* Categories *)

let is_cat : Environ.env -> t -> bool Proofview.tactic = fun env cat ->
  let* sigma = Proofview.tclEVARMAP in
  ret (match EConstr.kind sigma cat with
      | Ind (ind,_) -> Env.is_cat ind
      | _ -> false)

let is_funct : Environ.env -> t -> (t*t) option Proofview.tactic = fun env funct ->
  let* sigma = Proofview.tclEVARMAP in 
  match EConstr.kind sigma funct with
  | App (funct, [| src; dst |]) ->
      begin match EConstr.kind sigma funct with
      | Ind (funct,_) when Env.is_functor funct -> ret (Some (src,dst))
      | _ -> ret None
      end
  | _ -> ret None

let funct_obj (env : Environ.env) (f : Data.funct) (obj : t) : t Proofview.tactic =
  Env.app (Env.mk_funct_obj ()) [| f.src.obj; f.dst.obj; f.obj; obj |]

let rec realizeElem (e : Data.elem) : EConstr.t Proofview.tactic =
  let* env = Proofview.tclENV in
  match e with
  | Elem e -> ret e.obj
  | FObj (f,e) ->
      let* e = realizeElem e in funct_obj env f e 

let is_object : Environ.env -> t -> t option Proofview.tactic = fun env obj ->
  let* sigma = Proofview.tclEVARMAP in
  ret (match EConstr.kind sigma obj with
      | Proj (p,obj) when Env.is_projection p Env.is_cat "object" -> Some obj
      | _ -> None)

let morphism : Environ.env -> t -> t -> t -> t Proofview.tactic = fun env cat src dst ->
  Env.app (Env.mk_mphT ()) [| cat; src; dst |]

let funct_mph (env : Environ.env) (f : Data.funct) (mph : Data.morphismData) : t Proofview.tactic =
  let* src = realizeElem mph.tp.src in 
  let* dst = realizeElem mph.tp.dst in
  Env.app 
    (Env.mk_funct_mph ())
    [| f.src.obj; f.dst.obj; f.obj; src; dst; mph.obj |]

let is_morphism : Environ.env -> t -> (t*t*t) option Proofview.tactic = fun env mph ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma mph with
  | App (p, [| src; dst |]) ->
    begin match EConstr.kind sigma p with
      | Proj (p,cat) when Env.is_projection p Env.is_cat "morphism" -> ret (Some (cat,src,dst))
      | _ -> ret None
    end
  | _ -> ret None

let compose : Environ.env -> t -> t -> t -> t -> t -> t -> t Proofview.tactic =
  fun env cat src mid dst m1 m2->
  Env.app (Env.mk_comp ())
    [| cat; src; mid; dst; m1; m2 |]
let composeT = fun (mT1 : Data.morphismT) (mT2 : Data.morphismT) ->
  let* env = Proofview.tclENV in
  let* src = realizeElem mT1.src in 
  let* dst = realizeElem mT2.dst in
  let* obj = morphism env mT1.category.obj src dst in
  ret { Data.category = mT1.category
      ; src = mT1.src
      ; dst = mT2.dst
      ; obj = obj
      }
let composeM = fun (m1 : Data.morphismData) (m2 : Data.morphismData) ->
  let* env = Proofview.tclENV in
  let* src = realizeElem m1.tp.src in 
  let* mid = realizeElem m1.tp.dst in 
  let* dst = realizeElem m2.tp.dst in
  let* obj =
    compose env m1.tp.category.obj
      src mid dst m1.obj m2.obj in
  let* tp = composeT m1.tp m2.tp in
  ret { Data.obj = obj; tp = tp }

let parse_compose : Environ.env -> t -> (t*t*t*t*t*t) option Proofview.tactic = fun env mph ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma mph with
  | App (cmp, [| src; int; dst; mid; msi |]) ->
    begin match EConstr.kind sigma cmp with
      | Proj (cmp,cat) when Env.is_projection cmp Env.is_cat "compose" ->
        ret (Some (cat,src,int,dst,msi,mid))
      | _ -> ret None
    end
  | _ -> ret None

let identity : Environ.env -> t -> t -> t Proofview.tactic = fun env cat x ->
  Env.app (Env.mk_id ()) [| cat; x |]
let identityM = fun (x : Data.elem) ->
  let* env = Proofview.tclENV in
  let* xobj = realizeElem x in
  let cat = Data.elemCategory x in
  let* obj = identity env cat.obj xobj in
  let* tp = morphism env cat.obj xobj xobj in
  ret { Data.obj = obj
      ; tp = { category = cat
             ; src = x
             ; dst = x
             ; obj = tp }
      }

let parse_identity : Environ.env -> t -> (t*t) option Proofview.tactic = fun env id ->
  let* sigma = Proofview.tclEVARMAP in
  ret (match EConstr.kind sigma id with
      | App (id, [| cat; elem |]) ->
        begin match EConstr.kind sigma id with
          | Const (name,_) when Env.is_id name -> Some (cat,elem)
          | _ -> None
        end
      | App (id, [| elem |]) ->
        begin match EConstr.kind sigma id with
          | Proj (id,cat) when Env.is_projection id Env.is_cat "identity" ->
            Some (cat,elem)
          | _ -> None
        end
      | _ -> None)


let is_mono : Environ.env -> t -> (t*t*t*t) option Proofview.tactic = fun env mono ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma mono with
  | App (mono, [| cat; src; dst; mph |]) ->
    begin match EConstr.kind sigma mono with
      | Const (mono,_) when Env.is_mono mono -> ret (Some (cat,src,dst,mph))
      | _ -> ret None
    end
  | _ -> ret None

let is_epi  : Environ.env -> t -> (t*t*t*t) option Proofview.tactic = fun env epi ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma epi with
  | App (epi, [| cat; src; dst; mph |]) ->
    begin match EConstr.kind sigma epi with
      | Const (epi,_) when Env.is_epi epi -> ret (Some (cat,src,dst,mph))
      | _ -> ret None
    end
  | _ -> ret None

let is_iso  : Environ.env -> t -> (t*t*t*t) option Proofview.tactic = fun env iso ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma iso with
  | App (iso, [| cat; src; dst; mph |]) ->
    begin match EConstr.kind sigma iso with
      | Ind (iso,_) when Env.is_iso iso -> ret (Some (cat,src,dst,mph))
      | _ -> ret None
    end
  | _ -> ret None

let inverse : Environ.env -> t -> t -> t -> t -> t -> t Proofview.tactic = fun env cat src dst mph iso ->
  Env.app (Env.mk_inv_mph ()) [| cat; src; dst; mph; iso |]

(* Takes the description of a path and gives the associated term *)
let rec realize (ms : Data.pathSkeleton) : Data.morphismData Proofview.tactic =
  match snd ms with
  | [] -> identityM (fst ms)
  | [ m ] -> realizeComp m
  | m :: ms -> let* m = realizeComp m in composeM m @<< realize (m.tp.dst, ms)
and realizeComp (m : (Data.morphismData,Data.pathSkeleton) Data.pathComponent) : Data.morphismData Proofview.tactic =
  match m with
  | Base m -> ret m
  | Functor (f,p) ->
      let* env = Proofview.tclENV in
      let* p = realize p in 
      let* fp = funct_mph env f p in
      assert false (* TODO *)
let realizePath (pth : Data.path) = realize (Data.toSkeleton pth)


(*  ____       _   _          *)
(* |  _ \ __ _| |_| |__  ___  *)
(* | |_) / _` | __| '_ \/ __| *)
(* |  __/ (_| | |_| | | \__ \ *)
(* |_|   \__,_|\__|_| |_|___/ *)
(* Paths *)

let eq : Environ.env -> t -> t -> t -> t Proofview.tactic = fun env tp a b ->
  Env.app (Env.mk_eq ()) [| tp; a; b |]

let is_eq : Environ.env -> t -> (t*t*t) option Proofview.tactic = fun env eq ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma eq with
  | App (eq, [| tp; a; b |]) ->
    begin match EConstr.kind sigma eq with
      | Ind (eq,_) when Env.is_eq eq -> ret (Some (tp,a,b))
      | _ -> ret None
    end
  | _ -> ret None

let rec real_eqT : Data.eqT -> EConstr.t Proofview.tactic = function
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
    let* src = realizeElem p1.tp.src in 
    let* mid = realizeElem p1.tp.dst in 
    let* dst = realizeElem p2.tp.dst in
    Env.app (Env.mk_compose_eq ())
    [| p1.tp.category.obj; src; mid; dst
       ; p1.src.obj; p1.dst.obj; p2.src.obj; p2.dst.obj; rp1; rp2 |]
  | Assoc (m1,m2,m3) ->
    let* src  = realizeElem m1.tp.src in 
    let* mid1 = realizeElem m2.tp.src in 
    let* mid2 = realizeElem m3.tp.src in 
    let* dst  = realizeElem m3.tp.dst in
    Env.app (Env.mk_assoc ())
      [| m1.tp.category.obj; src; mid1; mid2; dst
       ; m1.obj; m2.obj; m3.obj |]
  | LeftId m ->
    let* src = realizeElem m.tp.src in 
    let* dst = realizeElem m.tp.dst in
    Env.app (Env.mk_left_id ())
      [| m.tp.category.obj; src; dst; m.obj |]
  | RightId m ->
    let* src = realizeElem m.tp.src in 
    let* dst = realizeElem m.tp.dst in
    Env.app (Env.mk_right_id ())
      [| m.tp.category.obj; src; dst; m.obj |]
  | RAp (p,m) ->
    let* rp = real_eqT p.eq in
    let* src = realizeElem p.tp.src in 
    let* mid = realizeElem p.tp.dst in 
    let* dst = realizeElem m.tp.dst in
    Env.app (Env.mk_rap ())
      [| m.tp.category.obj; src; mid; dst
       ; p.src.obj; p.dst.obj; m.obj; rp |]
  | LAp (m,p) ->
    let* src = realizeElem m.tp.src in
    let* mid = realizeElem p.tp.src in 
    let* dst = realizeElem p.tp.dst in 
    let* rp = real_eqT p.eq in
    Env.app (Env.mk_lap ())
      [| m.tp.category.obj; src; mid; dst
       ; m.obj; p.src.obj; p.dst.obj; rp |]
  | RInv iso ->
    let mph = iso.mph in
    let* src = realizeElem mph.data.tp.src in 
    let* dst = realizeElem mph.data.tp.dst in 
    Env.app (Env.mk_right_inv ())
      [| mph.data.tp.category.obj; src; dst; mph.data.obj; iso.obj |]
  | LInv iso ->
    let mph = iso.mph in
    let* src = realizeElem mph.data.tp.src in 
    let* dst = realizeElem mph.data.tp.dst in 
    Env.app (Env.mk_left_inv ())
      [| mph.data.tp.category.obj; src; dst; mph.data.obj; iso.obj |]
  | Mono (ec,m1,m2,p) ->
    let* rp = real_eqT p.eq in
    let* src = realizeElem p.tp.src in
    ret (EConstr.mkApp (ec, [| src; m1.obj; m2.obj; rp |]))
  | Epi (ec,m1,m2,p) ->
    let* rp = real_eqT p.eq in
    let* dst = realizeElem p.tp.dst in
    ret (EConstr.mkApp (ec, [| dst; m1.obj; m2.obj; rp |]))
  | Atom eq -> ret eq

let realizeEq = fun (eq : Data.eq) -> real_eqT eq.eq

