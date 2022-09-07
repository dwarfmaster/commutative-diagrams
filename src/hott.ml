
type t = EConstr.t

module M = struct
  type 'a m = 'a Proofview.tactic
  let bind = Proofview.tclBIND
  let return = Proofview.tclUNIT
end

module St = Store.Make(M)
open St.Combinators
type 'a m = ('a,EConstr.t) St.t
let none () = ret None
let some x = ret (Some x)
let env () = lift Proofview.tclENV
let evars () = lift Proofview.tclEVARMAP

let getType (e : EConstr.t) : EConstr.t m =
  let* env = env () in 
  let* sigma = evars () in 
  let (sigma,tp) = Typing.type_of env sigma e in 
  let* _ = lift (Proofview.Unsafe.tclEVARS sigma) in 
  ret tp

let isProj sigma indPred name t =
  match EConstr.kind sigma t with
  | Proj (p,_) -> Env.is_projection p indPred name 
  | _ -> false
let isConst sigma pred t =
  match EConstr.kind sigma t with 
  | Const (name,_) -> pred name
  | _ -> false
let isInd sigma pred t =
  match EConstr.kind sigma t with
  | Ind (ind,_) -> pred ind 
  | _ -> false

(*  ____                _              *)
(* |  _ \ __ _ _ __ ___(_)_ __   __ _  *)
(* | |_) / _` | '__/ __| | '_ \ / _` | *)
(* |  __/ (_| | |  \__ \ | | | | (_| | *)
(* |_|   \__,_|_|  |___/_|_| |_|\__, | *)
(*                              |___/  *)
(* Parsing *)


(*   ___      _                    _         *)
(*  / __|__ _| |_ ___ __ _ ___ _ _(_)___ ___ *)
(* | (__/ _` |  _/ -_) _` / _ \ '_| / -_|_-< *)
(*  \___\__,_|\__\___\__, \___/_| |_\___/__/ *)
(*                   |___/                   *)
(* Categories *)

let registerAtomCat (cat : EConstr.t) =
  let* cat = St.registerCategory ~cat in 
  ret (Data.AtomicCategory cat)

let parseCategoryType (cat : EConstr.t) : bool m =
  let* env = env () in
  let* sigma = evars () in
  ret (isInd sigma Env.is_cat cat)

let parseCategoryTerm (cat : EConstr.t) =
  registerAtomCat cat

let parseCategory (cat : EConstr.t) (tp : EConstr.t) = 
  let* is = parseCategoryType tp in
  if is then some @<< parseCategoryTerm cat else none ()


(*  ___             _               *)
(* | __|  _ _ _  __| |_ ___ _ _ ___ *)
(* | _| || | ' \/ _|  _/ _ \ '_(_-< *)
(* |_| \_,_|_||_\__|\__\___/_| /__/ *)
(*                                  *)
(* Functors *)

let registerAtomFunct funct src dst =
  let* funct = St.registerFunctor ~funct ~src ~dst in 
  ret (Data.AtomicFunctor funct)

let parseFunctorType (funct : t) : (t*t) option m =
  let* env = env () in
  let* sigma = evars () in 
  match EConstr.kind sigma funct with
  | App (funct, [| src; dst |]) when isInd sigma Env.is_functor funct ->
      some (src,dst)
  | _ -> none ()

let parseFunctorTerm (funct : t) src dst =
  registerAtomFunct funct src dst

let parseFunctor (funct : EConstr.t) (tp : EConstr.t) =
  let* is = parseFunctorType tp in 
  match is with 
  | Some (src,dst) ->
      let* src = parseCategoryTerm src in 
      let* dst = parseCategoryTerm dst in 
      some @<< parseFunctorTerm funct src dst
  | None -> none ()



(*  ___ _               *)
(* | __| |___ _ __  ___ *)
(* | _|| / -_) '  \(_-< *)
(* |___|_\___|_|_|_/__/ *)
(*                      *)
(* Elems *)

let registerAtomElem elem cat =
  let* elem = St.registerElem ~elem ~cat in 
  ret (Data.AtomicElem elem)

let parseElemType (obj : t) : t option m =
  let* env = env () in
  let* sigma = evars () in
  match EConstr.kind sigma obj with
  | Proj (p,obj) when Env.is_projection p Env.is_cat "object" -> some obj
  | _ -> none ()

exception AtomElem
let rec parseElemTerm (elem : EConstr.t) cat =
  let* env = env () in 
  let* sigma = evars () in 
  try match EConstr.kind sigma elem with
  | App (fobj, [| elem |]) ->
      begin match EConstr.kind sigma fobj with
      | Proj (fobj,funct) when Env.is_projection fobj Env.is_functor "object_of" ->
          let* tp = parseFunctorType @<< getType funct in
          begin match tp with
          | None -> assert false (* Shouldn't happen *)
          | Some (src,dst) ->
              let* src = parseCategoryTerm src in
              let* dst = parseCategoryTerm dst in 
              let* funct = parseFunctorTerm funct src dst in
              let* elem = parseElemTerm elem src in
              ret (Data.FObj (funct,elem))
          end
      | _ -> raise AtomElem
      end
  | _ -> raise AtomElem
  with AtomElem -> registerAtomElem elem cat

let parseElem (elem : EConstr.t) (tp : EConstr.t) =
  let* is = parseElemType tp in 
  match is with
  | Some cat ->
      let* cat = registerAtomCat cat in 
      some @<< registerAtomElem elem cat
  | None -> none ()



(*  __  __              _    _              *)
(* |  \/  |___ _ _ _ __| |_ (_)____ __  ___ *)
(* | |\/| / _ \ '_| '_ \ ' \| (_-< '  \(_-< *)
(* |_|  |_\___/_| | .__/_||_|_/__/_|_|_/__/ *)
(*                |_|                       *)
(* Morphisms *)

let registerAtomMorphism mph cat src dst =
  let* mph = St.registerMorphism ~mph ~cat ~src ~dst in 
  ret (Data.AtomicMorphism mph)

let parseMorphismType (mph : t) : (t * t * t) option m =
  let* sigma = evars () in
  match EConstr.kind sigma mph with
  | App (p, [| src; dst |]) ->
    begin match EConstr.kind sigma p with
      | Proj (p,cat) when Env.is_projection p Env.is_cat "morphism" ->
          some (cat,src,dst)
      | _ -> none ()
    end
  | _ -> none ()

exception AtomMph
let rec parseMorphismTerm mph cat src dst =
  let* sigma = evars () in
  try match EConstr.kind sigma mph with
  | App (cmp, [| _; int; _; mid; msi |]) ->
    begin match EConstr.kind sigma cmp with
      | Proj (cmp,_) when Env.is_projection cmp Env.is_cat "compose" ->
          let* int = parseElemTerm int cat in
          let* msi = parseMorphismTerm msi cat src int in
          let* mid = parseMorphismTerm mid cat int dst in
          ret (Data.Comp (msi,mid))
      | _ -> raise AtomMph
    end
  | App (funct, [| src; dst; mph |]) ->
      begin match EConstr.kind sigma funct with
      | Proj (mof,funct) when Env.is_projection mof Env.is_functor "morphism_of" ->
          let* tp = parseFunctorType @<< getType funct in 
          begin match tp with
          | None -> assert false (* Shouldn't happen *)
          | Some (src_cat,_) ->
              let* src_cat = parseCategoryTerm src_cat in 
              let* src = parseElemTerm src src_cat in 
              let* dst = parseElemTerm dst src_cat in 
              let* funct = parseFunctorTerm funct src_cat cat in
              let* mph = parseMorphismTerm mph cat src dst in 
              ret (Data.FMph (funct,mph))
          end
      | _ -> raise AtomMph 
      end
  | App (id, [| _; elem |]) ->
      begin match EConstr.kind sigma id with
      | Const (name,_) when Env.is_id name ->
          let* elem = parseElemTerm elem cat in 
          ret (Data.Identity elem)
      | _ -> raise AtomMph
      end 
  | App (id, [| elem |]) ->
      begin match EConstr.kind sigma id with
      | Proj (id,_) when Env.is_projection id Env.is_cat "identity" ->
          let* elem = parseElemTerm elem cat in 
          ret (Data.Identity elem)
      | _ -> raise AtomMph
      end
  | _ -> raise AtomMph
  with AtomMph -> registerAtomMorphism mph cat src dst

let parseMorphism mph tp =
  let* tp = parseMorphismType tp in 
  match tp with
  | None -> none ()
  | Some (cat,src,dst) ->
      let* cat = parseCategoryTerm cat in 
      let* src = parseElemTerm src cat in 
      let* dst = parseElemTerm dst cat in 
      some @<< parseMorphismTerm mph cat src dst



(*  ___                _ _ _   _         *)
(* | __|__ _ _  _ __ _| (_) |_(_)___ ___ *)
(* | _|/ _` | || / _` | | |  _| / -_|_-< *)
(* |___\__, |\_,_\__,_|_|_|\__|_\___/__/ *)
(*        |_|                            *)
(* Equalities *)

let registerAtomEq eq right left cat src dst =
  let* eq = St.registerEq ~eq ~right ~left ~cat ~src ~dst in 
  ret (Data.AtomicEq eq)

let parseEqType (eq : t) : (t * t * t * t * t) option m =
  let* sigma = evars () in
  match EConstr.kind sigma eq with
  | App (eq, [| tp; left; right |]) ->
    begin match EConstr.kind sigma eq with
      | Ind (eq,_) when Env.is_eq eq ->
          let* tp = parseMorphismType tp in 
          begin match tp with
          | Some (cat,src,dst) -> some (right,left,cat,src,dst)
          | None -> none ()
          end
      | _ -> none ()
    end
  | _ -> none ()

(* No need to parse the concrete eq terms *)
let parseEqTerm eq right left cat src dst =
  registerAtomEq eq right left cat src dst

let parseEq eq tp =
  let* tp = parseEqType tp in 
  match tp with
  | Some (right,left,cat,src,dst) ->
      let* cat = parseCategoryTerm cat in 
      let* src = parseElemTerm src cat in 
      let* dst = parseElemTerm dst cat in 
      let* right = parseMorphismTerm right cat src dst in
      let* left = parseMorphismTerm left cat src dst in
      some @<< parseEqTerm eq right left cat src dst
  | _ -> none ()


(* XXXXX *)

(*   ____      _                        _ *)
(*  / ___|__ _| |_ ___  __ _  ___  _ __(_) ___  ___ *)
(* | |   / _` | __/ _ \/ _` |/ _ \| '__| |/ _ \/ __| *)
(* | |__| (_| | ||  __/ (_| | (_) | |  | |  __/\__ \ *)
(*  \____\__,_|\__\___|\__, |\___/|_|  |_|\___||___/ *)
(*                     |___/ *)
(* Categories *)

let funct_obj (env : Environ.env) (f : Data.funct) (obj : t) : t Proofview.tactic =
  Env.app (Env.mk_funct_obj ()) [| f.src.obj; f.dst.obj; f.obj; obj |]

let rec realizeElem (e : Data.elem) : EConstr.t Proofview.tactic =
  let* env = Proofview.tclENV in
  match e with
  | Elem e -> ret e.obj
  | FObj (f,e) ->
      let* e = realizeElem e in funct_obj env f e 

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
      let open Data in
      let* env = Proofview.tclENV in
      let* p = realize p in 
      let* fp = funct_mph env f p in
      let* src = funct_obj env f @<< realizeElem p.tp.src in
      let* dst = funct_obj env f @<< realizeElem p.tp.dst in
      let* tp = morphism env f.dst.obj src dst in
      ret { obj = fp 
          ; tp = { category = f.dst
                 ; src = FObj (f,p.tp.src)
                 ; dst = FObj (f,p.tp.dst)
                 ; obj = tp; }
          }
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

