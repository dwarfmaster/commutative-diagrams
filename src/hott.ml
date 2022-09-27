
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




(*  ____            _ _          _   _              *)
(* |  _ \ ___  __ _| (_)______ _| |_(_) ___  _ __   *)
(* | |_) / _ \/ _` | | |_  / _` | __| |/ _ \| '_ \  *)
(* |  _ <  __/ (_| | | |/ / (_| | |_| | (_) | | | | *)
(* |_| \_\___|\__,_|_|_/___\__,_|\__|_|\___/|_| |_| *)
(*                                                  *)
(* Realization *)

let (let$) = Proofview.tclBIND
let pret = Proofview.tclUNIT

let realizeCategory cat =
  let open Data in
  match cat with
  | AtomicCategory cat -> pret cat.cat_obj

let realizeFunctor funct =
  let open Data in
  match funct with
  | AtomicFunctor funct -> pret funct.funct_obj

let rec realizeElem elem =
  let open Data in 
  match elem with
  | AtomicElem elem -> pret elem.elem_obj
  | FObj (funct,elem) ->
      let$ src = realizeCategory (funct_src funct) in
      let$ dst = realizeCategory (funct_dst funct) in
      let$ funct = realizeFunctor funct in 
      let$ elem = realizeElem elem in
      Env.app 
        (Env.mk_funct_obj ())
        [| src; dst; funct; elem |]

let rec realizeMorphism mph =
  let open Data in 
  match mph with
  | AtomicMorphism mph -> pret mph.mph_obj
  | Identity elem ->
      let$ cat = realizeCategory (elem_cat elem) in
      let$ elem = realizeElem elem in 
      Env.app (Env.mk_id ()) [| cat; elem |]
  | Comp (m1,m2) ->
      let$ cat = realizeCategory (morphism_cat m1) in 
      let$ src = realizeElem (morphism_src m1) in
      let$ mid = realizeElem (morphism_dst m1) in
      let$ dst = realizeElem (morphism_dst m2) in
      let$ m1  = realizeMorphism m1 in 
      let$ m2  = realizeMorphism m2 in
      Env.app (Env.mk_comp ()) [| cat; src; mid; dst; m1; m2 |]
  | Inv (AtomicMorphism m) ->
      begin match m.iso with
      | Some iso ->
          let$ cat = realizeCategory m.mph_cat_ in 
          let$ src = realizeElem m.mph_src_ in 
          let$ dst = realizeElem m.mph_dst_ in 
          Env.app (Env.mk_inv ()) [| cat; src; dst; m.mph_obj; iso.iso_obj |]
      | None -> assert false (* Shouldn't happen *)
      end
  | Inv _ -> assert false (* Not supported yet *)
  | FMph (funct,mph) ->
      let$ cat_src = realizeCategory (funct_src funct) in 
      let$ cat_dst = realizeCategory (funct_dst funct) in 
      let$ funct   = realizeFunctor funct in 
      let$ src     = realizeElem (morphism_src mph) in 
      let$ dst     = realizeElem (morphism_dst mph) in 
      let$ mph     = realizeMorphism mph in
      Env.app (Env.mk_funct_mph ()) [| cat_src; cat_dst; funct; src; dst; mph |]


let mphT m =
  let open Data in
  let$ cat = realizeCategory (morphism_cat m) in 
  let$ src = realizeElem (morphism_src m) in 
  let$ dst = realizeElem (morphism_dst m) in 
  Env.app (Env.mk_mphT ()) [| cat; src; dst |]

let rec realizeEq eq =
  let open Data in
  match eq with
  | Refl m ->
      let$ tp  = mphT m in
      let$ m   = realizeMorphism m in
      Env.app (Env.mk_refl ()) [| tp; m |]
  | Concat (p1,p2) ->
      let$ tp  = mphT (eq_right p1) in
      let$ left = realizeMorphism (eq_left p1) in 
      let$ mid = realizeMorphism (eq_right p1) in 
      let$ right = realizeMorphism (eq_right p2) in
      let$ p1 = realizeEq p1 in 
      let$ p2 = realizeEq p2 in
      Env.app (Env.mk_concat ()) [| tp; left; mid; right; p1; p2 |]
  | InvEq p ->
      let$ tp = mphT (eq_right p) in
      let$ left = realizeMorphism (eq_left p) in 
      let$ right = realizeMorphism (eq_right p) in
      let$ p = realizeEq p in
      Env.app (Env.mk_inv ()) [| tp; left; right; p |]
  | Compose (p1,p2) ->
      let$ cat = realizeCategory (eq_cat p1) in
      let$ src = realizeElem (eq_src p1) in 
      let$ mid = realizeElem (eq_dst p1) in 
      let$ dst = realizeElem (eq_dst p2) in
      let$ m1l = realizeMorphism (eq_left p1) in 
      let$ m1r = realizeMorphism (eq_right p1) in
      let$ m2l = realizeMorphism (eq_left p2) in 
      let$ m2r = realizeMorphism (eq_right p2) in
      let$ p1  = realizeEq p1 in 
      let$ p2  = realizeEq p2 in
      Env.app 
        (Env.mk_compose_eq ())
        [| cat; src; mid; dst; m1l; m1r; m2l; m2r; p1; p2 |]
  | Assoc (m1,m2,m3) ->
      let$ cat = realizeCategory (morphism_cat m1) in
      let$ src = realizeElem (morphism_src m1) in 
      let$ mid1 = realizeElem (morphism_dst m1) in 
      let$ mid2 = realizeElem (morphism_dst m2) in 
      let$ dst = realizeElem (morphism_dst m3) in
      let$ m1 = realizeMorphism m1 in
      let$ m2 = realizeMorphism m2 in
      let$ m3 = realizeMorphism m3 in
      Env.app (Env.mk_assoc ()) [| cat; src; mid1; mid2; dst; m1; m2; m3 |]
  | LeftId m ->
      let$ cat = realizeCategory (morphism_cat m) in 
      let$ src = realizeElem (morphism_src m) in 
      let$ dst = realizeElem (morphism_dst m) in 
      let$ m = realizeMorphism m in 
      Env.app (Env.mk_left_id ()) [| cat; src; dst; m |]
  | RightId m ->
      let$ cat = realizeCategory (morphism_cat m) in 
      let$ src = realizeElem (morphism_src m) in 
      let$ dst = realizeElem (morphism_dst m) in 
      let$ m = realizeMorphism m in 
      Env.app (Env.mk_right_id ()) [| cat; src; dst; m |]
  | RAp (p,m) ->
      let$ cat = realizeCategory (eq_cat p) in 
      let$ src = realizeElem (eq_src p) in 
      let$ mid = realizeElem (eq_dst p) in 
      let$ dst = realizeElem (morphism_dst m) in 
      let$ left = realizeMorphism (eq_left p) in 
      let$ right = realizeMorphism (eq_right p) in 
      let$ m = realizeMorphism m in 
      let$ p = realizeEq p in
      Env.app (Env.mk_rap ()) [| cat; src; mid; dst; left; right; m; p |]
  | LAp (m,p) ->
      let$ cat = realizeCategory (eq_cat p) in 
      let$ src = realizeElem (morphism_src m) in 
      let$ mid = realizeElem (eq_src p) in 
      let$ dst = realizeElem (eq_dst p) in 
      let$ left = realizeMorphism (eq_left p) in 
      let$ right = realizeMorphism (eq_right p) in 
      let$ m = realizeMorphism m in 
      let$ p = realizeEq p in
      Env.app (Env.mk_lap ()) [| cat; src; mid; dst; m; left; right; p |]
  | RInv iso ->
      let m = iso.iso_mph in 
      let$ cat = realizeCategory (morphism_cat m) in 
      let$ src = realizeElem (morphism_src m) in 
      let$ dst = realizeElem (morphism_dst m) in 
      let$ m = realizeMorphism m in
      Env.app (Env.mk_right_inv ()) [| cat; src; dst; m; iso.iso_obj |]
  | LInv iso ->
      let m = iso.iso_mph in 
      let$ cat = realizeCategory (morphism_cat m) in 
      let$ src = realizeElem (morphism_src m) in 
      let$ dst = realizeElem (morphism_dst m) in 
      let$ m = realizeMorphism m in
      Env.app (Env.mk_left_inv ()) [| cat; src; dst; m; iso.iso_obj |]
  | Mono (ec,m1,m2,p) ->
      let$ src = realizeElem (eq_src p) in
      let$ m1  = realizeMorphism m1 in 
      let$ m2  = realizeMorphism m2 in 
      let$ p   = realizeEq p in
      pret (EConstr.mkApp (ec, [| src; m1; m2; p |]))
  | Epi (ec,m1,m2,p) ->
      let$ dst = realizeElem (eq_dst p) in
      let$ m1  = realizeMorphism m1 in 
      let$ m2  = realizeMorphism m2 in 
      let$ p   = realizeEq p in
      pret (EConstr.mkApp (ec, [| dst; m1; m2; p |]))
  | AtomicEq eq -> pret eq.eq_obj


(*  _   _ _   _ _      *)
(* | | | | |_(_) |___  *)
(* | | | | __| | / __| *)
(* | |_| | |_| | \__ \ *)
(*  \___/ \__|_|_|___/ *)
(*                     *)
(* Utils *)
let eq e1 e2 = 
  let$ env = Proofview.tclENV in 
  let$ sigma = Proofview.tclEVARMAP in
  pret (Reductionops.check_conv env sigma e1 e2)
