
type t = EConstr.t

module M = struct
  (* A reader monad over Proofview.tactic giving access to the goal environment *)
  type 'a m = { runEnv : Environ.env -> 'a Proofview.tactic }
  let bind a f =
    { runEnv = fun env -> let a = a.runEnv env in Proofview.tclBIND a (fun x -> (f x).runEnv env) }
  let return x =
    { runEnv = fun env -> Proofview.tclUNIT x }

  let env () = { runEnv = fun env -> Proofview.tclUNIT env }
  let lift a = { runEnv = fun env -> a }
  let run env a = a.runEnv env
end
let lift_tactic = M.lift

module St = Hyps.Make(M)
open St.Combinators
type 'a m = ('a,EConstr.t) St.t
let liftP a = lift (M.lift a)
let none () = ret None
let some x = ret (Some x)
let env () = lift (M.env ())
let evars () = liftP Proofview.tclEVARMAP

let getType (e : EConstr.t) : EConstr.t m =
  let* env = env () in 
  let* sigma = evars () in 
  let (sigma,tp) = Typing.type_of env sigma e in 
  let* _ = liftP (Proofview.Unsafe.tclEVARS sigma) in 
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
  let* cat = St.registerCategory ~cat:(Data.Ctx cat) in 
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
  let* funct = St.registerFunctor ~funct:(Data.Ctx funct) ~src ~dst in 
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
  let* elem = St.registerElem ~elem:(Data.Ctx elem) ~cat in 
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
  let* mph = St.registerMorphism ~mph:(Data.Ctx mph) ~cat ~src ~dst in 
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
  let* eq = St.registerEq ~eq:(Data.Ctx eq) ~right ~left ~cat ~src ~dst in 
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

let parseEqGoal goal =
  let* env = env () in
  let* sigma = evars () in
  let goal = Reductionops.nf_all env sigma goal in
  let* goal = parseEqType goal in 
  match goal with
  | Some (right,left,cat,src,dst) ->
      let* cat = parseCategoryTerm cat in 
      let* src = parseElemTerm src cat in 
      let* dst = parseElemTerm dst cat in 
      let* right = parseMorphismTerm right cat src dst in
      let* left = parseMorphismTerm left cat src dst in
      some (left,right)
  | _ -> none ()



(*  ___                       _   _         *)
(* | _ \_ _ ___ _ __  ___ _ _| |_(_)___ ___ *)
(* |  _/ '_/ _ \ '_ \/ -_) '_|  _| / -_|_-< *)
(* |_| |_| \___/ .__/\___|_|  \__|_\___/__/ *)
(*             |_|                          *)
(* Properties *)

type propType = Mono
              | Epi
              | Iso
              | Nothing

let parsePropType sigma (prop : EConstr.t) =
  match EConstr.kind sigma prop with
  | Const (prop,_) ->
      if Env.is_epi prop then Epi
      else if Env.is_mono prop then Mono
      else Nothing
  | Ind (prop,_) ->
      if Env.is_iso prop then Iso else Nothing
  | _ -> Nothing

let parseProperties hyp prop =
  let* sigma = evars () in 
  match EConstr.kind sigma prop with
  | App (prop, [| catEC; srcEC; dstEC; mphEC |]) when parsePropType sigma prop <> Nothing ->
      let* cat = parseCategoryTerm catEC in
      let* src = parseElemTerm srcEC cat in
      let* dst = parseElemTerm dstEC cat in
      let* mph = parseMorphismTerm mphEC cat src dst in
      let* mphData =
        match mph with
        | AtomicMorphism dt -> ret dt
        | _ ->
            let* mphAtom = St.registerMorphism ~mph:(Data.Ctx mphEC) ~cat ~src ~dst in
            let* tp = liftP (Env.app (Env.mk_mphT ()) [| catEC; srcEC; dstEC |]) in
            let* eq = liftP (Env.app (Env.mk_refl ()) [| tp; mphEC |]) in
            let* _ = registerAtomEq eq (AtomicMorphism mphAtom) mph cat src dst in
            ret mphAtom
      in begin match parsePropType sigma prop with
      | Epi -> mphData.epi <- Some hyp; ret ()
      | Mono -> mphData.mono <- Some hyp; ret ()
      | Iso ->
          let* inv = liftP (Env.app (Env.mk_inv_mph ()) [| catEC; srcEC; dstEC; mphEC; hyp |]) in
          let* inv = St.registerMorphism ~mph:(Data.Ctx inv) ~cat ~src:dst ~dst:src in
          let iso = 
            { Data.iso_obj = hyp
            ; Data.iso_mph = mphData
            ; Data.iso_inv = inv } in
          mphData.iso <- Some iso;
          inv.iso <- Some iso;
          ret ()
      | Nothing -> ret ()
      end
  | _ -> ret ()

(*  ____            _ _          _   _              *)
(* |  _ \ ___  __ _| (_)______ _| |_(_) ___  _ __   *)
(* | |_) / _ \/ _` | | |_  / _` | __| |/ _ \| '_ \  *)
(* |  _ <  __/ (_| | | |/ / (_| | |_| | (_) | | | | *)
(* |_| \_\___|\__,_|_|_/___\__,_|\__|_|\___/|_| |_| *)
(*                                                  *)
(* Realization *)

let (let$) = M.bind
let pret = M.return
let app f args = M.lift (Env.app f args)

(* Create a new evar of type tp *)
let realizeEvar tp =
  let$ sigma = M.lift Proofview.tclEVARMAP in
  let$ env = M.env () in
  let (sigma,evar) = Evarutil.new_evar env sigma tp in
  let$ _ = M.lift (Proofview.Unsafe.tclEVARS sigma) in
  pret evar

let realizeAtomic a =
  let open Data in
  match a with
  | Ctx h -> pret h
  | Evar (_,Some e) -> pret e
  | Evar _ -> assert false (* Should never happen *)

let realizeCategory cat =
  let open Data in
  match cat with
  | AtomicCategory cat -> realizeAtomic cat.cat_obj

let realizeFunctor funct =
  let open Data in
  match funct with
  | AtomicFunctor funct -> realizeAtomic funct.funct_obj

let rec realizeElem elem =
  let open Data in 
  match elem with
  | AtomicElem elem -> realizeAtomic elem.elem_obj
  | FObj (funct,elem) ->
      let$ src = realizeCategory (funct_src funct) in
      let$ dst = realizeCategory (funct_dst funct) in
      let$ funct = realizeFunctor funct in 
      let$ elem = realizeElem elem in
      app 
        (Env.mk_funct_obj ())
        [| src; dst; funct; elem |]

let rec realizeMorphism mph =
  let open Data in 
  match mph with
  | AtomicMorphism mph -> realizeAtomic mph.mph_obj
  | Identity elem ->
      let$ cat = realizeCategory (elem_cat elem) in
      let$ elem = realizeElem elem in 
      app (Env.mk_id ()) [| cat; elem |]
  | Comp (m1,m2) ->
      let$ cat = realizeCategory (morphism_cat m1) in 
      let$ src = realizeElem (morphism_src m1) in
      let$ mid = realizeElem (morphism_dst m1) in
      let$ dst = realizeElem (morphism_dst m2) in
      let$ m1  = realizeMorphism m1 in 
      let$ m2  = realizeMorphism m2 in
      app (Env.mk_comp ()) [| cat; src; mid; dst; m1; m2 |]
  | Inv (AtomicMorphism m) ->
      begin match m.iso with
      | Some iso ->
          let$ cat = realizeCategory m.mph_cat_ in 
          let$ src = realizeElem m.mph_src_ in 
          let$ dst = realizeElem m.mph_dst_ in 
          let$ mph = realizeAtomic m.mph_obj in
          app (Env.mk_inv ()) [| cat; src; dst; mph; iso.iso_obj |]
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
      app (Env.mk_funct_mph ()) [| cat_src; cat_dst; funct; src; dst; mph |]


let mphT m =
  let open Data in
  let$ cat = realizeCategory (morphism_cat m) in 
  let$ src = realizeElem (morphism_src m) in 
  let$ dst = realizeElem (morphism_dst m) in 
  app (Env.mk_mphT ()) [| cat; src; dst |]

let rec realizeEq eq =
  let open Data in
  match eq with
  | Refl m ->
      let$ tp  = mphT m in
      let$ m   = realizeMorphism m in
      app (Env.mk_refl ()) [| tp; m |]
  | Concat (p1,p2) ->
      let$ tp  = mphT (eq_right p1) in
      let$ left = realizeMorphism (eq_left p1) in 
      let$ mid = realizeMorphism (eq_right p1) in 
      let$ right = realizeMorphism (eq_right p2) in
      let$ p1 = realizeEq p1 in 
      let$ p2 = realizeEq p2 in
      app (Env.mk_concat ()) [| tp; left; mid; right; p1; p2 |]
  | InvEq p ->
      let$ tp = mphT (eq_right p) in
      let$ left = realizeMorphism (eq_left p) in 
      let$ right = realizeMorphism (eq_right p) in
      let$ p = realizeEq p in
      app (Env.mk_inv ()) [| tp; left; right; p |]
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
      app 
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
      app (Env.mk_assoc ()) [| cat; src; mid1; mid2; dst; m1; m2; m3 |]
  | LeftId m ->
      let$ cat = realizeCategory (morphism_cat m) in 
      let$ src = realizeElem (morphism_src m) in 
      let$ dst = realizeElem (morphism_dst m) in 
      let$ m = realizeMorphism m in 
      app (Env.mk_left_id ()) [| cat; src; dst; m |]
  | RightId m ->
      let$ cat = realizeCategory (morphism_cat m) in 
      let$ src = realizeElem (morphism_src m) in 
      let$ dst = realizeElem (morphism_dst m) in 
      let$ m = realizeMorphism m in 
      app (Env.mk_right_id ()) [| cat; src; dst; m |]
  | RAp (p,m) ->
      let$ cat = realizeCategory (eq_cat p) in 
      let$ src = realizeElem (eq_src p) in 
      let$ mid = realizeElem (eq_dst p) in 
      let$ dst = realizeElem (morphism_dst m) in 
      let$ left = realizeMorphism (eq_left p) in 
      let$ right = realizeMorphism (eq_right p) in 
      let$ m = realizeMorphism m in 
      let$ p = realizeEq p in
      app (Env.mk_rap ()) [| cat; src; mid; dst; left; right; m; p |]
  | LAp (m,p) ->
      let$ cat = realizeCategory (eq_cat p) in 
      let$ src = realizeElem (morphism_src m) in 
      let$ mid = realizeElem (eq_src p) in 
      let$ dst = realizeElem (eq_dst p) in 
      let$ left = realizeMorphism (eq_left p) in 
      let$ right = realizeMorphism (eq_right p) in 
      let$ m = realizeMorphism m in 
      let$ p = realizeEq p in
      app (Env.mk_lap ()) [| cat; src; mid; dst; m; left; right; p |]
  | RInv iso ->
      let m = iso.iso_mph in 
      let$ cat = realizeCategory m.mph_cat_ in 
      let$ src = realizeElem m.mph_src_ in 
      let$ dst = realizeElem m.mph_dst_ in 
      let$ m = realizeAtomic m.mph_obj in
      app (Env.mk_right_inv ()) [| cat; src; dst; m; iso.iso_obj |]
  | LInv iso ->
      let m = iso.iso_mph in 
      let$ cat = realizeCategory m.mph_cat_ in 
      let$ src = realizeElem m.mph_src_ in 
      let$ dst = realizeElem m.mph_dst_ in 
      let$ m = realizeAtomic m.mph_obj in
      app (Env.mk_left_inv ()) [| cat; src; dst; m; iso.iso_obj |]
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
  | FId (f,e) ->
      let$ c = realizeCategory (funct_src f) in 
      let$ d = realizeCategory (funct_dst f) in
      let$ f = realizeFunctor f in
      let$ e = realizeElem e in
      app (Env.mk_funct_id ()) [| c; d; f; e |]
  | FComp (f,m1,m2) ->
      let$ c = realizeCategory (funct_src f) in 
      let$ d = realizeCategory (funct_dst f) in
      let$ f = realizeFunctor f in
      let$ x = realizeElem (morphism_src m1) in 
      let$ y = realizeElem (morphism_dst m1) in 
      let$ z = realizeElem (morphism_dst m2) in
      let$ m1 = realizeMorphism m1 in
      let$ m2 = realizeMorphism m2 in
      app (Env.mk_funct_comp ()) [| c; d; f; x; y; z; m1; m2 |]
  | FCtx (f,e) ->
      let$ c = realizeCategory (funct_src f) in 
      let$ d = realizeCategory (funct_dst f) in
      let$ f = realizeFunctor f in
      let$ x = realizeElem (eq_src e) in
      let$ y = realizeElem (eq_dst e) in
      let$ m1 = realizeMorphism (eq_left e) in
      let$ m2 = realizeMorphism (eq_right e) in
      let$ e = realizeEq e in
      app (Env.mk_funct_ctx ()) [| c; d; f; x; y; m1; m2; e |]
  | AtomicEq eq -> realizeAtomic eq.eq_obj



(*  _____                   ____            _  *)
(* |_   _|   _ _ __   ___  |  _ \ ___  __ _| | *)
(*   | || | | | '_ \ / _ \ | |_) / _ \/ _` | | *)
(*   | || |_| | |_) |  __/ |  _ <  __/ (_| | | *)
(*   |_| \__, | .__/ \___| |_| \_\___|\__,_|_| *)
(*       |___/|_|                              *)
let realizeCatType _ = M.lift (Env.mk_cat ())
let realizeFunctType data =
  let open Data in
  let$ src = realizeCategory data.funct_src_ in
  let$ dst = realizeCategory data.funct_dst_ in
  M.lift (Env.app (Env.mk_funct_obj ()) [| src; dst |])
let realizeElemType data =
  let open Data in
  let$ cat = realizeCategory data.elem_cat_ in
  M.lift (Env.app (Env.mk_object ()) [| cat |])
let realizeMphType data =
  let open Data in
  let$ cat = realizeCategory data.mph_cat_ in
  let$ src = realizeElem data.mph_src_ in
  let$ dst = realizeElem data.mph_dst_ in
  M.lift (Env.app (Env.mk_mphT ()) [| cat; src; dst |])
let realizeEqType data =
  let open Data in
  let$ cat = realizeCategory data.eq_cat_ in
  let$ src = realizeElem data.eq_src_ in
  let$ dst = realizeElem data.eq_dst_ in
  let$ left = realizeMorphism data.eq_left_ in
  let$ right = realizeMorphism data.eq_right_ in
  let$ mphT = M.lift (Env.app (Env.mk_mphT ()) [| cat; src; dst |]) in
  M.lift (Env.app (Env.mk_eq ()) [| mphT; left; right |])


(*  _   _ _   _ _      *)
(* | | | | |_(_) |___  *)
(* | | | | __| | / __| *)
(* | |_| | |_| | \__ \ *)
(*  \___/ \__|_|_|___/ *)
(*                     *)
(* Utils *)
let eq e1 e2 = 
  let$ env = M.env () in 
  let$ sigma = M.lift Proofview.tclEVARMAP in
  pret (Reductionops.check_conv env sigma e1 e2)
let print ec =
  let$ ev = M.env () in
  let$ sigma = M.lift Proofview.tclEVARMAP in
  let pp = Printer.pr_econstr_env ev sigma ec in
  M.return (Pp.string_of_ppcmds pp)
let fail msg = msg
  |> Pp.str
  |> Tacticals.tclFAIL 0
  |> M.lift
let message msg = Feedback.msg_info (Pp.str msg); M.return ()
let warning msg = Feedback.msg_warning (Pp.str msg); M.return ()
let env = M.env
let to_econstr x = x
let from_econstr x = x
