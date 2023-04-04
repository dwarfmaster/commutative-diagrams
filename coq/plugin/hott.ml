
type ec = EConstr.t
type 'a m = 'a Hyps.t

open Hyps.Combinators

let getType (e : ec) : ec m =
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

let registerAtomCat (cat : ec) =
  let* cat = Hyps.registerCategory ~cat in 
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
  let* funct = Hyps.registerFunctor ~funct ~src ~dst in 
  ret (Data.AtomicFunctor funct)

let parseFunctorType (funct : ec) : (ec*ec) option m =
  let* env = env () in
  let* sigma = evars () in 
  match EConstr.kind sigma funct with
  | App (funct, [| src; dst |]) when isInd sigma Env.is_functor funct ->
      some (src,dst)
  | _ -> none ()

let parseFunctorTerm (funct : ec) src dst =
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
  let* elem = Hyps.registerElem ~elem ~cat in 
  ret (Data.AtomicElem elem)

let parseElemType (obj : ec) : ec option m =
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
  let* mph = Hyps.registerMorphism ~mph ~cat ~src ~dst in 
  ret (Data.AtomicMorphism mph)

let parseMorphismType (mph : ec) : (ec * ec * ec) option m =
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
  let* eq = Hyps.registerEq ~eq ~right ~left ~cat ~src ~dst in 
  ret (Data.AtomicEq eq)

let parseEqType (eq : ec) : (ec * ec * ec * ec * ec) option m =
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
            let* mphAtom = Hyps.registerMorphism ~mph:mphEC ~cat ~src ~dst in
            let* tp = lift (Env.app (Env.mk_mphT ()) [| catEC; srcEC; dstEC |]) in
            let* eq = lift (Env.app (Env.mk_refl ()) [| tp; mphEC |]) in
            let* _ = registerAtomEq eq (AtomicMorphism mphAtom) mph cat src dst in
            ret mphAtom
      in begin match parsePropType sigma prop with
      | Epi -> mphData.epi <- Some hyp; ret ()
      | Mono -> mphData.mono <- Some hyp; ret ()
      | Iso ->
          let* inv = lift (Env.app (Env.mk_inv_mph ()) [| catEC; srcEC; dstEC; mphEC; hyp |]) in
          let* inv = Hyps.registerMorphism ~mph:inv ~cat ~src:dst ~dst:src in
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


(*    _   ___ ___  *)
(*   /_\ | _ \_ _| *)
(*  / _ \|  _/| |  *)
(* /_/ \_\_| |___| *)
(*                 *)
(* API *)

type parsedType =
  | CategoryT
  | FunctorT of Data.category * Data.category
  | ElemT of Data.category
  | MorphismT of Data.category * Data.elem * Data.elem
  | EqT of Data.category * Data.elem * Data.elem * Data.morphism * Data.morphism
type parsed =
  | Category of Data.category
  | Functor of Data.funct
  | Elem of Data.elem
  | Morphism of Data.morphism
  | Equality of Data.eq
  | Prod of Names.Name.t * parsedType * parsed
  | Exists of Names.Name.t * parsedType * parsed

let parse term tp =
  let* () = parseProperties term tp in
  let* is_cat = parseCategory term tp in
  match is_cat with
  | Some c -> some (Category c)
  | None ->
      let* is_funct = parseFunctor term tp in
      match is_funct with
      | Some f -> some (Functor f)
      | None ->
          let* is_elem = parseElem term tp in
          match is_elem with
          | Some e -> some (Elem e)
          | None ->
              let* is_mph = parseMorphism term tp in
              match is_mph with
              | Some m -> some (Morphism m)
              | None ->
                  let* is_eq = parseEq term tp in
                  match is_eq with
                  | Some e -> some (Equality e)
                  | None -> none ()
and parseType tp =
  let* is_cat = parseCategoryType tp in
  if is_cat then some CategoryT
  else
    let* is_funct = parseFunctorType tp in
    match is_funct with
    | Some (src,dst) ->
        let* src = parseCategoryTerm src in
        let* dst = parseCategoryTerm dst in
        some (FunctorT (src,dst))
    | None ->
        let* is_elem = parseElemType tp in
        match is_elem with
        | Some cat ->
            let* cat = parseCategoryTerm cat in
            some (ElemT cat)
        | None ->
            let* is_mph = parseMorphismType tp in
            match is_mph with
            | Some (cat,src,dst) ->
                let* cat = parseCategoryTerm cat in
                let* src = parseElemTerm src cat in
                let* dst = parseElemTerm dst cat in
                some (MorphismT (cat,src,dst))
            | None ->
                let* is_eq = parseEqType tp in
                match is_eq with
                | Some (right,left,cat,src,dst) ->
                    let* cat = parseCategoryTerm cat in
                    let* src = parseElemTerm src cat in
                    let* dst = parseElemTerm dst cat in
                    let* left = parseMorphismTerm left cat src dst in
                    let* right = parseMorphismTerm right cat src dst in
                    some (EqT (cat,src,dst,left,right))
                | None ->
                    none ()

(*  ____            _ _          _   _              *)
(* |  _ \ ___  __ _| (_)______ _| |_(_) ___  _ __   *)
(* | |_) / _ \/ _` | | |_  / _` | __| |/ _ \| '_ \  *)
(* |  _ <  __/ (_| | | |/ / (_| | |_| | (_) | | | | *)
(* |_| \_\___|\__,_|_|_/___\__,_|\__|_|\___/|_| |_| *)
(*                                                  *)
(* Realization *)

let app f args = lift (Env.app f args)

(* Create a new evar of type tp *)
let realizeEvar tp =
  let* sigma = evars () in
  let* env = env () in
  let (sigma,evar) = Evarutil.new_evar env sigma tp in
  let* _ = lift (Proofview.Unsafe.tclEVARS sigma) in
  ret evar

let rec realizeAtomic a : EConstr.t m =
  let open Data in
  match a with
  | Ctx (_,h) -> ret h
  | Evar (_,Some e) -> ret e
  | Evar _ -> assert false (* Should never happen *)
  | Cat c -> realizeCategory c
  | Funct f -> realizeFunctor f
  | Elem e -> realizeElem e
  | Mph m -> realizeMorphism m
  | Eq e -> realizeEq e
  | Composed (_,ec,args) ->
      let rec mapM f = function
        | [] -> ret []
        | x :: t ->
            let* x = f x in
            let* t = mapM f t in
            ret (x :: t) in
      let* args = mapM realizeAtomic args in
      let args = Array.of_list args in
      ret (EConstr.mkApp (ec,args))

and realizeCategory cat =
  let open Data in
  match cat with
  | AtomicCategory cat -> realizeAtomic cat.cat_atom

and realizeFunctor funct =
  let open Data in
  match funct with
  | AtomicFunctor funct -> realizeAtomic funct.funct_atom

and realizeElem elem =
  let open Data in 
  match elem with
  | AtomicElem elem -> realizeAtomic elem.elem_atom
  | FObj (funct,elem) ->
      let* src = realizeCategory (funct_src funct) in
      let* dst = realizeCategory (funct_dst funct) in
      let* funct = realizeFunctor funct in 
      let* elem = realizeElem elem in
      app 
        (Env.mk_funct_obj ())
        [| src; dst; funct; elem |]

and realizeMorphism mph =
  let open Data in 
  match mph with
  | AtomicMorphism mph -> realizeAtomic mph.mph_atom
  | Identity elem ->
      let* cat = realizeCategory (elem_cat elem) in
      let* elem = realizeElem elem in 
      app (Env.mk_id ()) [| cat; elem |]
  | Comp (m1,m2) ->
      let* cat = realizeCategory (morphism_cat m1) in 
      let* src = realizeElem (morphism_src m1) in
      let* mid = realizeElem (morphism_dst m1) in
      let* dst = realizeElem (morphism_dst m2) in
      let* m1  = realizeMorphism m1 in 
      let* m2  = realizeMorphism m2 in
      app (Env.mk_comp ()) [| cat; src; mid; dst; m1; m2 |]
  | Inv (AtomicMorphism m) ->
      begin match m.iso with
      | Some iso ->
          let* cat = realizeCategory m.mph_cat_ in 
          let* src = realizeElem m.mph_src_ in 
          let* dst = realizeElem m.mph_dst_ in 
          let* mph = realizeAtomic m.mph_atom in
          app (Env.mk_inv ()) [| cat; src; dst; mph; iso.iso_obj |]
      | None -> assert false (* Shouldn't happen *)
      end
  | Inv _ -> assert false (* Not supported yet *)
  | FMph (funct,mph) ->
      let* cat_src = realizeCategory (funct_src funct) in 
      let* cat_dst = realizeCategory (funct_dst funct) in 
      let* funct   = realizeFunctor funct in 
      let* src     = realizeElem (morphism_src mph) in 
      let* dst     = realizeElem (morphism_dst mph) in 
      let* mph     = realizeMorphism mph in
      app (Env.mk_funct_mph ()) [| cat_src; cat_dst; funct; src; dst; mph |]


and mphT m =
  let open Data in
  let* cat = realizeCategory (morphism_cat m) in 
  let* src = realizeElem (morphism_src m) in 
  let* dst = realizeElem (morphism_dst m) in 
  app (Env.mk_mphT ()) [| cat; src; dst |]

and realizeEq eq =
  let open Data in
  match eq with
  | Refl m ->
      let* tp  = mphT m in
      let* m   = realizeMorphism m in
      app (Env.mk_refl ()) [| tp; m |]
  | Concat (p1,p2) ->
      let* tp  = mphT (eq_right p1) in
      let* left = realizeMorphism (eq_left p1) in 
      let* mid = realizeMorphism (eq_right p1) in 
      let* right = realizeMorphism (eq_right p2) in
      let* p1 = realizeEq p1 in 
      let* p2 = realizeEq p2 in
      app (Env.mk_concat ()) [| tp; left; mid; right; p1; p2 |]
  | InvEq p ->
      let* tp = mphT (eq_right p) in
      let* left = realizeMorphism (eq_left p) in 
      let* right = realizeMorphism (eq_right p) in
      let* p = realizeEq p in
      app (Env.mk_inv ()) [| tp; left; right; p |]
  | Compose (p1,p2) ->
      let* cat = realizeCategory (eq_cat p1) in
      let* src = realizeElem (eq_src p1) in 
      let* mid = realizeElem (eq_dst p1) in 
      let* dst = realizeElem (eq_dst p2) in
      let* m1l = realizeMorphism (eq_left p1) in 
      let* m1r = realizeMorphism (eq_right p1) in
      let* m2l = realizeMorphism (eq_left p2) in 
      let* m2r = realizeMorphism (eq_right p2) in
      let* p1  = realizeEq p1 in 
      let* p2  = realizeEq p2 in
      app 
        (Env.mk_compose_eq ())
        [| cat; src; mid; dst; m1l; m1r; m2l; m2r; p1; p2 |]
  | Assoc (m1,m2,m3) ->
      let* cat = realizeCategory (morphism_cat m1) in
      let* src = realizeElem (morphism_src m1) in 
      let* mid1 = realizeElem (morphism_dst m1) in 
      let* mid2 = realizeElem (morphism_dst m2) in 
      let* dst = realizeElem (morphism_dst m3) in
      let* m1 = realizeMorphism m1 in
      let* m2 = realizeMorphism m2 in
      let* m3 = realizeMorphism m3 in
      app (Env.mk_assoc ()) [| cat; src; mid1; mid2; dst; m1; m2; m3 |]
  | LeftId m ->
      let* cat = realizeCategory (morphism_cat m) in 
      let* src = realizeElem (morphism_src m) in 
      let* dst = realizeElem (morphism_dst m) in 
      let* m = realizeMorphism m in 
      app (Env.mk_left_id ()) [| cat; src; dst; m |]
  | RightId m ->
      let* cat = realizeCategory (morphism_cat m) in 
      let* src = realizeElem (morphism_src m) in 
      let* dst = realizeElem (morphism_dst m) in 
      let* m = realizeMorphism m in 
      app (Env.mk_right_id ()) [| cat; src; dst; m |]
  | RAp (p,m) ->
      let* cat = realizeCategory (eq_cat p) in 
      let* src = realizeElem (eq_src p) in 
      let* mid = realizeElem (eq_dst p) in 
      let* dst = realizeElem (morphism_dst m) in 
      let* left = realizeMorphism (eq_left p) in 
      let* right = realizeMorphism (eq_right p) in 
      let* m = realizeMorphism m in 
      let* p = realizeEq p in
      app (Env.mk_rap ()) [| cat; src; mid; dst; left; right; m; p |]
  | LAp (m,p) ->
      let* cat = realizeCategory (eq_cat p) in 
      let* src = realizeElem (morphism_src m) in 
      let* mid = realizeElem (eq_src p) in 
      let* dst = realizeElem (eq_dst p) in 
      let* left = realizeMorphism (eq_left p) in 
      let* right = realizeMorphism (eq_right p) in 
      let* m = realizeMorphism m in 
      let* p = realizeEq p in
      app (Env.mk_lap ()) [| cat; src; mid; dst; m; left; right; p |]
  | RInv iso ->
      let m = iso.iso_mph in 
      let* cat = realizeCategory m.mph_cat_ in 
      let* src = realizeElem m.mph_src_ in 
      let* dst = realizeElem m.mph_dst_ in 
      let* m = realizeAtomic m.mph_atom in
      app (Env.mk_right_inv ()) [| cat; src; dst; m; iso.iso_obj |]
  | LInv iso ->
      let m = iso.iso_mph in 
      let* cat = realizeCategory m.mph_cat_ in 
      let* src = realizeElem m.mph_src_ in 
      let* dst = realizeElem m.mph_dst_ in 
      let* m = realizeAtomic m.mph_atom in
      app (Env.mk_left_inv ()) [| cat; src; dst; m; iso.iso_obj |]
  | Mono (ec,m1,m2,p) ->
      let* src = realizeElem (eq_src p) in
      let* m1  = realizeMorphism m1 in 
      let* m2  = realizeMorphism m2 in 
      let* p   = realizeEq p in
      ret (EConstr.mkApp (ec, [| src; m1; m2; p |]))
  | Epi (ec,m1,m2,p) ->
      let* dst = realizeElem (eq_dst p) in
      let* m1  = realizeMorphism m1 in 
      let* m2  = realizeMorphism m2 in 
      let* p   = realizeEq p in
      ret (EConstr.mkApp (ec, [| dst; m1; m2; p |]))
  | FId (f,e) ->
      let* c = realizeCategory (funct_src f) in 
      let* d = realizeCategory (funct_dst f) in
      let* f = realizeFunctor f in
      let* e = realizeElem e in
      app (Env.mk_funct_id ()) [| c; d; f; e |]
  | FComp (f,m1,m2) ->
      let* c = realizeCategory (funct_src f) in 
      let* d = realizeCategory (funct_dst f) in
      let* f = realizeFunctor f in
      let* x = realizeElem (morphism_src m1) in 
      let* y = realizeElem (morphism_dst m1) in 
      let* z = realizeElem (morphism_dst m2) in
      let* m1 = realizeMorphism m1 in
      let* m2 = realizeMorphism m2 in
      app (Env.mk_funct_comp ()) [| c; d; f; x; y; z; m1; m2 |]
  | FCtx (f,e) ->
      let* c = realizeCategory (funct_src f) in 
      let* d = realizeCategory (funct_dst f) in
      let* f = realizeFunctor f in
      let* x = realizeElem (eq_src e) in
      let* y = realizeElem (eq_dst e) in
      let* m1 = realizeMorphism (eq_left e) in
      let* m2 = realizeMorphism (eq_right e) in
      let* e = realizeEq e in
      app (Env.mk_funct_ctx ()) [| c; d; f; x; y; m1; m2; e |]
  | AtomicEq eq -> realizeAtomic eq.eq_atom



(*  _____                   ____            _  *)
(* |_   _|   _ _ __   ___  |  _ \ ___  __ _| | *)
(*   | || | | | '_ \ / _ \ | |_) / _ \/ _` | | *)
(*   | || |_| | |_) |  __/ |  _ <  __/ (_| | | *)
(*   |_| \__, | .__/ \___| |_| \_\___|\__,_|_| *)
(*       |___/|_|                              *)
let realizeCatType _ = lift (Env.mk_cat ())
let realizeFunctType data =
  let open Data in
  let* src = realizeCategory data.funct_src_ in
  let* dst = realizeCategory data.funct_dst_ in
  lift (Env.app (Env.mk_funct_obj ()) [| src; dst |])
let realizeElemType data =
  let open Data in
  let* cat = realizeCategory data.elem_cat_ in
  lift (Env.app (Env.mk_object ()) [| cat |])
let realizeMphType data =
  let open Data in
  let* cat = realizeCategory data.mph_cat_ in
  let* src = realizeElem data.mph_src_ in
  let* dst = realizeElem data.mph_dst_ in
  lift (Env.app (Env.mk_mphT ()) [| cat; src; dst |])
let realizeEqType data =
  let open Data in
  let* cat = realizeCategory data.eq_cat_ in
  let* src = realizeElem data.eq_src_ in
  let* dst = realizeElem data.eq_dst_ in
  let* left = realizeMorphism data.eq_left_ in
  let* right = realizeMorphism data.eq_right_ in
  let* mphT = lift (Env.app (Env.mk_mphT ()) [| cat; src; dst |]) in
  lift (Env.app (Env.mk_eq ()) [| mphT; left; right |])


