
let cmp2 i1 i2 = if i1 = 0 then i2 else i1
let cmp3 i1 i2 i3 =
  if i1 <> 0 then i1
  else if i2 <> 0 then i2
  else i3

(*  _____                       *)
(* |_   _|   _ _ __   ___  ___  *)
(*   | || | | | '_ \ / _ \/ __| *)
(*   | || |_| | |_) |  __/\__ \ *)
(*   |_| \__, | .__/ \___||___/ *)
(*       |___/|_|               *)
(* Types *)

type fn =
  | FnConst of Names.Constant.t
  | FnVar of Names.Id.t
  | FnInd of Names.inductive
  | FnConstr of Names.constructor
  | FnProj of Names.Projection.t
and atomic =
  (* The id is a common id shared by all types of objects *)
  | Ctx of int * EConstr.t
  | Evar of int * EConstr.t option
  | Fn of int * fn
  | Cat of category
  | Funct of funct
  | Elem of elem
  | Mph of morphism
  | Eq of eq
  | Composed of atomic * atomic list
and categoryData =
  { cat_atom : atomic
  }
and category =
  | AtomicCategory of categoryData
and functData =
  { funct_atom : atomic
  ; funct_src_ : category
  ; funct_dst_ : category
  }
and funct =
  | AtomicFunctor of functData
and elemData =
  { elem_atom : atomic
  ; elem_cat_ : category
  }
and elem =
  | AtomicElem of elemData
  | FObj of funct * elem
and morphismData =
  { mph_atom : atomic
  ; mph_cat_ : category
  ; mph_src_ : elem 
  ; mph_dst_ : elem
  ; mutable mono : EConstr.t option
  ; mutable epi  : EConstr.t option
  ; mutable iso  : isoData option
  }
and isoData =
  { iso_obj : EConstr.t
  ; iso_mph : morphismData
  ; iso_inv : morphismData
  }
and morphism =
  | AtomicMorphism of morphismData
  | Identity of elem
  | Comp of morphism * morphism (* Comp (m1,m2) ~ m2 o m1 *)
  | Inv of morphism
  | FMph of funct * morphism
(* Equality between uninterned morphisms *)
and eq =
  | Refl of morphism
  | Concat of eq * eq
  | InvEq of eq
  | Compose of eq * eq
  | Assoc of morphism * morphism * morphism
  | LeftId of morphism
  | RightId of morphism
  | RAp of eq * morphism
  | LAp of morphism * eq
  | RInv of isoData
  | LInv of isoData
  | Mono of EConstr.t * morphism * morphism * eq
  | Epi of EConstr.t * morphism * morphism * eq
  | FId of funct * elem 
  | FComp of funct * morphism * morphism
  | FCtx of funct * eq
  | AtomicEq of eqData
and eqData =
  { eq_left_  : morphism
  ; eq_right_ : morphism
  ; eq_src_   : elem 
  ; eq_dst_   : elem 
  ; eq_cat_   : category
  ; eq_atom   : atomic
  }



(*     _   _                  _       *)
(*    / \ | |_ ___  _ __ ___ (_) ___  *)
(*   / _ \| __/ _ \| '_ ` _ \| |/ __| *)
(*  / ___ \ || (_) | | | | | | | (__  *)
(* /_/   \_\__\___/|_| |_| |_|_|\___| *)
(*                                    *)
(* Atomic *)
let rec atomic_constructor_id (a: atomic) : int =
  match a with
  | Ctx _ -> 0
  | Evar _ -> 1
  | Fn _ -> 2
  | Cat _ -> 3
  | Funct _ -> 4
  | Elem _ -> 5
  | Mph _ -> 6
  | Eq _ -> 7
  | Composed _ -> 8
and cmp_atomic (a1 : atomic) (a2 : atomic) : int =
  match a1, a2 with
  | Ctx (i1,_), Ctx (i2,_) -> i2 - i1
  | Evar (i1,_), Evar (i2,_) -> i2 - i1
  | Fn (i1,_), Fn (i2,_) -> i2 - i1
  | Cat c1, Cat c2 -> cmp_category c1 c2
  | Funct f1, Funct f2 -> cmp_funct f1 f2
  | Elem e1, Elem e2 -> cmp_elem e1 e2
  | Mph m1, Mph m2 ->  cmp_morphism m1 m2
  | Eq eq1, Eq eq2 -> cmp_eq eq1 eq2
  | Composed (f1,args1), Composed (f2,args2) ->
      cmp2 (cmp_atomic f1 f2) (List.compare cmp_atomic args1 args2)
  | _, _ -> atomic_constructor_id a2 - atomic_constructor_id a1


(*   ____      _                               *)
(*  / ___|__ _| |_ ___  __ _  ___  _ __ _   _  *)
(* | |   / _` | __/ _ \/ _` |/ _ \| '__| | | | *)
(* | |__| (_| | ||  __/ (_| | (_) | |  | |_| | *)
(*  \____\__,_|\__\___|\__, |\___/|_|   \__, | *)
(*                     |___/            |___/  *)
(* Category *)
and check_category : category -> bool = fun _ -> true
and cmp_category (c1 : category) (c2 : category) : int =
  match c1, c2 with
  | AtomicCategory c1, AtomicCategory c2 -> cmp_atomic c1.cat_atom c2.cat_atom

(*  _____                 _              *)
(* |  ___|   _ _ __   ___| |_ ___  _ __  *)
(* | |_ | | | | '_ \ / __| __/ _ \| '__| *)
(* |  _|| |_| | | | | (__| || (_) | |    *)
(* |_|   \__,_|_| |_|\___|\__\___/|_|    *)
(*                                       *)
(* Functor *)
and funct_src (f : funct) : category =
  match f with
  | AtomicFunctor f -> f.funct_src_

and funct_dst (f : funct) : category =
  match f with
  | AtomicFunctor f -> f.funct_dst_

and check_funct : funct -> bool = fun _ -> true
and cmp_funct (f1 : funct) (f2 : funct) : int =
  match f1, f2 with
  | AtomicFunctor f1, AtomicFunctor f2 -> cmp_atomic f1.funct_atom f2.funct_atom

(*  _____ _                 *)
(* | ____| | ___ _ __ ___   *)
(* |  _| | |/ _ \ '_ ` _ \  *)
(* | |___| |  __/ | | | | | *)
(* |_____|_|\___|_| |_| |_| *)
(*                          *)
(* Elem *)
and elem_cat (e : elem) : category =
  match e with
  | AtomicElem e -> e.elem_cat_ 
  | FObj (f,_) -> funct_dst f

and check_elem (e : elem) : bool =
  match e with
  | AtomicElem _ -> true 
  | FObj (f,e) -> check_funct f 
               && check_elem e 
               && cmp_category (elem_cat e) (funct_src f) = 0

and cmp_elem (e1 : elem) (e2 : elem) : int =
  match e1, e2 with
  | AtomicElem e1, AtomicElem e2 -> cmp_atomic e1.elem_atom e2.elem_atom
  | FObj (f1,e1), FObj (f2,e2) ->
      let cf = cmp_funct f1 f2 in 
      if cf = 0 then cmp_elem e1 e2 else cf 
  | AtomicElem _, FObj _ -> 1
  | FObj _, AtomicElem _ -> -1

(*  __  __                  _     _                    *)
(* |  \/  | ___  _ __ _ __ | |__ (_)___ _ __ ___  ___  *)
(* | |\/| |/ _ \| '__| '_ \| '_ \| / __| '_ ` _ \/ __| *)
(* | |  | | (_) | |  | |_) | | | | \__ \ | | | | \__ \ *)
(* |_|  |_|\___/|_|  | .__/|_| |_|_|___/_| |_| |_|___/ *)
(*                   |_|                               *)
(* Morphisms *)
and morphism_cat (m : morphism) : category =
  match m with
  | AtomicMorphism m -> m.mph_cat_ 
  | Identity e -> elem_cat e
  | Comp (m1,m2) -> morphism_cat m1
  | Inv m -> morphism_cat m 
  | FMph (f,_) -> funct_dst f
and morphism_src (m : morphism) : elem =
  match m with
  | AtomicMorphism m -> m.mph_src_ 
  | Identity e -> e
  | Comp (m1,m2) -> morphism_src m1
  | Inv m -> morphism_dst m 
  | FMph (f,m) -> FObj (f,morphism_src m)
and morphism_dst (m : morphism) : elem = 
  match m with 
  | AtomicMorphism m -> m.mph_dst_ 
  | Identity e -> e
  | Comp (m1,m2) -> morphism_dst m2
  | Inv m -> morphism_src m 
  | FMph (f,m) -> FObj (f,morphism_dst m)

and is_iso (m : morphism) : bool =
  match m with
  | AtomicMorphism m -> begin match m.iso with | Some _ -> true | None -> false end
  | Identity e -> true
  | Comp (m1,m2) -> is_iso m1 && is_iso m2
  | Inv m -> true
  | FMph (_,m) -> is_iso m
and check_morphism (m : morphism) : bool =
  match m with
  | AtomicMorphism m -> true
  | Identity e -> check_elem e
  | Comp (m1,m2) -> check_morphism m1
                 && check_morphism m2 
                 && cmp_elem (morphism_dst m1) (morphism_src m2) = 0
  | Inv m -> check_morphism m && is_iso m
  | FMph (f,m) -> check_funct f 
               && check_morphism m 
               && cmp_category (funct_src f) (morphism_cat m) = 0

and morphism_constructor_id (m : morphism) : int =
  match m with
  | AtomicMorphism _ -> 0
  | Identity _ -> 1
  | Comp _ -> 2 
  | Inv _ -> 3 
  | FMph _ -> 4
and cmp_morphism (m1 : morphism) (m2 : morphism) : int =
  match m1, m2 with
  | AtomicMorphism m1, AtomicMorphism m2 -> cmp_atomic m1.mph_atom m2.mph_atom
  | Identity e1, Identity e2 -> cmp_elem e1 e2
  | Comp (m11,m12), Comp (m21,m22) ->
      let d = cmp_morphism m11 m21 in 
      if d = 0 then cmp_morphism m12 m22 else d
  | Inv m1, Inv m2 -> cmp_morphism m1 m2 
  | FMph (f1,m1), FMph (f2,m2) ->
      let d = cmp_funct f1 f2 in 
      if d = 0 then cmp_morphism m1 m2 else d
  | _, _ -> morphism_constructor_id m2 - morphism_constructor_id m1

(*  _____                  _ _ _          *)
(* | ____|__ _ _   _  __ _| (_) |_ _   _  *)
(* |  _| / _` | | | |/ _` | | | __| | | | *)
(* | |__| (_| | |_| | (_| | | | |_| |_| | *)
(* |_____\__, |\__,_|\__,_|_|_|\__|\__, | *)
(*          |_|                    |___/  *)
(* Equality *)
and eq_left (e : eq) : morphism = 
  match e with
  | Refl m -> m 
  | Concat (e1,_) -> eq_left e1
  | InvEq e -> eq_right e
  | Compose (e1,e2) -> Comp (eq_left e1,eq_left e2)
  | Assoc (m1,m2,m3) -> Comp (Comp (m1,m2),m3)
  | LeftId m -> Comp (m,Identity (morphism_dst m))
  | RightId m -> Comp (Identity (morphism_src m),m)
  | RAp (e,m) -> Comp (eq_left e,m)
  | LAp (m,e) -> Comp (m,eq_left e)
  | RInv i -> Comp (AtomicMorphism i.iso_mph, AtomicMorphism i.iso_inv)
  | LInv i -> Comp (AtomicMorphism i.iso_inv, AtomicMorphism i.iso_mph)
  | Mono (_,m,_,_) -> m 
  | Epi (_,m,_,_) -> m
  | FId (f,e) -> FMph (f, Identity e)
  | FComp (f,m1,m2) -> FMph (f, Comp (m1,m2))
  | FCtx (f,e) -> FMph (f,eq_left e)
  | AtomicEq e -> e.eq_left_

and eq_right (e : eq) : morphism = 
  match e with
  | Refl m -> m 
  | Concat (_,e2) -> eq_right e2
  | InvEq e -> eq_left e
  | Compose (e1,e2) -> Comp (eq_right e1,eq_right e2)
  | Assoc (m1,m2,m3) -> Comp (m1,Comp (m2,m3))
  | LeftId m -> m
  | RightId m -> m
  | RAp (e,m) -> Comp (eq_right e,m)
  | LAp (m,e) -> Comp (m,eq_right e)
  | RInv i -> Identity i.iso_mph.mph_src_
  | LInv i -> Identity i.iso_mph.mph_dst_
  | Mono (_,_,m,_) -> m 
  | Epi (_,_,m,_) -> m
  | FId (f,e) -> Identity (FObj (f,e))
  | FComp (f,m1,m2) -> Comp (FMph (f,m1), FMph (f,m2))
  | FCtx (f,e) -> FMph (f,eq_right e)
  | AtomicEq e -> e.eq_right_

and eq_src (e : eq) : elem =
  match e with
  | Refl m -> morphism_src m 
  | Concat (e,_) -> eq_src e
  | InvEq e -> eq_src e
  | Compose (e1,_) -> eq_src e1
  | Assoc (m1,_,_) -> morphism_src m1
  | LeftId m -> morphism_src m
  | RightId m -> morphism_src m
  | RAp (e,m) -> eq_src e
  | LAp (m,e) -> morphism_src m
  | RInv i -> i.iso_mph.mph_src_
  | LInv i -> i.iso_mph.mph_dst_
  | Mono (_,_,m,_) -> morphism_src m
  | Epi (_,_,m,_) -> morphism_src m
  | FId (f,e) -> FObj (f,e)
  | FComp (f,m1,_) -> FObj (f, morphism_src m1)
  | FCtx (f,e) -> FObj (f, eq_src e)
  | AtomicEq e -> e.eq_src_

and eq_dst (e : eq) : elem =
  match e with 
  | Refl m -> morphism_dst m 
  | Concat (e,_) -> eq_dst e
  | InvEq e -> eq_dst e
  | Compose (_,e2) -> eq_dst e2
  | Assoc (_,_,m3) -> morphism_dst m3
  | LeftId m -> morphism_dst m 
  | RightId m -> morphism_dst m
  | RAp (e,m) -> morphism_dst m 
  | LAp (m,e) -> eq_dst e
  | RInv i -> i.iso_mph.mph_src_ 
  | LInv i -> i.iso_mph.mph_dst_
  | Mono (_,_,m,_) -> morphism_dst m
  | Epi (_,_,m,_) -> morphism_dst m
  | FId (f,e) -> FObj (f,e)
  | FComp (f,_,m2) -> FObj (f, morphism_dst m2)
  | FCtx (f,e) -> FObj (f, eq_dst e)
  | AtomicEq e -> e.eq_dst_

and eq_cat (e : eq) : category =
  match e with 
  | Refl m -> morphism_cat m 
  | Concat (e,_) -> morphism_cat (eq_left e)
  | InvEq e -> morphism_cat (eq_left e)
  | Compose (_,e2) -> morphism_cat (eq_left e2)
  | Assoc (m1,_,_) -> morphism_cat m1
  | LeftId m -> morphism_cat m 
  | RightId m -> morphism_cat m
  | RAp (e,m) -> morphism_cat m 
  | LAp (m,e) -> morphism_cat (eq_left e)
  | RInv i -> i.iso_mph.mph_cat_
  | LInv i -> i.iso_mph.mph_cat_
  | Mono (_,_,m,_) -> morphism_cat m
  | Epi (_,_,m,_) -> morphism_cat m
  | FId (f,_) -> funct_dst f 
  | FComp (f,_,_) -> funct_dst f 
  | FCtx (f,_) -> funct_dst f
  | AtomicEq e -> e.eq_cat_

and check_eq (e : eq) : bool =
  match e with
  | Refl m -> check_morphism m 
  | Concat (e1,e2) -> check_eq e1
                   && check_eq e2
                   && cmp_morphism (eq_right e1) (eq_left e2) = 0
  | InvEq e -> check_eq e
  | Compose (e1,e2) -> check_eq e1
                    && check_eq e2
                    && cmp_elem (eq_dst e1) (eq_src e2) = 0
  | Assoc (m1,m2,m3) -> check_morphism m1 && check_morphism m2 && check_morphism m3
  | LeftId m -> check_morphism m 
  | RightId m -> check_morphism m
  | RAp (e,m) -> check_eq e 
              && check_morphism m
              && cmp_elem (eq_dst e) (morphism_src m) = 0
  | LAp (m,e) -> check_eq e 
              && check_morphism m 
              && cmp_elem (morphism_dst m) (eq_src e) = 0
  | RInv _ -> true 
  | LInv _ -> true 
  | Mono (_,m1,m2,e) -> check_eq e 
                     && check_morphism m1 
                     && check_morphism m2 
                     && cmp_elem (morphism_src m1) (morphism_src m2) = 0
                     && cmp_elem (morphism_dst m2) (morphism_dst m2) = 0
  | Epi (_,m1,m2,e) -> check_eq e 
                    && check_morphism m1 
                    && check_morphism m2 
                    && cmp_elem (morphism_src m1) (morphism_src m2) = 0
                    && cmp_elem (morphism_dst m2) (morphism_dst m2) = 0
  | FId (f,e) -> check_funct f 
              && check_elem e 
              && cmp_category (funct_src f) (elem_cat e) = 0
  | FComp (f,m1,m2) -> check_funct f 
                    && check_morphism m1 
                    && check_morphism m2 
                    && cmp_category (funct_src f) (morphism_cat m1) = 0
                    && cmp_category (funct_src f) (morphism_cat m2) = 0
                    && cmp_elem (morphism_dst m1) (morphism_src m2) = 0
  | FCtx (f,e) -> check_funct f 
               && check_eq e 
               && cmp_category (funct_src f) (eq_cat e) = 0
  | AtomicEq _ -> true

and eq_constructor_id eq =
  match eq with
  | Refl _ -> 1
  | Concat _ -> 2
  | InvEq _ -> 3
  | Compose _ -> 4
  | Assoc _ -> 5
  | LeftId _ -> 6
  | RightId _ -> 7
  | RAp _ -> 8
  | LAp _ -> 9
  | RInv _ -> 10
  | LInv _ -> 11
  | Mono _ -> 12
  | Epi _ -> 13
  | FId _ -> 14
  | FComp _ -> 15
  | FCtx _ -> 16
  | AtomicEq _ -> 17

and cmp_eq eq1 eq2 =
  match eq1, eq2 with
  | Refl m1, Refl m2 -> cmp_morphism m1 m2
  | Concat (eq11,eq12), Concat (eq21,eq22) -> cmp2 (cmp_eq eq11 eq21) (cmp_eq eq12 eq22)
  | InvEq eq1, InvEq eq2 -> cmp_eq eq1 eq2
  | Compose (eq11,eq12), Compose (eq21,eq22) -> cmp2 (cmp_eq eq11 eq21) (cmp_eq eq12 eq22)
  | Assoc (m11,m12,m13), Assoc (m21,m22,m23) ->
      cmp3 (cmp_morphism m11 m21) (cmp_morphism m12 m22) (cmp_morphism m13 m23)
  | LeftId m1, LeftId m2 -> cmp_morphism m1 m2
  | RightId m1, RightId m2 -> cmp_morphism m1 m2
  | RAp (eq1,m1), RAp (eq2,m2) -> cmp2 (cmp_eq eq1 eq2) (cmp_morphism m1 m2)
  | LAp (m1,eq1), LAp (m2,eq2) -> cmp2 (cmp_morphism m1 m2) (cmp_eq eq1 eq2)
  | RInv iso1, RInv iso2 -> cmp_atomic iso2.iso_mph.mph_atom iso1.iso_mph.mph_atom
  | LInv iso1, LInv iso2 -> cmp_atomic iso2.iso_mph.mph_atom iso1.iso_mph.mph_atom
  | Mono (_,m11,m12,eq1), Mono (_,m21,m22,eq2) ->
      cmp3 (cmp_morphism m11 m21) (cmp_morphism m12 m22) (cmp_eq eq1 eq2)
  | Epi (_,m11,m12,eq1), Epi (_,m21,m22,eq2) ->
      cmp3 (cmp_morphism m11 m21) (cmp_morphism m12 m22) (cmp_eq eq1 eq2)
  | FId (f1,e1), FId (f2,e2) -> cmp2 (cmp_funct f1 f2) (cmp_elem e1 e2)
  | FComp (f1,m11,m12), FComp (f2,m21,m22) ->
      cmp3 (cmp_funct f1 f2) (cmp_morphism m11 m21) (cmp_morphism m12 m22)
  | FCtx (f1,eq1), FCtx (f2,eq2) -> cmp2 (cmp_funct f1 f2) (cmp_eq eq1 eq2)
  | AtomicEq eq1, AtomicEq eq2 -> cmp_atomic eq1.eq_atom eq2.eq_atom
  | _, _ -> eq_constructor_id eq2 - eq_constructor_id eq1


(*  __  __           _       _            *)
(* |  \/  | ___   __| |_   _| | ___  ___  *)
(* | |\/| |/ _ \ / _` | | | | |/ _ \/ __| *)
(* | |  | | (_) | (_| | |_| | |  __/\__ \ *)
(* |_|  |_|\___/ \__,_|\__,_|_|\___||___/ *)
(*                                        *)
(* Modules *)

module EqCat = struct
  type t = category 
  let compare = cmp_category 
end

module EqFunct = struct 
  type t = funct
  let compare = cmp_funct
end

module EqElem = struct 
  type t = elem 
  let compare = cmp_elem
end

module EqMph = struct 
  type t = morphism 
  let compare = cmp_morphism 
end

module EqEq = struct
  type t = eq
  let compare = cmp_eq
end

