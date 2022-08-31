
(*  _____                       *)
(* |_   _|   _ _ __   ___  ___  *)
(*   | || | | | '_ \ / _ \/ __| *)
(*   | || |_| | |_) |  __/\__ \ *)
(*   |_| \__, | .__/ \___||___/ *)
(*       |___/|_|               *)
(* Types *)
type 't categoryData =
  { cat_obj : 't
  ; cat_id  : int
  }
and 't category =
  | AtomicCategory of 't categoryData
and 't functData =
  { funct_obj  : 't
  ; funct_id   : int
  ; funct_src_ : 't category
  ; funct_dst_ : 't category
  }
and 't funct =
  | AtomicFunctor of 't functData
and 't elemData =
  { elem_obj  : 't
  ; elem_cat_ : 't category
  ; elem_id   : int
  }
and 't elem =
  | AtomicElem of 't elemData
  | FObj of 't funct * 't elem
and 't morphismData =
  { mph_obj  : 't
  ; mph_cat_ : 't category
  ; mph_src_ : 't elem 
  ; mph_dst_ : 't elem
  ; mph_id   : int
  ; mutable mono : 't option
  ; mutable epi  : 't option
  ; mutable iso  : 't isoData option
  }
and 't morphism =
  | AtomicMorphism of 't morphismData
  | Identity of 't elem
  | Comp of 't morphism * 't morphism (* Comp (m1,m2) ~ m2 o m1 *)
  | Inv of 't morphism
  | FMph of 't funct * 't morphism
and 't isoData =
  { iso_obj : 't
  ; iso_mph : 't morphism
  ; iso_inv : 't morphism
  }

(* Equality between uninterned morphisms *)
type 't eq =
  | Refl of 't morphism
  | Concat of 't eq * 't eq
  | InvEq of 't eq
  | Compose of 't eq * 't eq
  | Assoc of 't morphism * 't morphism * 't morphism
  | LeftId of 't morphism
  | RightId of 't morphism
  | RAp of 't eq * 't morphism
  | LAp of 't morphism * 't eq
  | RInv of 't isoData
  | LInv of 't isoData
  | Mono of 't * 't morphism * 't morphism * 't eq
  | Epi of 't * 't morphism * 't morphism * 't eq
  | AtomicEq of 't eqData
and 't eqData =
  { eq_left_  : 't morphism
  ; eq_right_ : 't morphism
  ; eq_src_   : 't elem 
  ; eq_dst_   : 't elem 
  ; eq_cat_   : 't category
  ; eq_obj    : 't
  ; eq_id     : int
  }


(*   ____      _                               *)
(*  / ___|__ _| |_ ___  __ _  ___  _ __ _   _  *)
(* | |   / _` | __/ _ \/ _` |/ _ \| '__| | | | *)
(* | |__| (_| | ||  __/ (_| | (_) | |  | |_| | *)
(*  \____\__,_|\__\___|\__, |\___/|_|   \__, | *)
(*                     |___/            |___/  *)
(* Category *)
let check_category : 't category -> bool = fun _ -> true
let cmp_category (c1 : 't category) (c2 : 't category) : int =
  match c1, c2 with
  | AtomicCategory c1, AtomicCategory c2 -> c2.cat_id - c1.cat_id

(*  _____                 _              *)
(* |  ___|   _ _ __   ___| |_ ___  _ __  *)
(* | |_ | | | | '_ \ / __| __/ _ \| '__| *)
(* |  _|| |_| | | | | (__| || (_) | |    *)
(* |_|   \__,_|_| |_|\___|\__\___/|_|    *)
(*                                       *)
(* Functor *)
let funct_src (f : 't funct) : 't category =
  match f with
  | AtomicFunctor f -> f.funct_src_

let funct_dst (f : 't funct) : 't category =
  match f with
  | AtomicFunctor f -> f.funct_dst_

let check_funct : 't funct -> bool = fun _ -> true
let cmp_funct (f1 : 't funct) (f2 : 't funct) : int =
  match f1, f2 with
  | AtomicFunctor f1, AtomicFunctor f2 -> f2.funct_id - f1.funct_id

(*  _____ _                 *)
(* | ____| | ___ _ __ ___   *)
(* |  _| | |/ _ \ '_ ` _ \  *)
(* | |___| |  __/ | | | | | *)
(* |_____|_|\___|_| |_| |_| *)
(*                          *)
(* Elem *)
let elem_cat (e : 't elem) : 't category =
  match e with
  | AtomicElem e -> e.elem_cat_ 
  | FObj (f,_) -> funct_dst f

let rec check_elem (e : 't elem) : bool =
  match e with
  | AtomicElem _ -> true 
  | FObj (f,e) -> check_funct f 
               && check_elem e 
               && cmp_category (elem_cat e) (funct_src f) = 0

let rec cmp_elem (e1 : 't elem) (e2 : 't elem) : int =
  match e1, e2 with
  | AtomicElem e1, AtomicElem e2 -> e2.elem_id - e1.elem_id
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
let rec morphism_cat (m : 't morphism) : 't category =
  match m with
  | AtomicMorphism m -> m.mph_cat_ 
  | Identity e -> elem_cat e
  | Comp (m1,m2) -> morphism_cat m1
  | Inv m -> morphism_cat m 
  | FMph (f,_) -> funct_dst f
let rec morphism_src (m : 't morphism) : 't elem =
  match m with
  | AtomicMorphism m -> m.mph_src_ 
  | Identity e -> e
  | Comp (m1,m2) -> morphism_src m1
  | Inv m -> morphism_dst m 
  | FMph (f,m) -> FObj (f,morphism_src m)
and morphism_dst (m : 't morphism) : 't elem = 
  match m with 
  | AtomicMorphism m -> m.mph_dst_ 
  | Identity e -> e
  | Comp (m1,m2) -> morphism_dst m2
  | Inv m -> morphism_src m 
  | FMph (f,m) -> FObj (f,morphism_dst m)

let rec is_iso (m : 't morphism) : bool =
  match m with
  | AtomicMorphism m -> begin match m.iso with | Some _ -> true | None -> false end
  | Identity e -> true
  | Comp (m1,m2) -> is_iso m1 && is_iso m2
  | Inv m -> true
  | FMph (_,m) -> is_iso m
let rec check_morphism (m : 't morphism) : bool =
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

let morphism_constructor_id (m : 't morphism) : int =
  match m with
  | AtomicMorphism _ -> 0
  | Identity _ -> 1
  | Comp _ -> 2 
  | Inv _ -> 3 
  | FMph _ -> 4
let rec cmp_morphism (m1 : 't morphism) (m2 : 't morphism) : int =
  match m1, m2 with
  | AtomicMorphism m1, AtomicMorphism m2 -> m2.mph_id - m1.mph_id
  | Identity e1, Identity e2 -> cmp_elem e1 e2
  | Comp (m11,m12), Comp (m21,m22) ->
      let d = cmp_morphism m11 m21 in 
      if d = 0 then cmp_morphism m21 m22 else d
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
let rec eq_left (e : 't eq) : ' tmorphism = 
  match e with
  | Refl m -> m 
  | Concat (e1,_) -> eq_left e1
  | InvEq e -> eq_right e
  | Compose (e1,e2) -> Comp (eq_left e1,eq_left e2)
  | Assoc (m1,m2,m3) -> Comp (Comp (m1,m2),m3)
  | LeftId m -> Comp (Identity (morphism_src m),m)
  | RightId m -> Comp (m,Identity (morphism_dst m))
  | RAp (e,m) -> Comp (eq_left e,m)
  | LAp (m,e) -> Comp (m,eq_left e)
  | RInv i -> Comp (i.iso_inv, i.iso_mph)
  | LInv i -> Comp (i.iso_mph, i.iso_inv)
  | Mono (_,m,_,_) -> m 
  | Epi (_,m,_,_) -> m
  | AtomicEq e -> e.eq_left_

and eq_right (e : 't eq) : 't morphism = 
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
  | RInv i -> Identity (morphism_src i.iso_mph)
  | LInv i -> Identity (morphism_dst i.iso_mph)
  | Mono (_,_,m,_) -> m 
  | Epi (_,_,m,_) -> m
  | AtomicEq e -> e.eq_right_

and eq_src (e : 't eq) : 't elem =
  match e with
  | Refl m -> morphism_src m 
  | Concat (e,_) -> morphism_src (eq_left e)
  | InvEq e -> morphism_src (eq_left e)
  | Compose (e1,_) -> morphism_src (eq_left e1)
  | Assoc (m1,_,_) -> morphism_src m1
  | LeftId m -> morphism_src m
  | RightId m -> morphism_src m
  | RAp (e,m) -> morphism_src (eq_left e)
  | LAp (m,e) -> morphism_src m
  | RInv i -> morphism_src i.iso_mph
  | LInv i -> morphism_dst i.iso_mph
  | Mono (_,_,m,_) -> morphism_src m
  | Epi (_,_,m,_) -> morphism_src m
  | AtomicEq e -> e.eq_src_

and eq_dst (e : 't eq) : 't elem =
  match e with 
  | Refl m -> morphism_dst m 
  | Concat (e,_) -> morphism_dst (eq_left e)
  | InvEq e -> morphism_dst (eq_left e)
  | Compose (_,e2) -> morphism_dst (eq_left e2)
  | Assoc (m1,_,_) -> morphism_dst m1
  | LeftId m -> morphism_dst m 
  | RightId m -> morphism_dst m
  | RAp (e,m) -> morphism_dst m 
  | LAp (m,e) -> morphism_dst (eq_left e)
  | RInv i -> morphism_dst i.iso_mph 
  | LInv i -> morphism_src i.iso_mph 
  | Mono (_,_,m,_) -> morphism_dst m
  | Epi (_,_,m,_) -> morphism_dst m
  | AtomicEq e -> e.eq_dst_

and eq_cat (e : 't eq) : 't category =
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
  | RInv i -> morphism_cat i.iso_mph 
  | LInv i -> morphism_cat i.iso_mph 
  | Mono (_,_,m,_) -> morphism_cat m
  | Epi (_,_,m,_) -> morphism_cat m
  | AtomicEq e -> e.eq_cat_

let rec check_eq (e : 't eq) : bool =
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
  | AtomicEq _ -> true



(*  __  __           _       _            *)
(* |  \/  | ___   __| |_   _| | ___  ___  *)
(* | |\/| |/ _ \ / _` | | | | |/ _ \/ __| *)
(* | |  | | (_) | (_| | |_| | |  __/\__ \ *)
(* |_|  |_|\___/ \__,_|\__,_|_|\___||___/ *)
(*                                        *)
(* Modules *)

module type Type = sig
  type t 
end

module EqCat(T:Type) = struct
  type t = T.t category 
  let compare = cmp_category 
end

module EqFunct(T:Type) = struct 
  type t = T.t funct
  let compare = cmp_funct
end

module EqElem(T:Type) = struct 
  type t = T.t elem 
  let compare = cmp_elem
end

module EqMph(T:Type) = struct 
  type t = T.t morphism 
  let compare = cmp_morphism 
end
