
type 't atomic =
  | Ctx of 't (* A constant in the context or an hypothesis *)
  | Evar of int * 't option (* An evar. It may be backed by a coq evar (and will be at realization time) *)

type 't categoryData =
  { cat_obj : 't atomic
  ; cat_id  : int
  }
and 't category =
  | AtomicCategory of 't categoryData
and 't functData =
  { funct_obj  : 't atomic
  ; funct_id   : int
  ; funct_src_ : 't category
  ; funct_dst_ : 't category
  }
and 't funct =
  | AtomicFunctor of 't functData
and 't elemData =
  { elem_obj  : 't atomic
  ; elem_cat_ : 't category
  ; elem_id   : int
  }
and 't elem =
  | AtomicElem of 't elemData
  | FObj of 't funct * 't elem
and 't morphismData =
  { mph_obj  : 't atomic
  ; mph_cat_ : 't category
  ; mph_src_ : 't elem 
  ; mph_dst_ : 't elem
  ; mph_id   : int
  ; mutable mono : 't option
  ; mutable epi  : 't option
  ; mutable iso  : 't isoData option
  }
and 't isoData =
  { iso_obj : 't
  ; iso_mph : 't morphismData
  ; iso_inv : 't morphismData
  }
and 't morphism =
  | AtomicMorphism of 't morphismData
  | Identity of 't elem
  | Comp of 't morphism * 't morphism (* Comp (m1,m2) ~ m2 o m1 *)
  | Inv of 't morphism
  | FMph of 't funct * 't morphism

(* check_* check invariants about the structure assumed everywhere in the code
   that are not enforced by the type system, for example that the composition
   of two morphisms have the right endpoints.
 *)
val check_category : 't category -> bool
val cmp_category : 't category -> 't category -> int

val check_funct : 't funct -> bool
val cmp_funct : 't funct -> 't funct -> int
val funct_src : 't funct -> 't category 
val funct_dst : 't funct -> 't category 

val check_elem : 't elem -> bool
val cmp_elem : 't elem -> 't elem -> int
val elem_cat  : 't elem -> 't category 

val check_morphism : 't morphism -> bool
val cmp_morphism : 't morphism -> 't morphism -> int
val morphism_cat : 't morphism -> 't category 
val morphism_src : 't morphism -> 't elem 
val morphism_dst : 't morphism -> 't elem

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
  | FId of 't funct * 't elem
  | FComp of 't funct * 't morphism * 't morphism
  | FCtx of 't funct * 't eq
  | AtomicEq of 't eqData
and 't eqData =
  { eq_left_  : 't morphism
  ; eq_right_ : 't morphism
  ; eq_src_   : 't elem 
  ; eq_dst_   : 't elem 
  ; eq_cat_   : 't category
  ; eq_obj    : 't atomic
  ; eq_id     : int
  }
val check_eq : 't eq -> bool
val cmp_eq : 't eq -> 't eq -> int
val eq_left  : 't eq -> 't morphism 
val eq_right : 't eq -> 't morphism 
val eq_src   : 't eq -> 't elem 
val eq_dst   : 't eq -> 't elem 
val eq_cat   : 't eq -> 't category

(* Comparison modules for convenience *)
module type Type = sig
  type t 
end
module EqCat(T:Type) : Map.OrderedType with type t = T.t category
module EqFunct(T:Type) : Map.OrderedType with type t = T.t funct
module EqElem(T:Type) : Map.OrderedType with type t = T.t elem
module EqMph(T:Type) : Map.OrderedType with type t = T.t morphism
module EqEq(T:Type) : Map.OrderedType with type t = T.t eq

