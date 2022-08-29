
module PA : sig
  type t
end

type categoryData =
  { cat_obj : PA.t
  ; cat_id  : int
  }
and category =
  | AtomicCategory of categoryData
and functData =
  { funct_obj  : PA.t
  ; funct_id   : int
  ; funct_src_ : category
  ; funct_dst_ : category
  }
and funct =
  | AtomicFunctor of functData
and elemData =
  { elem_obj  : PA.t
  ; elem_cat_ : category
  ; elem_id   : int
  }
and elem =
  | AtomicElem of elemData
  | FObj of funct * elem
and morphismData =
  { mph_obj  : PA.t
  ; mph_cat_ : category
  ; mph_src_ : elem 
  ; mph_dst_ : elem
  ; mph_id   : int
  ; mutable mono : PA.t option
  ; mutable epi  : PA.t option
  ; mutable iso  : isoData option
  }
and morphism =
  | AtomicMorphism of morphismData
  | Identity of elem
  | Comp of morphism * morphism (* Comp (m1,m2) ~ m2 o m1 *)
  | Inv of morphism
  | FMph of funct * morphism
and isoData =
  { iso_obj : PA.t
  ; iso_mph : morphism
  ; iso_inv : morphism
  }

(* check_* check invariants about the structure assumed everywhere in the code
   that are not enforced by the type system, for example that the composition
   of two morphisms have the right endpoints.
 *)
val check_category : category -> bool
val cmp_category : category -> category -> int

val check_funct : funct -> bool
val cmp_funct : funct -> funct -> int
val funct_src : funct -> category 
val funct_dst : funct -> category 

val check_elem : elem -> bool
val cmp_elem : elem -> elem -> int
val elem_cat  : elem -> category 

val check_morphism : morphism -> bool
val cmp_morphism : morphism -> morphism -> int
val morphism_cat : morphism -> category 
val morphism_src : morphism -> elem 
val morphism_dst : morphism -> elem

(* Equality between uninterned morphisms *)
type eq =
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
  | Mono of PA.t * morphism * morphism * eq
  | Epi of PA.t * morphism * morphism * eq
  | AtomicEq of eqData
and eqData =
  { eq_left_  : morphism
  ; eq_right_ : morphism
  ; eq_src_   : elem 
  ; eq_dst_   : elem 
  ; eq_cat_   : category
  ; eq_obj    : PA.t
  ; eq_id     : int
  }
val check_eq : eq -> bool
val eq_left  : eq -> morphism 
val eq_right : eq -> morphism 
val eq_src   : eq -> elem 
val eq_dst   : eq -> elem 
val eq_cat   : eq -> category

