
type fn =
  | FnConst of EConstr.t
  | FnProj of Names.Projection.t
and atomic =
  | Ctx of int * EConstr.t (* A constant in the context or an hypothesis *)
  | Evar of int * EConstr.t option (* An evar. It may be backed by a coq evar (and will be at realization time) *)
  | Fn of int * fn (* An arbitrary coq term *)
  | Cat of category
  | Funct of funct
  | Elem of elem
  | Mph of morphism
  | Eq of eq
  | Composed of atomic * atomic list (* An application of the first atomic *)
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

val check_eq : eq -> bool
val cmp_eq : eq -> eq -> int
val eq_left  : eq -> morphism 
val eq_right : eq -> morphism 
val eq_src   : eq -> elem 
val eq_dst   : eq -> elem 
val eq_cat   : eq -> category

(* Comparison modules for convenience *)
module EqCat : Map.OrderedType with type t = category
module EqFunct : Map.OrderedType with type t = funct
module EqElem : Map.OrderedType with type t = elem
module EqMph : Map.OrderedType with type t = morphism
module EqEq : Map.OrderedType with type t = eq

