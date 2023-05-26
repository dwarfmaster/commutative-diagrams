
module Tag : sig
  type t =
    (* Types *)
    | Category
    | Object
    | Morphism
    | Functor
    | Equality
    (* Objects *)
    | AppliedFunctObj
    (* Morphisms *)
    | Identity
    | ComposeMph
    | AppliedFunctMph
    (* Equality *)
    | Reflexivity
    | Concat
    | InverseEq
    | ComposeEq
    | Associativity
    | LeftUnitality
    | RightUnitality
    | LeftApplication
    | RightApplication
    | FunctIdentity
    | FunctComposition
    | AppliedFunctEq

  module Eq : Map.OrderedType with type t = t
end

type obj = Hyps.obj
type t =
  (* Types *)
  | Category
  | Object of (*cat*)obj
  | Morphism of (*cat*)obj * (*src*)obj * (*dst*)obj
  | Functor of (*src*)obj * (*dst*)obj
  | Equality of (*cat*)obj * (*src*)obj * (*dst*)obj * (*left*)obj * (*right*)obj
  (* Objects *)
  | AppliedFunctObj of (*src*)obj * (*dst*)obj * (*funct*)obj * (*obj*)obj
  (* Morphisms *)
  | Identity of (*cat*)obj * (*obj*)obj
  | ComposeMph of (*cat*)obj * (*src*)obj * (*mid*)obj * (*dst*)obj * (*m1*)obj * (*m2*)obj
  | AppliedFunctMph of (*src*)obj * (*dst*)obj * (*funct*)obj
                     * (*src*)obj * (*dst*)obj * (*mph*)obj
  (* Equality *)
  | Reflexivity of (*cat*)obj * (*src*)obj * (*dst*)obj * (*mph*)obj
  | Concat of (*cat*)obj * (*src*)obj * (*dst*)obj * (*left*)obj * (*mid*)obj * (*right*)obj
            * (*eq1*)obj * (*eq2*)obj
  | InverseEq of (*cat*)obj * (*src*)obj * (*dst*)obj * (*left*)obj * (*right*)obj * (*eq*)obj
  | ComposeEq of (*cat*)obj * (*src*)obj * (*mid*)obj * (*dst*)obj
               * (*left1*)obj * (*right1*)obj * (*eq1*)obj
               * (*left2*)obj * (*right2*)obj * (*eq2*)obj
  | Associativity of (*cat*)obj * (*src*)obj * (*mid1*)obj * (*mid2*)obj * (*dst*)obj
                   * (*m1*)obj * (*m2*)obj * (*m3*)obj
  | LeftUnitality of (*cat*)obj * (*src*)obj * (*dst*)obj * (*mph*)obj
  | RightUnitality of (*cat*)obj * (*src*)obj * (*dst*)obj * (*mph*)obj
  | LeftApplication of (*cat*)obj * (*src*)obj * (*mid*)obj * (*dst*)obj
                     * (*mph*)obj * (*left*)obj * (*right*)obj * (*eq*)obj
  | RightApplication of (*cat*)obj * (*src*)obj * (*mid*)obj * (*dst*)obj
                      * (*left*)obj * (*right*)obj * (*eq*)obj * (*mph*)obj
  | FunctIdentity of (*src*)obj * (*dst*)obj * (*funct*)obj * (*obj*)obj
  | FunctComposition of (*src*)obj * (*dst*)obj * (*funct*)obj
                      * (*src*)obj * (*mid*)obj * (*dst*)obj * (*m1*)obj * (*m2*)obj
  | AppliedFunctEq of (*src*)obj * (*dst*)obj * (*funct*)obj
                    * (*src*)obj * (*dst*)obj * (*left*)obj * (*right*)obj * (*eq*)obj

val tag : t -> Tag.t
val to_list : t -> obj list
module Eq : Map.OrderedType with type t = t
