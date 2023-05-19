
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
    | InverseMph
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

type t =
  (* Types *)
  | Category
  | Object of (*cat*)int
  | Morphism of (*cat*)int * (*src*)int * (*dst*)int
  | Functor of (*src*)int * (*dst*)int
  | Equality of (*cat*)int * (*src*)int * (*dst*)int * (*left*)int * (*right*)int
  (* Objects *)
  | AppliedFunctObj of (*src*)int * (*dst*)int * (*funct*)int * (*obj*)int
  (* Morphisms *)
  | Identity of (*cat*)int * (*obj*)int
  | ComposeMph of (*cat*)int * (*src*)int * (*mid*)int * (*dst*)int * (*m1*)int * (*m2*)int
  | InverseMph of (*cat*)int * (*src*)int * (*dst*)int * (*mph*)int
  | AppliedFunctMph of (*src*)int * (*dst*)int * (*funct*)int
                     * (*src*)int * (*dst*)int * (*mph*)int
  (* Equality *)
  | Reflexivity of (*cat*)int * (*src*)int * (*dst*)int * (*mph*)int
  | Concat of (*cat*)int * (*src*)int * (*dst*)int * (*left*)int * (*mid*)int * (*right*)int
            * (*eq1*)int * (*eq2*)int
  | InverseEq of (*cat*)int * (*src*)int * (*dst*)int * (*left*)int * (*right*)int * (*eq*)int
  | ComposeEq of (*cat*)int * (*src*)int * (*mid*)int * (*dst*)int
               * (*left1*)int * (*right1*)int * (*eq1*)int
               * (*left2*)int * (*right2*)int * (*eq2*)int
  | Associativity of (*cat*)int * (*src*)int * (*mid1*)int * (*mid2*)int * (*dst*)int
                   * (*m1*)int * (*m2*)int * (*m3*)int
  | LeftUnitality of (*cat*)int * (*src*)int * (*dst*)int * (*mph*)int
  | RightUnitality of (*cat*)int * (*src*)int * (*dst*)int * (*mph*)int
  | LeftApplication of (*cat*)int * (*src*)int * (*mid*)int * (*dst*)int
                     * (*mph*)int * (*left*)int * (*right*)int * (*eq*)int
  | RightApplication of (*cat*)int * (*src*)int * (*mid*)int * (*dst*)int
                      * (*left*)int * (*right*)int * (*eq*)int * (*mph*)int
  | FunctIdentity of (*src*)int * (*dst*)int * (*funct*)int * (*obj*)int
  | FunctComposition of (*src*)int * (*dst*)int * (*funct*)int
                      * (*src*)int * (*mid*)int * (*dst*)int * (*m1*)int * (*m2*)int
  | AppliedFunctEq of (*src*)int * (*dst*)int * (*funct*)int
                    * (*src*)int * (*dst*)int * (*left*)int * (*right*)int * (*eq*)int

val tag : t -> Tag.t
val to_list : t -> int list
module Eq : Map.OrderedType with type t = t
