
module Tag : sig
  type t =
    (* Types *)
    | Category
    | Object
    | Morphism
    | Functor
    | Equality
    | Prop
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
  | Prop
  (* Objects *)
  | AppliedFunctObj of (*funct*)int * (*obj*)int
  (* Morphisms *)
  | Identity of (*obj*)int
  | ComposeMph of (*m1*)int * (*m2*)int
  | InverseMph of int
  | AppliedFunctMph of (*funct*)int * (*mph*)int
  (* Equality *)
  | Reflexivity of (*mph*)int
  | Concat of (*eq1*)int * (*eq2*)int
  | InverseEq of (*eq*)int
  | ComposeEq of (*eq1*)int * (*eq2*)int
  | Associativity of (*m1*)int * (*m2*)int * (*m3*)int
  | LeftUnitality of (*mph*)int
  | RightUnitality of (*mph*)int
  | LeftApplication of (*mph*)int * (*eq*)int
  | RightApplication of (*eq*)int * (*mph*)int
  | FunctIdentity of (*funct*)int * (*obj*)int
  | FunctComposition of (*funct*)int * (*m1*)int * (*m2*)int
  | AppliedFunctEq of (*funct*)int * (*eq*)int

val tag : t -> Tag.t
val to_list : t -> int list
module Eq : Map.OrderedType with type t = t
