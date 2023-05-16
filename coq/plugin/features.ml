
module Tag = struct
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

  module Eq = struct
    type super = t
    type t = super

    let tag_id = function
    | Category -> 0
    | Object -> 1
    | Morphism -> 2
    | Functor -> 3
    | Equality -> 4
    | Prop -> 5
    | AppliedFunctObj -> 6
    | Identity -> 7
    | ComposeMph -> 8
    | InverseMph -> 9
    | AppliedFunctMph -> 10
    | Reflexivity -> 11
    | Concat -> 12
    | InverseEq -> 13
    | ComposeEq -> 14
    | Associativity -> 15
    | LeftUnitality -> 16
    | RightUnitality -> 17
    | LeftApplication -> 18
    | RightApplication -> 19
    | FunctIdentity -> 20
    | FunctComposition -> 21
    | AppliedFunctEq -> 22

    let compare tag1 tag2 = Int.compare (tag_id tag1) (tag_id tag2)
  end
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

let tag = function
| Category -> Tag.Category
| Object _ -> Tag.Object
| Morphism _ -> Tag.Morphism
| Functor _ -> Tag.Functor
| Equality _ -> Tag.Equality
| Prop -> Tag.Prop
| AppliedFunctObj _ -> Tag.AppliedFunctObj
| Identity _ -> Tag.Identity
| ComposeMph _ -> Tag.ComposeMph
| InverseMph _ -> Tag.InverseMph
| AppliedFunctMph _ -> Tag.AppliedFunctMph
| Reflexivity _ -> Tag.Reflexivity
| Concat _ -> Tag.Concat
| InverseEq _ -> Tag.InverseEq
| ComposeEq _ -> Tag.ComposeEq
| Associativity _ -> Tag.Associativity
| LeftUnitality _ -> Tag.LeftUnitality
| RightUnitality _ -> Tag.RightUnitality
| LeftApplication _ -> Tag.LeftApplication
| RightApplication _ -> Tag.RightApplication
| FunctIdentity _ -> Tag.FunctIdentity
| FunctComposition _ -> Tag.FunctComposition
| AppliedFunctEq _ -> Tag.AppliedFunctEq

let to_list = function
| Category -> []
| Object cat -> [cat]
| Morphism (cat,src,dst) -> [cat;src;dst]
| Functor (src,dst) -> [src;dst]
| Equality (cat,src,dst,left,right) -> [cat;src;dst;left;right]
| Prop -> []
| AppliedFunctObj (funct,obj) -> [funct;obj]
| Identity obj -> [obj]
| ComposeMph (m1,m2) -> [m1;m2]
| InverseMph mph -> [mph]
| AppliedFunctMph (funct,mph) -> [funct;mph]
| Reflexivity mph -> [mph]
| Concat (eq1,eq2) -> [eq1;eq2]
| InverseEq eq -> [eq]
| ComposeEq (eq1,eq2) -> [eq1;eq2]
| Associativity (m1,m2,m3) -> [m1;m2;m3]
| LeftUnitality mph -> [mph]
| RightUnitality mph -> [mph]
| LeftApplication (mph,eq) -> [mph;eq]
| RightApplication (eq,mph) -> [eq;mph]
| FunctIdentity (funct,obj) -> [funct;obj]
| FunctComposition (funct,m1,m2) -> [funct;m1;m2]
| AppliedFunctEq (funct,eq) -> [funct;eq]

module Eq = struct
  type super = t
  type t = super
  let compare x1 x2 =
    let cmp = Tag.Eq.compare (tag x1) (tag x2) in
    if cmp = 0
    then List.compare Int.compare (to_list x1) (to_list x2)
    else cmp
end
