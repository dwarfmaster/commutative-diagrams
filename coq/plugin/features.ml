
module Tag = struct
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

  module Eq = struct
    type super = t
    type t = super

    let tag_id = function
    | Category -> 0
    | Object -> 1
    | Morphism -> 2
    | Functor -> 3
    | Equality -> 4
    (* Prop ? *)
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

let tag = function
| Category -> Tag.Category
| Object _ -> Tag.Object
| Morphism _ -> Tag.Morphism
| Functor _ -> Tag.Functor
| Equality _ -> Tag.Equality
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
| AppliedFunctObj (src,dst,funct,obj) -> [src;dst;funct;obj]
| Identity (cat,obj) -> [cat;obj]
| ComposeMph (cat,src,mid,dst,m1,m2) -> [cat;src;mid;dst;m1;m2]
| InverseMph (cat,src,dst,mph) -> [cat;src;dst;mph]
| AppliedFunctMph (src,dst,funct,srcm,dstm,mph) -> [src;dst;funct;srcm;dstm;mph]
| Reflexivity (cat,src,dst,mph) -> [cat;src;dst;mph]
| Concat (cat,src,dst,left,mid,right,eq1,eq2) ->
    [cat;src;dst;left;mid;right;eq1;eq2]
| InverseEq (cat,src,dst,left,right,eq) -> [cat;src;dst;left;right;eq]
| ComposeEq (cat,src,mid,dst,left1,right1,eq1,left2,right2,eq2) -> 
    [cat;src;mid;dst;left1;right1;eq1;left2;right2;eq2]
| Associativity (cat,src,mid1,mid2,dst,m1,m2,m3) ->
    [cat;src;mid1;mid2;dst;m1;m2;m3]
| LeftUnitality (cat,src,dst,mph) -> [cat;src;dst;mph]
| RightUnitality (cat,src,dst,mph) -> [cat;src;dst;mph]
| LeftApplication (cat,src,mid,dst,mph,left,right,eq) ->
    [cat;src;mid;dst;mph;left;right;eq]
| RightApplication (cat,src,mid,dst,left,right,eq,mph) ->
    [cat;src;mid;dst;left;right;eq;mph]
| FunctIdentity (src,dst,funct,obj) -> [src;dst;funct;obj]
| FunctComposition (src,dst,funct,srcm,midm,dstm,m1,m2) -> 
    [src;dst;funct;srcm;midm;dstm;m1;m2]
| AppliedFunctEq (src,dst,funct,srcm,dstm,left,right,eq) ->
    [src;dst;funct;srcm;dstm;left;right;eq]

module Eq = struct
  type super = t
  type t = super
  let compare x1 x2 =
    let cmp = Tag.Eq.compare (tag x1) (tag x2) in
    if cmp = 0
    then List.compare Int.compare (to_list x1) (to_list x2)
    else cmp
end
