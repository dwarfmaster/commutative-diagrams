
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
    type nonrec t = t

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
    | AppliedFunctMph -> 9
    | Reflexivity -> 10
    | Concat -> 11
    | InverseEq -> 12
    | ComposeEq -> 13
    | Associativity -> 14
    | LeftUnitality -> 15
    | RightUnitality -> 16
    | LeftApplication -> 17
    | RightApplication -> 18
    | FunctIdentity -> 19
    | FunctComposition -> 20
    | AppliedFunctEq -> 21

    let compare tag1 tag2 = Int.compare (tag_id tag1) (tag_id tag2)
  end
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

let tag = function
| Category -> Tag.Category
| Object _ -> Tag.Object
| Morphism _ -> Tag.Morphism
| Functor _ -> Tag.Functor
| Equality _ -> Tag.Equality
| AppliedFunctObj _ -> Tag.AppliedFunctObj
| Identity _ -> Tag.Identity
| ComposeMph _ -> Tag.ComposeMph
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
  type nonrec t = t
  let compare x1 x2 =
    let cmp = Tag.Eq.compare (tag x1) (tag x2) in
    if cmp = 0
    then List.compare Hyps.compare_obj (to_list x1) (to_list x2)
    else cmp
end
