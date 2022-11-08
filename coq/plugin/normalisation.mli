
open Data

val normalizeMorphism : 't morphism -> 't morphism * 't eq
(* Return an equality between the normalized sides *)
val normalizeEq : 't eq -> 't eq
val isNormal : 't morphism -> bool

