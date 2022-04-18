
type t = private
  { src : Morphisms.Make(Utils.CLEConstr).morphism
  ; dst : Morphisms.Make(Utils.CLEConstr).morphism
  ; tp  : Morphisms.Make(Utils.CLEConstr).morphismT
  ; eq  : EConstr.t
  }

exception Ill_typed

(* a -> a =_A a *)
val refl : Evd.evar_map -> Environ.env -> Morphisms.Make(Utils.CLEConstr).morphism -> t
(* a = b -> b = c -> a = c *)
val concat : Evd.evar_map -> Environ.env -> t -> t -> t
(* a = b -> b = a *)
val inv : Evd.evar_map -> Environ.env -> t -> t
(* m1 = m2 -> m1' = m2' -> m1 o m1' = m2 o m2' *)
val compose : Evd.evar_map -> Environ.env -> t -> t -> t
(* m1 -> m2 -> m3 -> m3 o (m2 o m1) = (m3 o m2) o m1 *)
val assoc : Evd.evar_map -> Environ.env -> Morphisms.Make(Utils.CLEConstr).morphism -> Morphisms.Make(Utils.CLEConstr).morphism -> Morphisms.Make(Utils.CLEConstr).morphism -> t
(* m -> id o m = m *)
val left_id : Evd.evar_map -> Environ.env -> Morphisms.Make(Utils.CLEConstr).morphism -> t
(* m -> m o id = m *)
val right_id : Evd.evar_map -> Environ.env -> Morphisms.Make(Utils.CLEConstr).morphism -> t
(* Split morphism along compositions, remove identities, and give equality *)
val normalize : Evd.evar_map -> Environ.env -> Morphisms.Make(Utils.CLEConstr).morphism -> Morphisms.Make(Utils.CLEConstr).morphism list * t
