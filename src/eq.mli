
module Make : functor (C : Utils.ConstrLike) -> sig
  type t = private
    { src : EConstr.t
    ; dst : EConstr.t
    ; tp  : EConstr.t
    ; eq  : EConstr.t
    }
  exception Ill_typed

  (* A -> a -> a =_A a *)
  val refl : Evd.evar_map -> Environ.env -> C.constr -> C.constr -> t
  (* a = b -> b = c -> a = c *)
  val concat : Evd.evar_map -> Environ.env -> t -> t -> t
  (* m1 = m2 -> m1' = m2' -> m1 o m1' = m2 o m2' *)
  val compose : Evd.evar_map -> Environ.env -> t -> t -> t
  (* m1 -> m2 -> m3 -> m3 o (m2 o m1) = (m3 o m2) o m1 *)
  val assoc : Evd.evar_map -> Environ.env -> C.constr -> C.constr -> C.constr -> t
  (* m -> id o m = m *)
  val left_id : Evd.evar_map -> Environ.env -> C.constr -> t
  (* m -> m o id = m *)
  val right_id : Evd.evar_map -> Environ.env -> C.constr -> t
  (* [ m1, m2, m3 ] -> (m3 o m2) o m1 *)
  val realize : Evd.evar_map -> Environ.env -> C.constr list -> C.constr
  (* Split morphism along compositions, remove identities, and give equality *)
  val normalize : Evd.evar_map -> Environ.env -> C.constr -> C.constr list * t
end
