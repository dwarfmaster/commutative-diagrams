
exception Object_not_found of string

val locate_inductive : string -> Names.inductive

val is_cat  : Names.inductive -> bool
val mk_cat  : unit -> EConstr.t

val is_eq   : Names.inductive -> bool
val mk_eq   : unit -> EConstr.t
val is_refl : Names.constructor -> bool
val mk_refl : unit -> EConstr.t
val is_concat : Names.Constant.t -> bool
val mk_concat : unit -> EConstr.t
val mk_compose_eq : unit -> EConstr.t
val mk_assoc : unit -> EConstr.t
val mk_left_id : unit -> EConstr.t
val mk_right_id : unit -> EConstr.t

val is_projection : Names.Projection.t -> (Names.inductive -> bool) -> string -> bool
