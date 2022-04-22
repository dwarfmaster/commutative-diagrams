
exception Object_not_found of string

val app : EConstr.t Proofview.tactic -> EConstr.t array -> EConstr.t Proofview.tactic

val is_cat  : Names.inductive -> bool
val mk_cat  : unit -> EConstr.t Proofview.tactic

val is_eq   : Names.inductive -> bool
val mk_eq   : unit -> EConstr.t Proofview.tactic
val is_refl : Names.constructor -> bool
val mk_refl : unit -> EConstr.t Proofview.tactic
val is_concat : Names.Constant.t -> bool
val mk_concat : unit -> EConstr.t Proofview.tactic
val mk_inv : unit -> EConstr.t Proofview.tactic
val mk_compose_eq : unit -> EConstr.t Proofview.tactic
val mk_assoc : unit -> EConstr.t Proofview.tactic
val mk_left_id : unit -> EConstr.t Proofview.tactic
val mk_right_id : unit -> EConstr.t Proofview.tactic
val is_id : Names.Constant.t -> bool
val mk_id : unit -> EConstr.t Proofview.tactic
val mk_comp : unit -> EConstr.t Proofview.tactic
val mk_mphT : unit -> EConstr.t Proofview.tactic

val is_projection : Names.Projection.t -> (Names.inductive -> bool) -> string -> bool
