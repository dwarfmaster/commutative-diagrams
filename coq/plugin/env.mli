
exception Object_not_found of string

val app : EConstr.t Proofview.tactic -> EConstr.t array -> EConstr.t Proofview.tactic

val is_cat  : Names.inductive -> bool
val mk_cat  : unit -> EConstr.t Proofview.tactic
val is_functor : Names.inductive -> bool
val mk_functor : unit -> EConstr.t Proofview.tactic
val mk_funct_obj : unit -> EConstr.t Proofview.tactic
val mk_funct_mph : unit -> EConstr.t Proofview.tactic
val mk_funct_id : unit -> EConstr.t Proofview.tactic
val mk_funct_comp : unit -> EConstr.t Proofview.tactic
val mk_funct_ctx : unit -> EConstr.t Proofview.tactic

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
val mk_lap : unit -> EConstr.t Proofview.tactic
val mk_rap : unit -> EConstr.t Proofview.tactic
val is_mono : Names.Constant.t -> bool
val mk_mono : unit -> EConstr.t Proofview.tactic
val is_epi : Names.Constant.t -> bool
val mk_epi : unit -> EConstr.t Proofview.tactic
val is_iso : Names.inductive -> bool
val mk_iso : unit -> EConstr.t Proofview.tactic
val mk_inv_mph : unit -> EConstr.t Proofview.tactic
val mk_right_inv : unit -> EConstr.t Proofview.tactic
val mk_left_inv : unit -> EConstr.t Proofview.tactic
val mk_object : unit -> EConstr.t Proofview.tactic

val is_projection : Names.Projection.t -> (Names.inductive -> bool) -> string -> bool
val build_const : Names.Constant.t -> EConstr.t Proofview.tactic
val build_constr : Names.constructor -> EConstr.t Proofview.tactic
val build_ind : Names.inductive -> EConstr.t Proofview.tactic

val whd : EConstr.t -> EConstr.t Proofview.tactic
