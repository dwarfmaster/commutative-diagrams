
exception Object_not_found of string

val app : EConstr.t Proofview.tactic -> EConstr.t array -> EConstr.t Proofview.tactic

val is_cat_ob_mor_from_data : Names.Constant.t -> bool
val mk_cat_ob_mor_from_data : unit -> EConstr.t Proofview.tactic
val is_cat_data_from_precat : Names.Constant.t -> bool
val mk_cat_data_from_precat : unit -> EConstr.t Proofview.tactic
val is_cat  : Names.Constant.t -> bool
val mk_cat  : unit -> EConstr.t Proofview.tactic
val is_functor : Names.Constant.t -> bool
val mk_functor : unit -> EConstr.t Proofview.tactic
val is_funct_obj : Names.Constant.t -> bool
val mk_funct_obj : unit -> EConstr.t Proofview.tactic
val is_funct_mph : Names.Constant.t -> bool
val mk_funct_mph : unit -> EConstr.t Proofview.tactic
val mk_funct_id : unit -> EConstr.t Proofview.tactic
val mk_funct_comp : unit -> EConstr.t Proofview.tactic
val mk_funct_ctx : unit -> EConstr.t Proofview.tactic
val is_funct_data_from_funct : Names.Constant.t -> bool
val mk_funct_data_from_funct : unit -> EConstr.t Proofview.tactic

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
val is_comp : Names.Constant.t -> bool
val mk_comp : unit -> EConstr.t Proofview.tactic
val is_mphT : Names.Constant.t -> bool
val mk_mphT : unit -> EConstr.t Proofview.tactic
val mk_lap : unit -> EConstr.t Proofview.tactic
val mk_rap : unit -> EConstr.t Proofview.tactic
val is_object : Names.Constant.t -> bool
val mk_object : unit -> EConstr.t Proofview.tactic

val is_projection : Names.Projection.t -> (Names.inductive -> bool) -> string -> bool
val build_const : Names.Constant.t -> EConstr.t Proofview.tactic
val build_constr : Names.constructor -> EConstr.t Proofview.tactic
val build_ind : Names.inductive -> EConstr.t Proofview.tactic

val whd : EConstr.t -> EConstr.t Proofview.tactic
