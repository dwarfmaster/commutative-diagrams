
module type ConstrLike = sig
  type constr
  type types
  type sorts
  type univs

  val kind : Evd.evar_map -> Environ.env -> constr -> (constr,types,sorts,univs) Constr.kind_of_term
  val print : Evd.evar_map -> Environ.env -> constr -> Pp.t
  val econstr : constr -> EConstr.t
  val try_solve : Evd.evar_map -> constr -> Constr.t option
end

module CLConstr : sig
  type constr = Constr.t
  type types = Constr.t
  type sorts = Sorts.t
  type univs = Univ.Instance.t

  val kind : Evd.evar_map -> Environ.env -> constr -> (constr,types,sorts,univs) Constr.kind_of_term
  val print : Evd.evar_map -> Environ.env -> constr -> Pp.t
  val econstr : constr -> EConstr.t
  val try_solve : Evd.evar_map -> constr -> Constr.t option
end

module CLEConstr : sig
  type constr = EConstr.t
  type types = EConstr.t
  type sorts = EConstr.ESorts.t
  type univs = EConstr.EInstance.t

  val kind : Evd.evar_map -> Environ.env -> constr -> (constr,types,sorts,univs) Constr.kind_of_term
  val print : Evd.evar_map -> Environ.env -> constr -> Pp.t
  val econstr : constr -> EConstr.t
  val try_solve : Evd.evar_map -> constr -> Constr.t option
end
