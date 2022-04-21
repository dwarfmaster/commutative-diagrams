
val cat   : Evd.evar_map -> Environ.env -> Hyps.category     -> Pp.t
val elem  : Evd.evar_map -> Environ.env -> Hyps.elem         -> Pp.t
val mphT  : Evd.evar_map -> Environ.env -> Hyps.morphismT    -> Pp.t
val mphDt : Evd.evar_map -> Environ.env -> Hyps.morphismData -> Pp.t
val mph   : Evd.evar_map -> Environ.env -> Hyps.morphism     -> Pp.t
val eq    : Evd.evar_map -> Environ.env -> Hyps.eq           -> Pp.t
val path  : Evd.evar_map -> Environ.env -> Hyps.path         -> Pp.t
val face  : Evd.evar_map -> Environ.env -> Hyps.face         -> Pp.t

val to_graphviz : Evd.evar_map -> Environ.env -> Hyps.t -> Pp.t
