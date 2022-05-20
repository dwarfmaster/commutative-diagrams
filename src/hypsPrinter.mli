
val cat     : Evd.evar_map -> Environ.env -> Data.category      -> Pp.t
val elem    : Evd.evar_map -> Environ.env -> Data.elem          -> Pp.t
val mphT    : Evd.evar_map -> Environ.env -> Data.morphismT     -> Pp.t
val mphDt   : Evd.evar_map -> Environ.env -> Data.morphismData  -> Pp.t
val mph     : Evd.evar_map -> Environ.env -> Data.morphism      -> Pp.t
val mphList : Evd.evar_map -> Environ.env -> Data.morphism list -> Pp.t
val eq      : Evd.evar_map -> Environ.env -> Data.eq            -> Pp.t
val path    : Evd.evar_map -> Environ.env -> Data.path          -> Pp.t
val face    : Evd.evar_map -> Environ.env -> Data.face          -> Pp.t

val to_graphviz : Evd.evar_map -> Environ.env -> Hyps.t -> Pp.t
