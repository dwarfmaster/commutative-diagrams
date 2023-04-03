
val cat     : Evd.evar_map -> Environ.env -> Data.category      -> Pp.t
val funct   : Evd.evar_map -> Environ.env -> Data.funct         -> Pp.t
val elem    : Evd.evar_map -> Environ.env -> Data.elem          -> Pp.t
val mph     : Evd.evar_map -> Environ.env -> Data.morphism      -> Pp.t
val mphList : Evd.evar_map -> Environ.env -> Data.morphism list -> Pp.t
val eq      : Evd.evar_map -> Environ.env -> Data.eq            -> Pp.t

val to_graphviz : unit -> Pp.t Hyps.t
