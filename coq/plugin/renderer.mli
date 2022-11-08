
val cat     : Evd.evar_map -> Environ.env -> Hott.t Data.category      -> Pp.t
val funct   : Evd.evar_map -> Environ.env -> Hott.t Data.funct         -> Pp.t
val elem    : Evd.evar_map -> Environ.env -> Hott.t Data.elem          -> Pp.t
val mph     : Evd.evar_map -> Environ.env -> Hott.t Data.morphism      -> Pp.t
val mphList : Evd.evar_map -> Environ.env -> Hott.t Data.morphism list -> Pp.t
val eq      : Evd.evar_map -> Environ.env -> Hott.t Data.eq            -> Pp.t

val to_graphviz : Evd.evar_map -> Environ.env -> (Pp.t,Hott.t) Store.Make(Hott.M).t
