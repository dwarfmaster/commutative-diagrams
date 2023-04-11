
type t
val empty : unit -> t
val add_node : Data.elem -> t -> int*t
val add_edge : Data.morphism -> t -> t
val add_face : Data.eq -> t -> t
val import_hyps : t -> t Hyps.t
val build : t -> Graph.graph
val debug_print : Environ.env -> Evd.evar_map -> t -> Pp.t
