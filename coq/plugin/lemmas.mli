
type t

val extractFromVar : Names.Id.t -> EConstr.t -> t option Hyps.t
val extractAllConstants : unit -> t list Hyps.t
val name : t -> string
val namespace : t -> string
val instantiate : t -> Graph.graph Hyps.t
