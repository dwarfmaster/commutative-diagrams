
exception Object_not_found of string

val locate_inductive : string -> Names.inductive

val is_cat : Names.inductive -> bool
val is_eq  : Names.inductive -> bool

val is_projection : Names.Projection.t -> (Names.inductive -> bool) -> string -> bool
