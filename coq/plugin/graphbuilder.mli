
type t
val empty : unit -> t
val add_node : Data.elem -> t -> int*t
val add_edge : Data.morphism -> t -> t
(* Faces may be ignored if they are not valid when building. However, if they are marked
   as important, the building step will fail *)
val add_face : ?important:bool -> Data.eq -> t -> t
val import_hyps : t -> t Hyps.t
val build : t -> Graph.graph option
(* If no face has been marked as important, the building step cannot fail, so we expose
   this convenience function *)
val build_unsafe : t -> Graph.graph
val debug_print : Environ.env -> Evd.evar_map -> t -> Pp.t
