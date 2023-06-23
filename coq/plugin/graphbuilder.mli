
module Make(O: Map.OrderedType) : sig
  type t
  val empty : unit -> t
  val add_node : (*cat*)O.t -> (*obj*)O.t -> t -> int*t
  val add_edge : (*cat*)O.t -> (*src*)O.t -> (*dst*)O.t -> (*mph*)O.t -> t -> int*t
  (* Faces may be ignored if they are not valid when building. However, if they are marked
     as important, the building step will fail *)
  val add_face : ?important:bool
              -> (*cat*)O.t
              -> (*src*)O.t
              -> (*dst*)O.t
              -> (*left*)(O.t*O.t*O.t) list
              -> (*right*)(O.t*O.t*O.t) list
              -> O.t
              -> t -> t
  val import : int -> (*object*)O.t -> (*type*)Hyps.obj -> (Hyps.obj -> O.t Hyps.t) -> t -> t Hyps.t
  val build : t -> O.t Graph.graph_impl option
  (* If no face has been marked as important, the building step cannot fail, so we expose
     this convenience function *)
  val build_unsafe : t -> O.t Graph.graph_impl
  val debug_print : Environ.env -> Evd.evar_map -> (O.t -> Pp.t) -> t -> Pp.t
end

type t
val empty : unit -> t
val add_node : (*cat*)int -> (*obj*)int -> t -> int*t
val add_edge : (*cat*)int -> (*src*)int -> (*dst*)int -> (*mph*)int -> t -> int*t
(* Faces may be ignored if they are not valid when building. However, if they are marked
   as important, the building step will fail *)
val add_face : ?important:bool
            -> (*cat*)int
            -> (*src*)int
            -> (*dst*)int
            -> (*left*)(int*int*int) list
            -> (*right*)(int*int*int) list
            -> int
            -> t -> t
val import : int -> int -> t -> t Hyps.t
val build : t -> Graph.graph option
(* If no face has been marked as important, the building step cannot fail, so we expose
   this convenience function *)
val build_unsafe : t -> Graph.graph
val debug_print : Environ.env -> Evd.evar_map -> t -> Pp.t
