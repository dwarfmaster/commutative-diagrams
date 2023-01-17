
module Make(PA : Pa.ProofAssistant) : sig
  type t
  val empty : unit -> t
  val add_node : PA.t Data.elem -> t -> int*t
  val add_edge : PA.t Data.morphism -> t -> t
  val add_face : PA.t Data.eq -> t -> t
  val import_hyps : t -> (t,PA.t) Hyps.Make(PA.M).t
  val build : t -> Graph.Make(PA).graph
end
