
module Make(PA: Pa.ProofAssistant) : sig
  type action =
    | Graph
    | Normalize
  val run : action -> PA.t Data.morphism -> PA.t Data.morphism -> (unit, PA.t) Hyps.Make(PA.M).t
end
