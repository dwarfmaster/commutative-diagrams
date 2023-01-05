
module Make(PA: Pa.ProofAssistant) : sig
  val run : PA.t Data.morphism -> PA.t Data.morphism -> (unit, PA.t) Hyps.Make(PA.M).t
end
