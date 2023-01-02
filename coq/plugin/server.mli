
module Make(PA: Pa.ProofAssistant) : sig
  val run : PA.t Data.morphism -> PA.t Data.morphism -> (PA.t Data.eq, PA.t) Hyps.Make(PA.M).t
end
