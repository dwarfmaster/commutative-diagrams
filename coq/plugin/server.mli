
module Make(PA: Pa.ProofAssistant) : sig
  val run : PA.t Data.morphism -> PA.t Data.morphism -> PA.t Data.eq Proofview.tactic
end
