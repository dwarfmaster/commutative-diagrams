
module Make(PA: Pa.ProofAssistant) : sig
  (* Wrappers over PA.realize* that instantiate all evars before doing the realization *)
  val realizeCategory : PA.t Data.category -> (PA.t, PA.t) Hyps.Make(PA.M).t
  val realizeFunctor : PA.t Data.funct -> (PA.t, PA.t) Hyps.Make(PA.M).t
  val realizeElem : PA.t Data.elem -> (PA.t, PA.t) Hyps.Make(PA.M).t
  val realizeMorphism : PA.t Data.morphism -> (PA.t, PA.t) Hyps.Make(PA.M).t
  val realizeEq : PA.t Data.eq -> (PA.t, PA.t) Hyps.Make(PA.M).t
end
