
module Make(PA : Pa.ProofAssistant) : sig
  val hook : PA.t Data.morphismData -> UnionFind.Make(PA).hook
end
