
module Make(PA : Pa.ProofAssistant) : sig
  val hook : UnionFind.Make(PA).hook
end
