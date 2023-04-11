
type lemma =
  { name: string
  ; graph: Graph.graph
  }

val extractFromVar : Names.Id.t -> EConstr.t -> lemma option Hyps.t
val extractAllConstants : unit -> lemma list Hyps.t
