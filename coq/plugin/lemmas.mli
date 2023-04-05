
type lemmaTerm =
  | ConstLemma of Names.Constant.t
  | VarLemma of Names.Id.t

type lemma =
  { name: string
  ; term: lemmaTerm
  ; graph: Graph.graph
  }

val extractFromType : lemmaTerm -> EConstr.t -> lemma option Hyps.t
val extractAllConstants : unit -> lemma list Hyps.t
