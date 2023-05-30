
type result =
  | Success of Msgpack.t
  | Failure of string
  | Terminate of bool * Msgpack.t
type state =
  { goal: Graph.graph
  ; lemmas: Lemmas.t array
  }

val goal : state -> Msgpack.t list -> result Hyps.t
val info : state -> Msgpack.t list -> result Hyps.t
val unify : state -> Msgpack.t list -> result Hyps.t
val equalify : state -> Msgpack.t list -> result Hyps.t
val lemmas : state -> Msgpack.t list -> result Hyps.t
val instantiate : state -> Msgpack.t list -> result Hyps.t
val query : state -> Msgpack.t list -> result Hyps.t
val build : state -> Msgpack.t list -> result Hyps.t
val parse : state -> Msgpack.t list -> result Hyps.t
val saveState : state -> Msgpack.t list -> result Hyps.t
val restoreState : state -> Msgpack.t list -> result Hyps.t
val finish : state -> Msgpack.t list -> result Hyps.t
