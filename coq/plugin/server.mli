
type action =
  | Graph of string option * bool * Data.morphism * Data.morphism * Lemmas.t list
  | Normalize of Data.morphism * Data.morphism
  | Print of string
  | Solve of int * Data.morphism * Data.morphism
val run : action -> unit Hyps.t
