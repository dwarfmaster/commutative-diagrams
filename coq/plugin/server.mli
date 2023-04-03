
type action =
  | Graph of string option * bool * Data.morphism * Data.morphism
  | Normalize of Data.morphism * Data.morphism
  | Print of string
  | Solve of int * Data.morphism * Data.morphism
val run : action -> unit Hyps.t
