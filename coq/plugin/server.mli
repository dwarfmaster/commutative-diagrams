
module Make(PA: Pa.ProofAssistant) : sig
  type action =
    | Graph of string option * bool * PA.t Data.morphism * PA.t Data.morphism
    | Normalize of PA.t Data.morphism * PA.t Data.morphism
    | Print of string
    | Solve of int * PA.t Data.morphism * PA.t Data.morphism
  val run : action -> (unit, PA.t) Hyps.Make(PA.M).t
end
