
module Make(PA : Pa.ProofAssistant) : sig

  (* t is mutable *)
  type t
  (* Init the structure *)
  val init : Enumerate.Make(PA).enumeration -> t

  (* Hooks *)
  type hook = PA.t Data.eq -> PA.t Data.eq list
  (* Hooks are run whenever two things are connected *)
  val registerHook : t -> hook -> unit

  (* Returns false if nothing was done *)
  (* connect assumes the sides of the equality are normalised *)
  (* Both connect and query will abort if the morphisms were not in the enumeration *)
  val connect : PA.t Data.eq -> t -> bool
  val query : PA.t Data.morphism -> PA.t Data.morphism -> t -> PA.t Data.eq option

end
