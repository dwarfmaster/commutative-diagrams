
module Make(PA : Pa.ProofAssistant) : sig

  type enumeration =
    { paths : PA.t Data.morphism array
    ; indices : int Map.Make(Data.EqMph(PA)).t
    }

  val enumerate_paths : ?asrt:bool -> int -> (enumeration,PA.t) Hyps.Make(PA.M).t

end
