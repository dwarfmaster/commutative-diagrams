
module Make(PA : Pa.ProofAssistant) = struct
  open Data

  let hook mph eq = 
    if cmp_elem mph.mph_src_ (eq_dst eq) = 0
    then [ Normalisation.normalizeEq (RAp (eq, AtomicMorphism mph)) ]
    else []

end
