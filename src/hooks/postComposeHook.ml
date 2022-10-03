
module Make(PA : Pa.ProofAssistant) = struct
  open Data

  let hook mph eq = 
    if cmp_elem (eq_dst eq) mph.mph_src_ = 0
    then [ Normalisation.normalizeEq (RAp (eq, AtomicMorphism mph)) ]
    else []

end
