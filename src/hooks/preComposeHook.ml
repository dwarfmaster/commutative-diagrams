
module Make(PA : Pa.ProofAssistant) = struct
  open Data

  let hook mph eq = 
    if cmp_elem mph.mph_dst_ (eq_src eq) = 0
    then [ Normalisation.normalizeEq (LAp (AtomicMorphism mph,eq)) ]
    else []

end
