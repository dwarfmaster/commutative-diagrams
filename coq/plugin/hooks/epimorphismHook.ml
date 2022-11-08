
module Make(PA : Pa.ProofAssistant) = struct
  open Data

  let hook eq = 
    let left = eq_left eq in
    let right = eq_right eq in
    match left, right with
    | Comp (AtomicMorphism el, l), Comp (AtomicMorphism er, r) when el.mph_id = er.mph_id ->
        begin match el.epi with
        | Some epi -> [ (Epi (epi,l,r,eq)) ]
        | _ -> []
        end
    | _, _ -> []

end
