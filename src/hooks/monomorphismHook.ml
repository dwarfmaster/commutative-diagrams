
module Make(PA : Pa.ProofAssistant) = struct
  open Data

  let rec split_last mph =
    match mph with
    | Comp (m1,AtomicMorphism m2) -> m1, Some m2
    | Comp (m1,m2) -> let m,last = split_last m2 in Comp (m1,m), last
    | mph -> mph, None

  let last_eq main last =
    let _, eq = Normalisation.normalizeMorphism (Comp (main, AtomicMorphism last)) in eq

  let hook eq =
    let ml, last_l = split_last (eq_left eq) in
    let mr, last_r = split_last (eq_right eq) in
    match last_l, last_r with
    | Some ll, Some lr when ll.mph_id = lr.mph_id ->
        begin match ll.mono with
        | Some mono ->
            let eql = last_eq ml ll in
            let eqr = last_eq mr lr in 
            [ (Mono (mono,ml,mr, Concat (eql, Concat (eq, InvEq eqr)))) ]
        | _ -> []
        end
    | _, _ -> []

end
