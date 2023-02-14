
module Make(PA: Pa.ProofAssistant) = struct
  module St = Hyps.Make(PA.M)
  open St.Combinators
  open Data

  let instantiateAtomic atom tp =
    match atom with
    | Evar (i,None) ->
        let* status = St.getEvar i in
        let* evar = match status with
        | Realized ev -> ret ev
        | Abstract ->
            let* evar = PA.realizeEvar tp |> lift in
            let* _ = St.instantiateEvar i evar in
            ret evar
        | NotFound ->
            let* evar = PA.realizeEvar tp |> lift in
            let* _ = St.newEvarAt i in
            let* _ = St.instantiateEvar i evar in
            ret evar in
        ret (Evar (i, Some evar))
    | _ -> ret atom

  let instantiateCat cat =
    match cat with
    | AtomicCategory data ->
        let* tp = PA.realizeCatType data |> lift in
        let* atom = instantiateAtomic data.cat_obj tp in
        AtomicCategory { data with cat_obj = atom } |> ret

  let instantiateFunct funct =
    match funct with
    | AtomicFunctor data ->
        let* tp = PA.realizeFunctType data |> lift in
        let* atom = instantiateAtomic data.funct_obj tp in
        AtomicFunctor { data with funct_obj = atom } |> ret

  let rec instantiateElem elem =
    match elem with
    | AtomicElem data ->
        let* tp = PA.realizeElemType data |> lift in
        let* atom = instantiateAtomic data.elem_obj tp in
        AtomicElem { data with elem_obj = atom } |> ret
    | FObj (f,o) ->
        let* f = instantiateFunct f in
        let* o = instantiateElem o in
        FObj (f,o) |> ret

  let rec instantiateMorphism mph =
    match mph with
    | AtomicMorphism data ->
        let* tp = PA.realizeMphType data |> lift in
        let* atom = instantiateAtomic data.mph_obj tp in
        AtomicMorphism { data with mph_obj = atom } |> ret
    | Identity o ->
        let* o = instantiateElem o in
        Identity o |> ret
    | Comp (m1,m2) ->
        let* m1 = instantiateMorphism m1 in
        let* m2 = instantiateMorphism m2 in
        Comp (m1,m2) |> ret
    | Inv m ->
        let* m = instantiateMorphism m in
        Inv m |> ret
    | FMph (f,m) ->
        let* f = instantiateFunct f in
        let* m = instantiateMorphism m in
        FMph (f,m) |> ret

  let rec instantiateEq eq =
    match eq with
    | AtomicEq data ->
        let* tp = PA.realizeEqType data |> lift in
        let* atom = instantiateAtomic data.eq_obj tp in
        AtomicEq { data with eq_obj = atom } |> ret
    | Refl m ->
        let* m = instantiateMorphism m in
        Refl m |> ret
    | Concat (p1,p2) ->
        let* p1 = instantiateEq p1 in
        let* p2 = instantiateEq p2 in
        Concat (p1,p2) |> ret
    | InvEq p ->
        let* p = instantiateEq p in
        InvEq p |> ret
    | Compose (p1,p2) ->
        let* p1 = instantiateEq p1 in
        let* p2 = instantiateEq p2 in
        Compose (p1,p2) |> ret
    | Assoc (m1,m2,m3) ->
        let* m1 = instantiateMorphism m1 in
        let* m2 = instantiateMorphism m2 in
        let* m3 = instantiateMorphism m3 in
        Assoc (m1,m2,m3) |> ret
    | LeftId m ->
        let* m = instantiateMorphism m in
        LeftId m |> ret
    | RightId m ->
        let* m = instantiateMorphism m in
        RightId m |> ret
    | RAp (p,m) ->
        let* p = instantiateEq p in
        let* m = instantiateMorphism m in
        RAp (p,m) |> ret
    | LAp (m,p) ->
        let* m = instantiateMorphism m in
        let* p = instantiateEq p in
        LAp (m,p) |> ret
    | RInv i -> RInv i |> ret
    | LInv i -> LInv i |> ret
    | Mono (mono,m1,m2,p) ->
        let* m1 = instantiateMorphism m1 in
        let* m2 = instantiateMorphism m2 in
        let* p = instantiateEq p in
        Mono (mono,m1,m2,p) |> ret
    | Epi (epi,m1,m2,p) ->
        let* m1 = instantiateMorphism m1 in
        let* m2 = instantiateMorphism m2 in
        let* p = instantiateEq p in
        Epi (epi,m1,m2,p) |> ret
    | FId (f,o) ->
        let* f = instantiateFunct f in
        let* o = instantiateElem o in
        FId (f,o) |> ret
    | FComp (f,m1,m2) ->
        let* f = instantiateFunct f in
        let* m1 = instantiateMorphism m1 in
        let* m2 = instantiateMorphism m2 in
        FComp (f,m1,m2) |> ret
    | FCtx (f,p) ->
        let* f = instantiateFunct f in
        let* p = instantiateEq p in
        FCtx (f,p) |> ret

  (* Wrappers over PA.realize* that instantiate all evars before doing the realization *)
  let realizeCategory cat =
    let* cat = instantiateCat cat in
    PA.realizeCategory cat |> lift
  let realizeFunctor funct =
    let* funct = instantiateFunct funct in
    PA.realizeFunctor funct |> lift
  let realizeElem elem =
    let* elem = instantiateElem elem in
    PA.realizeElem elem |> lift
  let realizeMorphism mph =
    let* mph = instantiateMorphism mph in
    PA.realizeMorphism mph |> lift
  let realizeEq eq = 
    let* eq = instantiateEq eq in
    PA.realizeEq eq |> lift
end
