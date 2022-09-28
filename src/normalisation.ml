
open Data

let applyFunctors functs m =
  List.fold_left (fun m f -> FMph (f,m)) m functs

(* raiseEquality [ f1 f2 f3 ... ] e
   -> ... (f3 (f2 (f1 e))), ... (f3 (f2 (f1 (id e)))) = id (... (f3 (f2 (f1 e)))) *)
let raiseEquality functs e =
  List.fold_left
    (fun (e,eq) f -> FObj (f,e), Concat (FCtx (f,eq), FId (f,e)))
    (e, Refl (Identity e)) functs

let raiseComp functs m1 m2 =
  List.fold_left 
    (fun (m1,m2,eq) f -> FMph (f,m1), FMph (f,m2), Concat (FCtx (f,eq), FComp (f,m1,m2)))
    (m1,m2,Refl (Comp (m1,m2))) functs

let rec normUnderFunctors functs m =
  match m with
  | Identity e -> let e, eq = raiseEquality functs e in Identity e, eq
  | Comp (m1, m2) ->
      let _, _, eq = raiseComp functs m1 m2 in
      let m1, eq1 = normUnderFunctors functs m1 in 
      let m2, eq2 = normUnderFunctors functs m2 in
      let eq = Concat (eq, Compose (eq1, eq2)) in
      begin match m1, m2 with
      | Identity e, _ -> m2, Concat (eq, LeftId m2)
      | _, Identity e -> m1, Concat (eq, RightId m1)
      | Comp (m11,m12), _ -> Comp(m11,Comp(m12,m2)), Concat (eq, Assoc (m11,m12,m2))
      | _ -> Comp (m1, m2), eq
      end
  | FMph (f, m) -> normUnderFunctors (f :: functs) m
  | Inv m ->
      begin match m with
      | AtomicMorphism m ->
          begin match m.iso with
          | Some iso ->
              let m = if Data.cmp_morphism iso.iso_mph (AtomicMorphism m) = 0
                      then iso.iso_inv
                      else iso.iso_mph in
              let m = applyFunctors functs m in 
              m, Refl m
          | None -> assert false (* Shouldn't happen *)
          end
      | _ -> assert false (* Unsupported *)
      end
  | _ -> let m = applyFunctors functs m in m, Refl m

let normalizeMorphism m = normUnderFunctors [] m

