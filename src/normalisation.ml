
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

let rec postCompose' m post =
  match m with
  | Identity e -> post, RightId post
  | Comp (m1,m2) ->
      let r, req = postCompose' m2 post in
      Comp (m1, r), Concat (Assoc (m1,m2,post), LAp (m1, req))
  | _ -> Comp (m,post), Refl (Comp (m,post))
let postCompose m post =
  match post with
  | Identity e -> m, LeftId m
  | _ -> postCompose' m post

let rec normUnderFunctors functs m =
  match m with
  | Identity e -> let e, eq = raiseEquality functs e in Identity e, eq
  | Comp (m1, m2) ->
      let _, _, eq = raiseComp functs m1 m2 in
      let m1, eq1 = normUnderFunctors functs m1 in 
      let m2, eq2 = normUnderFunctors functs m2 in
      let eq = Concat (eq, Compose (eq1, eq2)) in
      let r, req = postCompose m1 m2 in 
      r, Concat (eq, req)
  | FMph (f, m) -> normUnderFunctors (f :: functs) m
  | Inv m ->
      begin match m with
      | AtomicMorphism m ->
          begin match m.iso with
          | Some iso ->
              let m = if iso.iso_mph.mph_id = m.mph_id
                      then iso.iso_inv
                      else iso.iso_mph in
              let m = applyFunctors functs (AtomicMorphism m) in 
              m, Refl m
          | None -> assert false (* Shouldn't happen *)
          end
      | _ -> assert false (* Unsupported *)
      end
  | _ -> let m = applyFunctors functs m in m, Refl m

let normalizeMorphism m = normUnderFunctors [] m

let normalizeEq eq =
  let _, eq1 = normalizeMorphism (eq_left eq) in
  let _, eq2 = normalizeMorphism (eq_right eq) in
  Concat (InvEq eq1, Concat (eq, eq2))

(* Check if there is a redex at head for performance, otherwise do the reduction and compare *)
let isNormal m =
  match m with
  | Identity _ -> true
  | Comp (Comp (_,_), _) -> false
  | Inv _ -> false
  | Comp (Identity _, _) -> false
  | Comp (_, Identity _) -> false
  | FMph (_, Comp (_,_)) -> false 
  | FMph (_, Identity _) -> false
  | _ ->
      let (red,_) = normalizeMorphism m in
      Data.cmp_morphism m red = 0

