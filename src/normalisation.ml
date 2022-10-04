
open Data

let applyFunctors functs m =
  List.fold_left (fun m f -> FMph (f,m)) m functs

(* raiseEquality [ f1 f2 f3 ... ] e
   -> ... (f3 (f2 (f1 e))), ... (f3 (f2 (f1 (id e)))) = id (... (f3 (f2 (f1 e)))) *)
let raiseIdentity functs e =
  List.fold_left
    (fun (e,eq) f -> FObj (f,e), Concat (FCtx (f,eq), FId (f,e)))
    (e, Refl (Identity e)) functs

let raiseComp functs m1 m2 =
  List.fold_left 
    (fun (m1,m2,eq) f -> FMph (f,m1), FMph (f,m2), Concat (FCtx (f,eq), FComp (f,m1,m2)))
    (m1,m2,Refl (Comp (m1,m2))) functs

let mutualInverse m1 m2 =
  match m1, m2 with
  | AtomicMorphism m1, AtomicMorphism m2 ->
      begin match m1.iso with
      | Some iso1 -> iso1.iso_inv.mph_id = m2.mph_id || iso1.iso_mph.mph_id = m2.mph_id
      | _ -> false
      end
  | _ -> false

(* Assumes m1 and m2 are mutual inverse. m1 >> m2 = id *)
let invert m1 m2 =
  match m1, m2 with
  | AtomicMorphism m1, AtomicMorphism m2 ->
      begin match m1.iso with
      | Some iso1 when iso1.iso_mph.mph_id = m1.mph_id -> RInv iso1
      | Some iso1 when iso1.iso_mph.mph_id = m2.mph_id -> LInv iso1
      | _ -> assert false (* Shouldn't happen *)
      end
  | _ -> assert false (* Shouldn't happen *)

(* (m1 >> m2 >> ...) >> post -> m1 >> m2 >> ... >> post, eq *)
(* Assumes m and post are normalised *)
(* Also simplifies inverses *)
let rec postCompose m post =
  match m with
  | Identity e -> post, RightId post
  | Comp (m1,m2) ->
      let r, req = postCompose m2 post in
      let eq = Concat (Assoc (m1,m2,post), LAp (m1, req)) in
      begin match r with
      | Comp (m21, m22) when mutualInverse m1 m21 ->
          m22, Concat (Concat (eq, InvEq (Assoc (m1,m21,m22))),
                       Concat (RAp (invert m1 m21, m22), RightId m22))
      | AtomicMorphism _ when mutualInverse m1 r ->
          Identity (morphism_src m1), Concat (eq, invert m1 r)
      | Identity _ -> m1, Concat (eq, LeftId m1)
      | _ -> Comp(m1,r), eq
      end
  | _ ->
      begin match post with
      | Comp (p1,p2) when mutualInverse m p1 ->
          p2, Concat (InvEq (Assoc (m,p1,p2)), Concat (RAp (invert m p1, p2), RightId p2))
      | AtomicMorphism _ when mutualInverse m post ->
          Identity (morphism_src m), invert m post
      | Identity _ -> m, LeftId m
      | _ -> Comp (m,post), Refl (Comp (m,post))
      end

let rec normUnderFunctors functs m =
  match m with
  | Identity e -> let e, eq = raiseIdentity functs e in Identity e, eq
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

