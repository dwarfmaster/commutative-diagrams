
open Data

let rec simpl_inv inv eq =
  match eq with
  | Hole (m1,m2) -> if inv then Hole (m2,m1) else Hole (m1,m2)
  | Refl m -> Refl m
  | Concat (eq1,eq2) ->
      let eq1 = simpl_inv inv eq1 in
      let eq2 = simpl_inv inv eq2 in
      begin match eq1, eq2 with
      | Refl m, Refl _ -> Refl m
      | Refl _, eq2 -> eq2
      | eq1, Refl _ -> eq1
      | _, _ -> if inv then Concat (eq2,eq1) else Concat (eq1,eq2) end
  | InvEq eq -> simpl_inv (not inv) eq
  | Compose (eq1, eq2) ->
      let eq1 = simpl_inv inv eq1 in
      let eq2 = simpl_inv inv eq2 in
      (* TODO handle eq @ eq^-1 --> 1 *)
      begin match eq1, eq2 with
      | Refl m1, Refl m2 -> Refl (Comp (m1,m2))
      | Refl m1, eq2 -> LAp (m1, eq2)
      | eq1, Refl m2 -> RAp (eq1, m2)
      | eq1, InvEq eq2 when cmp_eq eq1 eq2 = 0 -> Refl (eq_left eq1)
      | InvEq eq1, eq2 when cmp_eq eq1 eq2 = 0 -> Refl (eq_right eq1)
      | _, _ -> Compose (eq1, eq2)
      end
  | RAp (eq,m) ->
      let eq = simpl_inv inv eq in
      begin match eq with
      | Refl m1 -> Refl (Comp (m1,m))
      | _ -> RAp (eq,m)
      end
  | LAp (m,eq) ->
      let eq = simpl_inv inv eq in
      begin match eq with
      | Refl m2 -> Refl (Comp (m,m2))
      | _ -> LAp (m,eq)
      end
  | Mono (mono,m1,m2,eq) ->
      if inv then Mono (mono,m2,m1,simpl_inv inv eq)
             else Mono (mono,m1,m2,simpl_inv inv eq)
  | Epi (epi,m1,m2,eq) ->
      if inv then Epi (epi,m2,m1,simpl_inv inv eq)
             else Epi (epi,m1,m2,simpl_inv inv eq)
  | FCtx (f,eq) ->
      let eq = simpl_inv inv eq in
      begin match eq with
      | Refl m -> Refl (FMph (f,m))
      | _ -> FCtx (f,eq)
      end
  | _ -> if inv then InvEq eq else eq

let simpl eq = simpl_inv false eq
