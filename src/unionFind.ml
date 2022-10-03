
module Make(PA : Pa.ProofAssistant) = struct

  module Enum = Enumerate.Make(PA)
  module M = Map.Make(Data.EqMph(PA))

  type hook = PA.t Data.eq -> PA.t Data.eq list
  type cell =
    { mutable parent : int
    ; mutable rank   : int
    ; mph            : PA.t Data.morphism
    ; mutable eq     : PA.t Data.eq
    }
  type t =
    { cells : cell array
    ; morphisms : Enum.enumeration
    ; mutable hooks : hook list
    }
  
  let initCell id mph =
    { parent = id
    ; rank   = 1
    ; mph    = mph
    ; eq     = Data.Refl mph
    }
  
  let init (enum : Enum.enumeration) =
    { cells = Array.mapi initCell enum.paths
    ; morphisms = enum 
    ; hooks = [] }

  let registerHook uf hk = uf.hooks <- hk :: uf.hooks
  
  let rec find (id : int) cells =
    if id = cells.(id).parent
    then (id,cells.(id).eq)
    else
      let pid,peq = find cells.(id).parent cells in
      cells.(id).parent <- pid;
      let eq = Data.Concat (cells.(id).eq, peq) in
      cells.(id).eq <- eq;
      (pid,eq)

  let query m1 m2 uf = 
    let id1,eq1 = find (M.find m1 uf.morphisms.indices) uf.cells in
    let id2,eq2 = find (M.find m2 uf.morphisms.indices) uf.cells in
    if id1 = id2
    then Some (Data.Concat (eq1, Data.InvEq eq2))
    else None
  
  (* Returns true if id1 and id2 were disjoint before *)
  let union id1 id2 eq cells =
    let parent1,eq1 = find id1 cells in
    let parent2,eq2 = find id2 cells in
    if parent1 = parent2 then false
    else
      let eq = Data.Concat (eq1, Data.Concat (eq, Data.InvEq eq2)) in
      if cells.(parent1).rank < cells.(parent2).rank
      then begin
        cells.(parent2).parent <- parent1;
        cells.(parent2).eq     <- Data.InvEq eq;
        cells.(parent1).rank   <- cells.(parent1).rank + cells.(parent2).rank;
        true
      end else begin
        cells.(parent1).parent <- parent2;
        cells.(parent1).eq     <- eq;
        cells.(parent2).rank   <- cells.(parent1).rank + cells.(parent2).rank;
        true
      end

  let rec connectWithHooks eqs uf =
    match eqs with
    | [] -> false
    | eq :: eqs ->
        let p1 = Data.eq_left eq in
        let p2 = Data.eq_right eq in
        let b = union (M.find p1 uf.morphisms.indices) (M.find p2 uf.morphisms.indices) eq uf.cells in
        if b then begin
          let new_eqs = List.concat_map (fun hk -> hk eq) uf.hooks in
          let _ = connectWithHooks (List.append new_eqs eqs) uf in
          true
        end else false
  
  let connect eq uf =
    connectWithHooks [eq] uf

end
