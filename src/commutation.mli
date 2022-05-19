
type t

(* Enumerate all paths and initialize the solver with those paths *)
(* Then add all the equalities *)
val build : Hyps.t -> int -> t Proofview.tactic
val query : Hyps.path -> Hyps.path -> t -> Hyps.eq option Proofview.tactic
val normalize_iso_in_path : Hyps.path -> Hyps.path Proofview.tactic
