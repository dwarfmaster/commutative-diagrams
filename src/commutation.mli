
type t

(* Enumerate all paths and initialize the solver with those paths *)
(* Then add all the equalities *)
val build : Hyps.t -> int -> t Proofview.tactic
val query : Data.path -> Data.path -> t -> Data.eq option Proofview.tactic
val normalize_iso_in_path : Data.path -> Data.path Proofview.tactic
