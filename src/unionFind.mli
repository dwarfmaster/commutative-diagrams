
(* t is mutable *)
type path = Data.elem * Data.morphism list
type t

val extract    : Data.path -> path
val init       : path list -> t Proofview.tactic
(* Returns false if nothing was done *)
val connect    : path -> path -> Data.eq -> t -> bool Proofview.tactic
val query      : path -> t -> (path * Data.eq) Proofview.tactic
val query_conn : path -> path -> t -> Data.eq option Proofview.tactic
(* Utilities *)
val print_path : path -> Pp.t Proofview.tactic
