
(* t is mutable *)
type t
type query = Data.elem * (Data.morphism,query) Data.pathComponent list

val mkQuery : Data.path -> query

val init       : query list -> t Proofview.tactic
(* Returns false if nothing was done *)
val connect    : query -> query -> Data.eq -> t -> bool Proofview.tactic
val query_conn : query -> query -> t -> Data.eq option Proofview.tactic
