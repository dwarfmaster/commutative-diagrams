
type t = EConstr.t



(*   ____      _                        _ *)
(*  / ___|__ _| |_ ___  __ _  ___  _ __(_) ___  ___ *)
(* | |   / _` | __/ _ \/ _` |/ _ \| '__| |/ _ \/ __| *)
(* | |__| (_| | ||  __/ (_| | (_) | |  | |  __/\__ \ *)
(*  \____\__,_|\__\___|\__, |\___/|_|  |_|\___||___/ *)
(*                     |___/ *)
(* Categories *)

val is_cat : Environ.env -> t -> bool Proofview.tactic
(* Gives the category of the object *)
val is_object : Environ.env -> t -> t option Proofview.tactic
(* cat -> src -> dst -> tp *)
val morphism : Environ.env -> t -> t -> t -> t Proofview.tactic
(* mphT -> cat*src*dst *)
val is_morphism : Environ.env -> t -> (t*t*t) option Proofview.tactic
(* cat -> src -> mid -> dst -> mph cat src mid -> mph cat mid dst -> mph cat src dst *)
val compose : Environ.env -> t -> t -> t -> t -> t -> t -> t Proofview.tactic
(* mph -> cat*src*mid*dst*m1*m2 *)
val parse_compose : Environ.env -> t -> (t*t*t*t*t*t) option Proofview.tactic
(* cat -> x -> 1_x *)
val identity : Environ.env -> t -> t -> t Proofview.tactic
(* mph -> cat*x *)
val parse_identity : Environ.env -> t -> (t*t) option Proofview.tactic

(* mono -> cat*src*dst*mph *)
val is_mono : Environ.env -> t -> (t*t*t*t) option Proofview.tactic
(* epi -> cat*src*dst*mph *)
val is_epi  : Environ.env -> t -> (t*t*t*t) option Proofview.tactic
(* iso -> cat*src*dst*mph *)
val is_iso  : Environ.env -> t -> (t*t*t*t) option Proofview.tactic
(* cat -> src -> dst -> mph -> iso -> inv *)
val inverse : Environ.env -> t -> t -> t -> t -> t -> t Proofview.tactic



(*  ____       _   _          *)
(* |  _ \ __ _| |_| |__  ___  *)
(* | |_) / _` | __| '_ \/ __| *)
(* |  __/ (_| | |_| | | \__ \ *)
(* |_|   \__,_|\__|_| |_|___/ *)
(* Paths *)
(* T x y *)
val eq : Environ.env -> t -> t -> t -> t Proofview.tactic
(* eqT -> T*x*y *)
val is_eq : Environ.env -> t -> (t*t*t) option Proofview.tactic
(* Take the description of an equality and give the associated term *)
val real_eq : Data.eq -> t Proofview.tactic
