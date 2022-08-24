
type t = EConstr.t



(*  ____            _ _          _   _              *)
(* |  _ \ ___  __ _| (_)______ _| |_(_) ___  _ __   *)
(* | |_) / _ \/ _` | | |_  / _` | __| |/ _ \| '_ \  *)
(* |  _ <  __/ (_| | | |/ / (_| | |_| | (_) | | | | *)
(* |_| \_\___|\__,_|_|_/___\__,_|\__|_|\___/|_| |_| *)
(*                                                  *)
(* Realization *)

(* Takes the description of a path and gives the associated term *)
val realize : Data.pathSkeleton -> Data.morphismData Proofview.tactic
val realizePath : Data.path -> Data.morphismData Proofview.tactic
(* Takes the description of an element and give the associated term *)
val realizeElem : Data.elem -> EConstr.t Proofview.tactic
(* Take the description of an equality and give the associated term *)
val realizeEq : Data.eq -> t Proofview.tactic

(*   ____      _                        _ *)
(*  / ___|__ _| |_ ___  __ _  ___  _ __(_) ___  ___ *)
(* | |   / _` | __/ _ \/ _` |/ _ \| '__| |/ _ \/ __| *)
(* | |__| (_| | ||  __/ (_| | (_) | |  | |  __/\__ \ *)
(*  \____\__,_|\__\___|\__, |\___/|_|  |_|\___||___/ *)
(*                     |___/ *)
(* Categories *)

val is_cat : Environ.env -> t -> bool Proofview.tactic
(* Gives the src and dst categories of the functor *)
val is_funct : Environ.env -> t -> (t * t) option Proofview.tactic
(* Apply functor to object *)
val funct_obj : Environ.env -> Data.funct -> t -> t Proofview.tactic
(* Apply functor to morphism *)
val funct_mph : Environ.env -> Data.funct -> Data.morphismData -> t Proofview.tactic
(* Gives the category of the object *)
val is_object : Environ.env -> t -> t option Proofview.tactic
(* cat -> src -> dst -> tp *)
val morphism : Environ.env -> t -> t -> t -> t Proofview.tactic
(* mphT -> cat*src*dst *)
val is_morphism : Environ.env -> t -> (t*t*t) option Proofview.tactic
(* cat -> src -> mid -> dst -> mph cat src mid -> mph cat mid dst -> mph cat src dst *)
val compose : Environ.env -> t -> t -> t -> t -> t -> t -> t Proofview.tactic
val composeM : Data.morphismData -> Data.morphismData -> Data.morphismData Proofview.tactic
val composeT : Data.morphismT -> Data.morphismT -> Data.morphismT Proofview.tactic
(* mph -> cat*src*mid*dst*m1*m2 *)
val parse_compose : Environ.env -> t -> (t*t*t*t*t*t) option Proofview.tactic
(* cat -> x -> 1_x *)
val identity : Environ.env -> t -> t -> t Proofview.tactic
val identityM : Data.elem -> Data.morphismData Proofview.tactic
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
