
type kind = (EConstr.t,EConstr.t,EConstr.ESorts.t,EConstr.EInstance.t) Constr.kind_of_term

type cat_id  = int
type elem_id = int
type mph_id  = int
type face_id = int

type category =
  { obj : EConstr.t
  ; id  : cat_id
  }
type elem =
  { obj      : EConstr.t
  ; category : category
  ; id       : elem_id
  }
type morphismT =
  { category : category
  ; src      : elem
  ; dst      : elem
  ; obj      : EConstr.t
  }
type morphismData =
  { obj : EConstr.t
  ; tp  : morphismT
  }
type morphism =
  { data : morphismData
  ; id   : mph_id
  ; mutable mono : EConstr.t option
  }

(* Equality between uninterned morphisms *)
type eqT
type eq =
  { src : morphismData
  ; dst : morphismData
  ; tp  : morphismT
  ; eq  : eqT
  }

(* The composed morphism of the path may not be in the context since we only keep the base *)
type path =
  { mph  : morphismData
  ; eq   : eq (* Equality from `mph` to `realize path` *)
  ; path : morphism list
  }
type face =
  { tp    : morphismT
  ; side1 : path
  ; side2 : path
  ; obj   : eq (* Equality between side1.mph and side2.mph *)
  ; id    : face_id
  }
type t =
  { categories : category array
  ; elems      : elem array
  ; morphisms  : morphism array
  ; faces      : face array
  }

exception Ill_typed
val extract : morphism list -> morphismData list

(*  __  __                  _     _ *)
(* |  \/  | ___  _ __ _ __ | |__ (_)___ _ __ ___  ___ *)
(* | |\/| |/ _ \| '__| '_ \| '_ \| / __| '_ ` _ \/ __| *)
(* | |  | | (_) | |  | |_) | | | | \__ \ | | | | \__ \ *)
(* |_|  |_|\___/|_|  | .__/|_| |_|_|___/_| |_| |_|___/ *)
(*                   |_| *)
(* m1 -> m2 -> m2 o m1 *)
val compose : morphismData -> morphismData -> morphismData Proofview.tactic
val composeT : morphismT -> morphismT -> morphismT Proofview.tactic
val mphT : EConstr.t -> EConstr.t -> EConstr.t -> EConstr.t Proofview.tactic
(* [ m1, m2, m3 ] -> (m3 o m2) o m1 *)
(* Raises Ill_typed if the list is not composable *)
val realize : elem -> morphismData list -> morphismData Proofview.tactic
val rpath : path -> morphismData Proofview.tactic
(* a -> 1_a *)
val identity : elem -> morphismData Proofview.tactic


(*  _____                  _ _ _          *)
(* | ____|__ _ _   _  __ _| (_) |_ _   _  *)
(* |  _| / _` | | | |/ _` | | | __| | | | *)
(* | |__| (_| | |_| | (_| | | | |_| |_| | *)
(* |_____\__, |\__,_|\__,_|_|_|\__|\__, | *)
(*          |_|                    |___/  *)
val eqT : morphismData -> morphismData -> EConstr.t Proofview.tactic
(* a -> a =_A a *)
val refl : morphismData -> eq Proofview.tactic
(* a = b -> b = c -> a = c *)
val concat : eq -> eq -> eq Proofview.tactic
(* a = b -> b = a *)
val inv : eq -> eq Proofview.tactic
(* m1 = m2 -> m1' = m2' -> m1 o m1' = m2 o m2' *)
val composeP : eq -> eq -> eq Proofview.tactic
(* m1 -> m2 -> m3 -> m3 o (m2 o m1) = (m3 o m2) o m1 *)
val assoc : morphismData -> morphismData -> morphismData -> eq Proofview.tactic
(* m -> id o m = m *)
val left_id : morphismData -> eq Proofview.tactic
(* m -> m o id = m *)
val right_id : morphismData -> eq Proofview.tactic
val atom_eq : EConstr.t -> eqT
val mono_eq : EConstr.t -> morphismData -> morphismData -> eq -> eqT
val real_eq : eq -> EConstr.t Proofview.tactic

(*   ____            _            _    *)
(*  / ___|___  _ __ | |_ _____  _| |_  *)
(* | |   / _ \| '_ \| __/ _ \ \/ / __| *)
(* | |__| (_) | | | | ||  __/>  <| |_  *)
(*  \____\___/|_| |_|\__\___/_/\_\\__| *)

val empty_context : t
val get_cat  : EConstr.t -> t -> (cat_id * t) Proofview.tactic
val get_elem : EConstr.t -> EConstr.t -> t -> (elem_id * t) Proofview.tactic
val get_mph  : morphismData -> t -> (mph_id * t) Proofview.tactic
(* TODO remove *)
val repeat_assoc : morphismData list -> morphismData -> eq Proofview.tactic
(* Split morphism along compositions, remove identities, and give equality *)
val normalize : morphismData -> t -> (morphism list * eq * t) Proofview.tactic
val get_face : morphismT -> EConstr.t -> EConstr.t -> EConstr.t -> t -> (face_id * t) Proofview.tactic

(*  ____                _              *)
(* |  _ \ __ _ _ __ ___(_)_ __   __ _  *)
(* | |_) / _` | '__/ __| | '_ \ / _` | *)
(* |  __/ (_| | |  \__ \ | | | | (_| | *)
(* |_|   \__,_|_|  |___/_|_| |_|\__, | *)
(*                              |___/  *)
val parse_cat  : Names.Id.t -> EConstr.t -> t -> (t * cat_id  option) Proofview.tactic
val parse_elem : Names.Id.t -> EConstr.t -> t -> (t * elem_id option) Proofview.tactic
val parse_mph  : Names.Id.t -> EConstr.t -> t -> (t * mph_id  option) Proofview.tactic
val read_face  : EConstr.t -> t -> (t * (path*path) option) Proofview.tactic
val parse_face : Names.Id.t -> EConstr.t -> t -> (t * face_id option) Proofview.tactic
val parse_mono : Names.Id.t -> EConstr.t -> t -> (t * mph_id option) Proofview.tactic
