

open Data
type t =
  { categories : category array
  ; elems      : elem array
  ; morphisms  : morphismBase array
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
val right_inv : isoData -> eq Proofview.tactic
val left_inv  : isoData -> eq Proofview.tactic
val atom_eq : EConstr.t -> eqT
val mono_eq : EConstr.t -> morphismData -> morphismData -> eq -> eqT
val epi_eq : EConstr.t -> morphismData -> morphismData -> eq -> eqT
val simpl_eq : eq -> eq

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
(* Split morphism along compositions, remove identities and inversions, and give equality *)
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
val parse_epi  : Names.Id.t -> EConstr.t -> t -> (t * mph_id option) Proofview.tactic
val parse_iso  : Names.Id.t -> EConstr.t -> t -> (t * mph_id option) Proofview.tactic
