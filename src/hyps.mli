
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
type morphism =
  { obj : EConstr.t
  ; tp  : morphismT
  ; id  : mph_id
  }

type eq =
  { src : morphism
  ; dst : morphism
  ; tp  : morphismT
  ; eq  : EConstr.t
  }

(* The composed morphism of the path may not be in the context since we only keep the base *)
type path =
  { mph  : morphism
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

(*   ____            _            _    *)
(*  / ___|___  _ __ | |_ _____  _| |_  *)
(* | |   / _ \| '_ \| __/ _ \ \/ / __| *)
(* | |__| (_) | | | | ||  __/>  <| |_  *)
(*  \____\___/|_| |_|\__\___/_/\_\\__| *)

val empty_context : t
val get_cat  : EConstr.t -> t -> t * cat_id
val get_elem : EConstr.t -> t -> t * elem_id
val get_mph  : EConstr.t -> t -> t * mph_id
val get_face : EConstr.t -> t -> t * face_id

(*  __  __                  _     _ *)
(* |  \/  | ___  _ __ _ __ | |__ (_)___ _ __ ___  ___ *)
(* | |\/| |/ _ \| '__| '_ \| '_ \| / __| '_ ` _ \/ __| *)
(* | |  | | (_) | |  | |_) | | | | \__ \ | | | | \__ \ *)
(* |_|  |_|\___/|_|  | .__/|_| |_|_|___/_| |_| |_|___/ *)
(*                   |_| *)
(* m1 -> m2 -> m2 o m1 *)
val compose : Evd.evar_map -> Environ.env -> morphism -> morphism -> morphism
val composeT : Evd.evar_map -> Environ.env -> morphismT -> morphismT -> morphismT
val mphT : Evd.evar_map -> Environ.env -> category -> EConstr.t -> EConstr.t -> morphismT
(* [ m1, m2, m3 ] -> (m3 o m2) o m1 *)
(* Raises Ill_typed if the list is empty of not composable *)
val realize : Evd.evar_map -> Environ.env -> morphism list -> morphism
(* a -> 1_a *)
val identity : Evd.evar_map -> Environ.env -> elem -> morphism


(*  _____                  _ _ _          *)
(* | ____|__ _ _   _  __ _| (_) |_ _   _  *)
(* |  _| / _` | | | |/ _` | | | __| | | | *)
(* | |__| (_| | |_| | (_| | | | |_| |_| | *)
(* |_____\__, |\__,_|\__,_|_|_|\__|\__, | *)
(*          |_|                    |___/  *)
(* a -> a =_A a *)
val refl : Evd.evar_map -> Environ.env -> morphism -> eq
(* a = b -> b = c -> a = c *)
val concat : Evd.evar_map -> Environ.env -> eq -> eq -> eq
(* a = b -> b = a *)
val inv : Evd.evar_map -> Environ.env -> eq -> eq
(* m1 = m2 -> m1' = m2' -> m1 o m1' = m2 o m2' *)
val composeP : Evd.evar_map -> Environ.env -> eq -> eq -> eq
(* m1 -> m2 -> m3 -> m3 o (m2 o m1) = (m3 o m2) o m1 *)
val assoc : Evd.evar_map -> Environ.env -> morphism -> morphism -> morphism -> eq
(* m -> id o m = m *)
val left_id : Evd.evar_map -> Environ.env -> morphism -> eq
(* m -> m o id = m *)
val right_id : Evd.evar_map -> Environ.env -> morphism -> eq
(* Split morphism along compositions, remove identities, and give equality *)
val normalize : Evd.evar_map -> Environ.env -> morphism -> morphism list * eq


(*  ____                _              *)
(* |  _ \ __ _ _ __ ___(_)_ __   __ _  *)
(* | |_) / _` | '__/ __| | '_ \ / _` | *)
(* |  __/ (_| | |  \__ \ | | | | (_| | *)
(* |_|   \__,_|_|  |___/_|_| |_|\__, | *)
(*                              |___/  *)
val parse_cat  : Evd.evar_map -> Environ.env -> kind -> t -> t * cat_id  option
val parse_elem : Evd.evar_map -> Environ.env -> kind -> t -> t * elem_id option
val parse_mph  : Evd.evar_map -> Environ.env -> kind -> t -> t * mph_id  option
val parse_face : Evd.evar_map -> Environ.env -> kind -> t -> t * face_id option
