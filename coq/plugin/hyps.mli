
(* The base type t is a state monad that memorize all atomic elements of the
   context. It also offers a reader monad over the goal environment.
*)

type obj = int
type metadata =
  { is_cat: unit option
  ; is_funct: (int * int) option
  ; is_elem: int option
  ; is_mph: (int * int * int) option
  ; is_eq: (int * int * int * int * int) option
  }
type 'a t

(* Monadic operations *)
module Combinators : sig
  val ret     : 'a -> 'a t
  val bind    : 'a t -> ('a -> 'b t) -> 'b t
  val (let*)  : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=)   : 'a t -> ('a -> 'b t) -> 'b t
  val (@<<)   : ('a -> 'b t) -> 'a t -> 'b t
  val (<$>)   : ('a -> 'b) -> 'a t -> 'b t
  val run     : Environ.env -> 'a t -> 'a Proofview.tactic
  val lift    : 'a Proofview.tactic -> 'a t
  val env     : unit -> Environ.env t
  val evars   : unit -> Evd.evar_map t
  val none    : unit -> 'a option t
  val some    : 'a -> 'a option t
  val print   : EConstr.t -> string t
  val fail    : string -> 'a t
  val message : string -> unit t
  val warning : string -> unit t
  val concat  : 'a t list -> 'a list t
  val mapM    : ('a -> 'b t) -> 'a list -> 'b list t
end

val withEnv : Environ.env -> 'a t -> 'a t
val saveState : unit -> int t
val restoreState : int -> unit t
val setState : Evd.evar_map -> unit t
val mapState : (Evd.evar_map -> 'a * Evd.evar_map) -> 'a t
(* Mark an evar as being handled by the plugin *)
val handleEvar : EConstr.t -> unit t
val handled : unit -> Evar.t list t

(* The 0th namespace is pre-registered and always considered valid. It is also
   considered as a super-namespace for all other. When registering an object in a
   namespace, it may add it to the 0th one if it was present in it. Same as with
   hasObject, which also search the Oth one. *)
val registerNamespace : unit -> int t
(* Execute a tactic in a specific namespace. A namespace stores a different evar map,
   and a different view of objects. However, the state saving and restoring operations
   are namespace independant and only operate on the evar map of the 0th aka global
   namespace. If rollback is false, the global evar_map is not reverted at the end *)
val inNamespace : ?rollback:bool -> int -> 'a t -> 'a t
val registerObj : EConstr.t -> EConstr.t -> string option -> int t
val hasObject : EConstr.t -> int option t
val getObjValue : int -> EConstr.t t
val getObjType : int -> EConstr.t t
val getObjName : int -> string option t
val getObjMtdt : int -> metadata t
val getObjRepr : int -> int t
val markAsCat : int -> unit -> unit t
val markAsFunct : int -> int * int -> unit t
val markAsElem : int -> int -> unit t
val markAsMph : int -> int * int * int -> unit t
val markAsEq : int -> int * int * int * int * int -> unit t

