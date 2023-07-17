
(* The base type t is a state monad that memorize all atomic elements of the
   context. It also offers a reader monad over the goal environment.
*)

type obj =
  { namespace: int
  ; id: int
  }
val compare_obj : obj -> obj -> int
type metadata =
  { is_cat: unit option
  ; is_funct: (obj * obj) option
  ; is_elem: obj option
  ; is_mph: (obj * obj * obj) option
  ; is_eq: (obj * obj * obj * obj * obj) option
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

val withMask : bool -> 'a t -> 'a t
val withEnv : Environ.env -> 'a t -> 'a t
val saveState : unit -> int t
val restoreState : int -> unit t
val setState : Evd.evar_map -> unit t
val mapState : (Evd.evar_map -> 'a * Evd.evar_map) -> 'a t
(* Mark an evar as being handled by the plugin *)
val handleEvar : EConstr.t -> unit t
val handled : unit -> Evar.t list t
(* Execute a monadic instruction, then returns the evar_map at the end and restore
   the stateful evar_map to the one before the execution *)
val evarsExcursion : 'a t -> ('a * Evd.evar_map) t
(* In hidden state, registered objects are not de-duplicated, will not be used when
   de-duplicating non-masked objects. A masked object remain masked for its entire
   lifetime. This is used exclusively for the pattern protocol *)
val enterHiddenState : unit -> unit t
val leaveHiddenState : unit -> unit t
val hiddenState : unit -> bool t

(* The 0th namespace is pre-registered and always considered valid. It is also
   considered as a super-namespace for all other. When registering an object in a
   namespace, it may add it to the 0th one if it was present in it. Same as with
   hasObject, which also search the Oth one. *)
val registerNamespace : unit -> int t
val registerObj : (* namespace *)int -> EConstr.t -> EConstr.t -> string option -> obj t
val hasObject : int -> EConstr.t -> obj option t
val getObjValue : obj -> EConstr.t t
val getObjType : obj -> EConstr.t t
val getObjName : obj -> string option t
val getObjMask : obj -> bool t
val getObjHidden : obj -> bool t
val getObjMtdt : obj -> metadata t
val getObjEvars : obj -> Evd.evar_map t
val setEvars : obj -> Evd.evar_map -> unit t
val markAsCat : obj -> unit -> unit t
val markAsFunct : obj -> obj * obj -> unit t
val markAsElem : obj -> obj -> unit t
val markAsMph : obj -> obj * obj * obj -> unit t
val markAsEq : obj -> obj * obj * obj * obj * obj -> unit t

