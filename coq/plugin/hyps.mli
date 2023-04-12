
(* The base type t is a state monad that memorize all atomic elements of the
   context. It also offers a reader monad over the goal environment.
*)

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
end


(* State operations *)
val getAtom : int -> Data.atomic option t
val withMask : bool -> 'a t -> 'a t
val withEnv : Environ.env -> 'a t -> 'a t

val catToIndex : int -> int option
val catFromIndex : int -> int
val getCategories : unit -> Data.categoryData array t 
val getCategory : int -> Data.categoryData t
val registerCategory : cat:EConstr.t
                    -> Data.categoryData t

val functorToIndex : int -> int option
val functorFromIndex : int -> int
val getFunctors : unit -> Data.functData array t 
val getFunctor : int -> Data.functData t
val registerFunctor : funct:EConstr.t
                   -> src:Data.category
                   -> dst:Data.category
                   -> Data.functData t

val elemToIndex : int -> int option
val elemFromIndex : int -> int
val getElems : unit -> (Data.elemData*bool) array t 
val getElem : int -> Data.elemData t 
val getElemMask : int -> bool t
val registerElem : elem:EConstr.t
                -> cat:Data.category
                -> Data.elemData t

val mphToIndex : int -> int option
val mphFromIndex : int -> int
val getMorphisms : unit -> (Data.morphismData*bool) array t
val getMorphism : int -> Data.morphismData t
val getMorphismMask : int -> bool t
val registerMorphism : mph:EConstr.t
                    -> cat:Data.category 
                    -> src:Data.elem
                    -> dst:Data.elem 
                    -> Data.morphismData t

val eqToIndex : int -> int option
val eqFromIndex : int -> int
val getEqs : unit -> (Data.eqData*bool) array t 
val getEq : int -> Data.eqData t 
val getEqMask : int -> bool t
val registerEq : eq:EConstr.t
              -> right:Data.morphism
              -> left:Data.morphism
              -> cat:Data.category
              -> src:Data.elem
              -> dst:Data.elem
              -> Data.eqData t

(* Functions are arbitrary Coq symbols that appear in function position of
   atomic Composed *)
val funToIndex : int -> int option
val funFromIndex : int -> int
val getFuns : unit -> Data.fn array t
val getFun : int -> Data.fn t
val registerFun : fn:Data.fn -> int t

(* Evars are an array of either uninstantiate evars (meaning not yet realized
   as evars in Coq, just abstract unique numbers), or linked to a coq Evar.t.
   Since build the econstr from the evar is too much a pain, we directly store
   the EConstr wrapping the EVar. *)
type evar =
  | Abstract
  | Realized of EConstr.t
  | NotFound
val getEvar : int -> evar t
(* Create a new abstract evar *)
val newEvar : unit -> int t
(* Creates a new evar with specific id, assuming it doesn't already exists *)
val newEvarAt : int -> unit t
(* Associate a coq evar to an abstract evar. It will fail if the evar was already
   instantiated *)
val instantiateEvar : int -> EConstr.t -> unit t

