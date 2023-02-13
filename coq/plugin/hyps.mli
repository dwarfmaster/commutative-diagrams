
(* The base type t is a state monad that memorize all atomic elements of the
   context. It is expressed over an arbitrary monad
*)

module type Monad = sig
  type 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m 
end

module Make(M : Monad) : sig
  type ('a,'t) t
  
  (* Monadic operations *)
  module Combinators : sig
    val ret    : 'a -> ('a,'t) t
    val bind   : ('a,'t) t -> ('a -> ('b,'t) t) -> ('b,'t) t
    val (let*) : ('a,'t) t -> ('a -> ('b,'t) t) -> ('b,'t) t
    val (>>=)  : ('a,'t) t -> ('a -> ('b,'t) t) -> ('b,'t) t
    val (@<<)  : ('a -> ('b,'t) t) -> ('a,'t) t -> ('b,'t) t
    val (<$>)  : ('a -> 'b) -> ('a,'t) t -> ('b,'t) t
    val run    : ('a,'t) t -> 'a M.m
    val lift   : 'a M.m -> ('a,'t) t
  end
  
  
  (* Support *)
  (* Register an equality predicate for Data.Data.constr *)
  val registerEqPredicate : ('t -> 't -> bool M.m) -> (unit,'t) t
  
  
  (* State operations *)
  val getCategories : unit -> ('t Data.categoryData array,'t) t 
  val getCategory : int -> ('t Data.categoryData,'t) t
  val registerCategory : cat:('t Data.atomic)
                      -> ('t Data.categoryData,'t) t
  
  val getFunctors : unit -> ('t Data.functData array,'t) t 
  val getFunctor : int -> ('t Data.functData,'t) t
  val registerFunctor : funct:('t Data.atomic)
                     -> src:'t Data.category
                     -> dst:'t Data.category
                     -> ('t Data.functData,'t) t
  
  val getElems : unit -> ('t Data.elemData array,'t) t 
  val getElem : int -> ('t Data.elemData,'t) t 
  val registerElem : elem:('t Data.atomic)
                  -> cat:'t Data.category
                  -> ('t Data.elemData,'t) t
  
  val getMorphisms : unit -> ('t Data.morphismData array,'t) t
  val getMorphism : int -> ('t Data.morphismData,'t) t
  val registerMorphism : mph:('t Data.atomic)
                      -> cat:'t Data.category 
                      -> src:'t Data.elem
                      -> dst:'t Data.elem 
                      -> ('t Data.morphismData,'t) t
  
  val getEqs : unit -> ('t Data.eqData array,'t) t 
  val getEq : int -> ('t Data.eqData,'t) t 
  val registerEq : eq:('t Data.atomic)
                -> right:'t Data.morphism
                -> left:'t Data.morphism
                -> cat:'t Data.category
                -> src:'t Data.elem
                -> dst:'t Data.elem
                -> ('t Data.eqData,'t) t

  (* Evars are an array of either uninstantiate evars (meaning not yet realized
     as evars in Coq, just abstract unique numbers), or linked to a coq Evar.t *)
  type evar =
    | Abstract
    | Realized of Evar.t
    | NotFound
  val getEvar : int -> (evar, 't) t
  (* Create a new abstract evar *)
  val newEvar : unit -> (int, 't) t
  (* Associate a coq evar to an abstract evar. It will fail if the evar was already
     present *)
  val instantiateEvar : int -> Evar.t -> (unit, 't) t
end

