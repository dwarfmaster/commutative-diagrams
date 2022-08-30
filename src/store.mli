
(* The base type t is a state monad that memorize all atomic elements of the
   context.
*)

type 'a t

(* Monadic operations *)
val ret    : 'a -> 'a t
val bind   : 'a t -> ('a -> 'b t) -> 'b t
val (let*) : 'a t -> ('a -> 'b t) -> 'b t
val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
val (@<<)  : ('a -> 'b t) -> 'a t -> 'b t
val (<$>)  : ('a -> 'b) -> 'a t -> 'b t
val run    : 'a t -> 'a


(* Support *)
(* Register an equality predicate for Data.PA.t *)
val registerEqPredicate : (Data.PA.t -> Data.PA.t -> bool) -> unit t


(* State operations *)
val getCategories : Data.categoryData array t 
val getCategory : int -> Data.categoryData t
val registerCategory : Data.PA.t 
                    -> Data.categoryData t

val getFunctors : Data.functData array t 
val getFunctor : int -> Data.functData t
val registerFunctor : funct:Data.PA.t
                   -> src:Data.category
                   -> dst:Data.category
                   -> Data.functData t

val getElems : Data.elemData array t 
val getElem : int -> Data.elemData t 
val registerElem : elem:Data.PA.t 
                -> cat:Data.category
                -> Data.elemData t

val getMorphisms : Data.morphismData array t
val getMorphism : int -> Data.morphismData t
val registerMorphism : mph:Data.PA.t 
                    -> cat:Data.category 
                    -> src:Data.elem
                    -> dst:Data.elem 
                    -> Data.morphismData t

val getEqs : Data.eqData array t 
val getEq : int -> Data.eqData t 
val registerEq : eq:Data.PA.t 
              -> right:Data.morphism
              -> left:Data.morphism
              -> cat:Data.category
              -> src:Data.elem
              -> dst:Data.elem
              -> Data.eqData t
