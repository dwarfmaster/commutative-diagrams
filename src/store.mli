
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


(* State operations *)
val getCategories : Data.category array t 
val getCategory : int -> Data.category t
val registerCategory : Data.PA.t 
                    -> int t

val getFunctors : Data.funct array t 
val getFunctor : int -> Data.funct t
val registerFunctor : funct:Data.PA.t
                   -> src:Data.PA.t
                   -> dst:Data.PA.t
                   -> int t

val getElems : Data.elem array t 
val getElem : int -> Data.elem t 
val registerElem : elem:Data.PA.t 
                -> cat:Data.PA.t 
                -> int t

val getMorphisms : Data.morphism array t
val getMorphism : int -> Data.morphism t
val registerMorphism : mph:Data.PA.t 
                    -> cat:Data.PA.t 
                    -> src:Data.PA.t 
                    -> dst:Data.PA.t 
                    -> int t

val getEqs : Data.eq array t 
val getEq : int -> Data.eq t 
val registerEq : eq:Data.PA.t 
              -> right:Data.PA.t 
              -> left:Data.PA.t 
              -> cat:Data.PA.t
              -> src:Data.PA.t 
              -> dst:Data.PA.t
              -> int t
