
(* The base type t is a state monad that memorize all atomic elements of the
   context.
*)

type ('a,'t) t

(* Monadic operations *)
val ret    : 'a -> ('a,'t) t
val bind   : ('a,'t) t -> ('a -> ('b,'t) t) -> ('b,'t) t
val (let*) : ('a,'t) t -> ('a -> ('b,'t) t) -> ('b,'t) t
val (>>=)  : ('a,'t) t -> ('a -> ('b,'t) t) -> ('b,'t) t
val (@<<)  : ('a -> ('b,'t) t) -> ('a,'t) t -> ('b,'t) t
val (<$>)  : ('a -> 'b) -> ('a,'t) t -> ('b,'t) t
val run    : ('a,'t) t -> 'a


(* Support *)
(* Register an equality predicate for Data.Data.constr *)
val registerEqPredicate : ('t -> 't -> bool) -> (unit,'t) t


(* State operations *)
val getCategories : unit -> ('t Data.categoryData array,'t) t 
val getCategory : int -> ('t Data.categoryData,'t) t
val registerCategory : cat:'t
                    -> ('t Data.categoryData,'t) t

val getFunctors : unit -> ('t Data.functData array,'t) t 
val getFunctor : int -> ('t Data.functData,'t) t
val registerFunctor : funct:'t
                   -> src:'t Data.category
                   -> dst:'t Data.category
                   -> ('t Data.functData,'t) t

val getElems : unit -> ('t Data.elemData array,'t) t 
val getElem : int -> ('t Data.elemData,'t) t 
val registerElem : elem:'t 
                -> cat:'t Data.category
                -> ('t Data.elemData,'t) t

val getMorphisms : unit -> ('t Data.morphismData array,'t) t
val getMorphism : int -> ('t Data.morphismData,'t) t
val registerMorphism : mph:'t 
                    -> cat:'t Data.category 
                    -> src:'t Data.elem
                    -> dst:'t Data.elem 
                    -> ('t Data.morphismData,'t) t

val getEqs : unit -> ('t Data.eqData array,'t) t 
val getEq : int -> ('t Data.eqData,'t) t 
val registerEq : eq:'t
              -> right:'t Data.morphism
              -> left:'t Data.morphism
              -> cat:'t Data.category
              -> src:'t Data.elem
              -> dst:'t Data.elem
              -> ('t Data.eqData,'t) t

