
type state
type 'a m
val run : Proofview.Goal.t -> 'a m -> 'a Proofview.tactic

(* Basic monadic operations *)
val bind : 'a m -> ('a -> 'b m) -> 'b m
val thn  : 'a m -> 'b m -> 'b m
val ret  : 'a -> 'a m
val (let*) : 'a m -> ('a -> 'b m) -> 'b m
val (>>=)  : 'a m -> ('a -> 'b m) -> 'b m
val (@<<)  : ('a -> 'b m) -> 'a m -> 'b m
val (<$>)  : ('a -> 'b) -> 'a m -> 'b m

(* Proof operations *)
val liftTactic : 'a Proofview.tactic -> 'a m
val getEnv : Environ.env m
val getEvarMap : Evd.evar_map m

(* State operations *)
val getCategories : Data.category array m
val getCategory : int -> Data.category m
val addCategory : Data.category -> unit m
val initCategory : (* cat *)EConstr.t -> Data.cat_id m

val getElems : Data.elem array m 
val getElem : int -> Data.elem m
val addElem : Data.elem -> unit m
val initElem : (* cat *)Data.category -> (* elem *)EConstr.t -> Data.elem_id m

val getMorphisms : Data.morphismBase array m 
val getMorphism : int -> Data.morphismBase m
val addMorphism : Data.morphismBase -> unit m
val initMorphism : Data.morphismData -> Data.mph_id m

val getFaces : Data.face array m
val getFace : int -> Data.face m
val addFace : Data.face -> unit m
val initFace : (* tp *)Data.morphismT -> (* mph1 *)Data.path -> (* mph2 *)Data.path
            -> (* eq *)Data.eq -> Data.face_id m
