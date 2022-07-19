
type state
type 'a t
val run : Proofview.Goal.t -> 'a t -> 'a Proofview.tactic

(* Basic monadic operations *)
val bind : 'a t -> ('a -> 'b t) -> 'b t
val thn  : 'a t -> 'b t -> 'b t
val ret  : 'a -> 'a t
val (let*) : 'a t -> ('a -> 'b t) -> 'b t
val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
val (@<<)  : ('a -> 'b t) -> 'a t -> 'b t
val (<$>)  : ('a -> 'b) -> 'a t -> 'b t

(* Proof operations *)
val liftTactic : 'a Proofview.tactic -> 'a t
val getEnv : Environ.env t
val getEvarMap : Evd.evar_map t

(* State operations *)
val getCategories : Data.category array t
val getCategory : int -> Data.category t
val addCategory : Data.category -> unit t
val initCategory : (* cat *)EConstr.t -> Data.cat_id t

val getElems : Data.elem array t 
val getElem : int -> Data.elem t
val addElem : Data.elem -> unit t
val initElem : (* cat *)EConstr.t -> (* elem *)EConstr.t -> Data.elem_id t

val getMorphisms : Data.morphismBase array t 
val getMorphism : int -> Data.morphismBase t
val addMorphism : Data.morphismBase -> unit t
val initMorphism : Data.morphismData -> Data.mph_id t

val getFaces : Data.face array t
val getFace : int -> Data.face t
val addFace : Data.face -> unit t
val initFace : (* tp *)Data.morphismT -> (* mph1 *)EConstr.t -> (* mph2 *)EConstr.t -> (* eq *)EConstr.t -> Data.face_id t
