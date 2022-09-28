
type t = EConstr.t
module M : sig
  type 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m

  val env : unit -> Environ.env m
  val lift : 'a Proofview.tactic -> 'a m
  val run : Environ.env -> 'a m -> 'a Proofview.tactic
end

(* Realization *)
val realizeCategory : t Data.category -> t M.m
val realizeFunctor : t Data.funct -> t M.m
val realizeElem : t Data.elem -> t M.m
val realizeMorphism : t Data.morphism -> t M.m
val realizeEq : t Data.eq -> t M.m

(* Parsing *)
module St := Store.Make(M)
val parseCategory : t -> t -> (t Data.category option,t) St.t
val parseFunctor : t -> t -> (t Data.funct option,t) St.t
val parseElem : t -> t -> (t Data.elem option,t) St.t
val parseMorphism : t -> t -> (t Data.morphism option,t) St.t
val parseEq : t -> t -> (t Data.eq option,t) St.t
val parseEqGoal : t -> ((t Data.morphism * t Data.morphism) option, t) Store.Make(M).t


(* Utils *)
val eq : t -> t -> bool M.m
