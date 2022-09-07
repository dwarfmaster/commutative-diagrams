
type t = EConstr.t
module M : sig
  type 'a m = 'a Proofview.tactic
  val bind : 'a Proofview.tactic -> ('a -> 'b Proofview.tactic) -> 'b Proofview.tactic
  val return : 'a -> 'a Proofview.tactic
end

(* Realization *)
val realizeCategory : t Data.category -> t Proofview.tactic
val realizeFunctor : t Data.funct -> t Proofview.tactic
val realizeElem : t Data.elem -> t Proofview.tactic
val realizeMorphism : t Data.morphism -> t Proofview.tactic
val realizeEq : t Data.eq -> t Proofview.tactic

(* Parsing *)
module St := Store.Make(M)
val parseCategory : t -> t -> (t Data.category option,t) St.t
val parseFunctor : t -> t -> (t Data.funct option,t) St.t
val parseElem : t -> t -> (t Data.elem option,t) St.t
val parseMorphism : t -> t -> (t Data.morphism option,t) St.t
val parseEq : t -> t -> (t Data.eq option,t) St.t

(* Utils *)
val eq : (t -> t -> bool) Proofview.tactic
