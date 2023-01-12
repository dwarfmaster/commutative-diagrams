
module type ProofAssistant = sig
  type t

  (* Monad *)
  module M : Hyps.Monad
  val lift_tactic : 'a Proofview.tactic -> 'a M.m

  (* Realisation *)
  val realizeCategory : t Data.category -> t M.m 
  val realizeFunctor : t Data.funct -> t M.m
  val realizeElem : t Data.elem -> t M.m 
  val realizeMorphism : t Data.morphism -> t M.m
  val realizeEq : t Data.eq -> t M.m

  (* Parsing *)
  (* First argument is the term, second is the type *)
  val parseCategory : t -> t -> (t Data.category option,t) Hyps.Make(M).t
  val parseFunctor : t -> t -> (t Data.funct option,t) Hyps.Make(M).t
  val parseElem : t -> t -> (t Data.elem option,t) Hyps.Make(M).t
  val parseMorphism : t -> t -> (t Data.morphism option,t) Hyps.Make(M).t
  val parseEq : t -> t -> (t Data.eq option,t) Hyps.Make(M).t
  val parseEqGoal : t -> ((t Data.morphism * t Data.morphism) option, t) Hyps.Make(M).t

  (* Properties on objects *)
  (* Expected to parse monomorphisms, epimorphisms and isomorphisms *)
  val parseProperties : t -> t -> (unit, t) Hyps.Make(M).t

  (* Utils *)
  val eq : t -> t -> bool M.m
  val print : t -> string M.m
  val fail : string -> unit M.m
  val message : string -> unit M.m
  val warning : string -> unit M.m
  val env : unit -> Environ.env M.m
  val to_econstr : t -> EConstr.t
  val from_econstr : EConstr.t -> t
end
