
module type ProofAssistant = sig
  type t

  (* Monad *)
  module M : Store.Monad

  (* Realisation *)
  val realizeCategory : t Data.category -> t M.m 
  val realizeFunctor : t Data.funct -> t M.m
  val realizeElem : t Data.elem -> t M.m 
  val realizeMorphism : t Data.morphism -> t M.m
  val realizeEq : t Data.eq -> t M.m

  (* Parsing *)
  (* First argument is the term, second is the type *)
  val parseCategory : t -> t -> (t Data.category option,t) Store.Make(M).t
  val parseFunctor : t -> t -> (t Data.funct option,t) Store.Make(M).t
  val parseElem : t -> t -> (t Data.elem option,t) Store.Make(M).t
  val parseMorphism : t -> t -> (t Data.morphism option,t) Store.Make(M).t
  val parseEq : t -> t -> (t Data.eq option,t) Store.Make(M).t
  val parseEqGoal : t -> ((t Data.morphism * t Data.morphism) option, t) Store.Make(M).t

  (* Properties on objects *)
  (* Expected to parse monomorphisms, epimorphisms and isomorphisms *)
  val parseProperties : t -> t -> (unit, t) Store.Make(M).t

  (* Utils *)
  val eq : t -> t -> bool M.m
end
