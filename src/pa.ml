
module type ProofAssistant = sig
  type t

  (* Monad *)
  type 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val ret  : 'a -> 'a m

  (* Realisation *)
  val realizeCategory : t Data.category -> t m 
  val realizeFunctor : t Data.category -> t m
  val realizeElem : t Data.elem -> t m 
  val realizeMorphism : t Data.morphism -> t m
  val realizeEq : t Data.eq -> t m

  (* Parsing *)
  val parseCategory : t -> (t Data.category option,t) Store.t m
  val parseFunctor : t -> (t Data.funct option,t) Store.t m
  val parseElem : t -> (t Data.elem option,t) Store.t m
  val parseMorphism : t -> (t Data.morphism option,t) Store.t m
  val parseEq : t -> (t Data.morphism option,t) Store.t m

  (* Utils *)
  val eq : t -> t -> bool
end
