
(* Realization *)
val realizeEvar : EConstr.t -> EConstr.t Hyps.t
val realizeCategory : Data.category -> EConstr.t Hyps.t
val realizeFunctor : Data.funct -> EConstr.t Hyps.t
val realizeElem : Data.elem -> EConstr.t Hyps.t
val realizeMorphism : Data.morphism -> EConstr.t Hyps.t
val realizeEq : Data.eq -> EConstr.t Hyps.t

(* Realization of types *)
val realizeCatType : Data.categoryData -> EConstr.t Hyps.t
val realizeFunctType : Data.functData -> EConstr.t Hyps.t
val realizeElemType : Data.elemData -> EConstr.t Hyps.t
val realizeMphType : Data.morphismData -> EConstr.t Hyps.t
val realizeEqType : Data.eqData -> EConstr.t Hyps.t

(* Parsing *)
type parsedType =
  | CategoryT
  | FunctorT of Data.category * Data.category
  | ElemT of Data.category
  | MorphismT of Data.category * Data.elem * Data.elem
  | EqT of Data.category * Data.elem * Data.elem * Data.morphism * Data.morphism
type parsed =
  | Category of Data.category
  | Functor of Data.funct
  | Elem of Data.elem
  | Morphism of Data.morphism
  | Equality of Data.eq
type lemma =
  | Prod of Names.Name.t * parsed * lemma
  | Exists of Names.Name.t * parsed * lemma
  | Result of parsed
(* Even when it returns none, it may have changed the context (for example if it
   parsed a property *)
val parse : EConstr.t -> EConstr.t -> parsed option Hyps.t
val parseType : EConstr.t -> parsedType option Hyps.t
val parseLemma : Data.fn -> EConstr.t -> lemma option Hyps.t
