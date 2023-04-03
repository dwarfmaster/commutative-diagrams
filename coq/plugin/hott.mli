
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
val parseCategory : EConstr.t -> EConstr.t -> Data.category option Hyps.t
val parseFunctor : EConstr.t -> EConstr.t -> Data.funct option Hyps.t
val parseElem : EConstr.t -> EConstr.t -> Data.elem option Hyps.t
val parseMorphism : EConstr.t -> EConstr.t -> Data.morphism option Hyps.t
val parseEq : EConstr.t -> EConstr.t -> Data.eq option Hyps.t
val parseEqGoal : EConstr.t -> (Data.morphism * Data.morphism) option Hyps.t
val parseProperties : EConstr.t -> EConstr.t -> unit Hyps.t
