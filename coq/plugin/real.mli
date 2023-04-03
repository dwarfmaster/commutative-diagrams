
(* Wrappers over PA.realize* that instantiate all evars before doing the realization *)
val realizeCategory : Data.category -> EConstr.t Hyps.t
val realizeFunctor : Data.funct -> EConstr.t Hyps.t
val realizeElem : Data.elem -> EConstr.t Hyps.t
val realizeMorphism : Data.morphism -> EConstr.t Hyps.t
val realizeEq : Data.eq -> EConstr.t Hyps.t
